# =============================================================================
# FLOW MAGNITUDE & DIRECTION — with lag time optimization
# 
# This code:
#   1. Filters out dead cycles (heater failure) and sensor spikes
#   2. Sweeps lag times from 10s to max cycle length in 10s steps
#   3. At each lag time, computes R² for multiple flow-prediction metrics
#   4. Plots R² vs lag time so you can pick the optimal measurement window
#   5. Extracts final features at the best lag time
#   6. Calibrates flow speed and recovers flow direction using log-ratios
#
# Requires: dat1_15_c with columns: cycle, flow, time_loop,
#           HEATER_heater, temp_A..temp_F
# =============================================================================

# =============================================================================
# FLOW MAGNITUDE & DIRECTION FROM HEAT PULSE DATA  (v2 — with QC filtering)
# 
# Insert after building dat1_15_c (with cycle column, time_loop, and 
# temp_A through temp_F assigned from the RING01_Traw columns).
#
# This code:
#   1. Filters out dead cycles (heater failure) and sensor spikes
#   2. Applies per-cycle baseline correction (subtract start-of-heat)
#   3. Extracts features at t=480s for both magnitude and direction
#   4. Calibrates flow speed from mean temperature response
#   5. Recovers flow direction from opposing sensor pairs
#
# Requires: dat1_15_c with columns: cycle, flow, time_loop,  
#           HEATER_heater, temp_A..temp_F
# =============================================================================
library(dplyr)
library(doBy)
library(ggplot2)
library(lubridate)
library(reshape2)

# =============================================================================
# CORRECTED 3D Flow Sensor Analysis
# 
# Key changes from original:
#   1. Vertical signal: use RING AVERAGES (mean Ring1 - mean Ring3) instead of
#      diagonal pairs (A1-D3, B1-E3, C1-F3). Diagonal pairs conflate horizontal
#      and vertical flow because A and D are on opposite sides of the ring.
#   2. Fixed sin(57.68) radians bug (R expects radians, not degrees).
#   3. Use all 6 same-position upper-lower pairs averaged for vertical signal.
# =============================================================================

average_traw <- function(df, interval = 30) {
  library(tidyverse)
  
  t0 <- min(df$time.s)
  traw_cols <- names(df) %>% str_subset("Traw")
  
  df <- df %>%
    mutate(across(all_of(traw_cols), as.numeric))
  
  df %>%
    mutate(bin = floor((time.s - t0) / interval)) %>%
    group_by(bin) %>%
    summarise(
      time.s_start = min(time.s),
      time.s_end   = max(time.s),
      time.s_mean  = mean(time.s),
      TS_switch    = first(TS_switch),
      flow         = first(flow),
      across(all_of(traw_cols), \(x) mean(x, na.rm = TRUE), .names = "{.col}_mean")
    ) %>%
    dplyr::select(-bin)
}

#function that labels each heating cycle numerically.
add_cycle_column <- function(df, var_name) {
  # Create a lagged version to detect transitions
  df$lag_var <- c(NA, head(df[[var_name]], -1))
  
  # Detect start of new cycles (0 -> 1 transition)
  df$cycle_start <- ifelse(is.na(df$lag_var), 
                          df[[var_name]] == 1,
                          df$lag_var == 0 & df[[var_name]] == 1)
  
  # Create cycle numbers using cumulative sum
  df$cycle <- cumsum(df$cycle_start)
  
  # If the first value is 0, it belongs to cycle 0 (pre-first cycle)
  if (!is.na(df[[var_name]][1]) && df[[var_name]][1] == 0) {
    df$cycle <- df$cycle
  }
  
  # Time since cycle start (seconds)
  df <- df %>%
    group_by(cycle) %>%
    mutate(time_loop = time.s - min(time.s)) %>%
    ungroup()
  
  # Clean up temporary columns
  df$lag_var <- NULL
  df$cycle_start <- NULL
  
  return(df)
}





m82<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/Good-1D-result/20260320_1830_82ml-min/9999883.CSV')

m82$flow<-82
m82_30<-as.data.frame(average_traw(m82, interval = 10))

#m90_30<-m90_30[-c(1:3600),]




m100<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/Good-1D-result/20260320_2345_100ml-min/9998353.CSV')

m100$flow<-100
m100_30<-as.data.frame(average_traw(m100, interval = 10))




m126<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/Good-1D-result/20260321_2037_126ml-min/9967158.CSV')

m126$flow<-126
m126_30<-as.data.frame(average_traw(m126, interval = 10))



m166<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/Good-1D-result/20260322_2052_166ml-min/9966115.CSV')

m166$flow<-166
m166_30<-as.data.frame(average_traw(m166, interval = 10))





mla<-bind_rows(m82_30,m100_30,m126_30,m166_30)

saveRDS(mla,"mla.rds")
#dat1<-read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/Lab-data-from-March2026/20260318_2119_all-velocities/flow_appended_9999899.CSV')

#dat1_15<-average_traw(dat1, interval = 15)



dat1_15<-mla
dat1_15<-subset(dat1_15,!is.na(RING01_TrawD_mean))


dat1_15$HEATER_heater<-as.numeric(dat1_15$TS_switch)
dat1_15$time.s<-dat1_15$time.s_mean
dat1_15_c<-as.data.frame(add_cycle_column(dat1_15,"HEATER_heater"))
dat1_15_c<-subset(dat1_15_c,!is.na(flow))



#dat1_15_c$time.s<-as.numeric(dat1_15_c$time.s)
dat1_15_c$temp_A<-as.numeric(dat1_15_c$RING01_TrawA_mean)
dat1_15_c$temp_B<-as.numeric(dat1_15_c$RING01_TrawB_mean)
dat1_15_c$temp_C<-as.numeric(dat1_15_c$RING01_TrawC_mean)
dat1_15_c$temp_D<-as.numeric(dat1_15_c$RING01_TrawE_mean)
dat1_15_c$temp_E<-as.numeric(dat1_15_c$RING01_TrawD_mean)
dat1_15_c$temp_F<-as.numeric(dat1_15_c$RING01_TrawF_mean)
dat1_15_c$time.s<-as.numeric(dat1_15_c$time.s)



dat1_15_c$time<-as.integer(dat1_15_c$time.s)
dat1_15_c$dtp<-as_datetime(dat1_15_c$time)







library(dplyr)
library(ggplot2)

sensor_cols <- c("temp_A", "temp_B", "temp_C", "temp_D", "temp_E", "temp_F")


# ---- STEP 1: Quality filter ----

MIN_RISE  <- 0.3
MAX_SPIKE <- 5.0

cycle_quality <- data.frame()
for (i in unique(dat1_15_c$cycle)) {
  cyc  <- subset(dat1_15_c, cycle == i)
  if (nrow(cyc) < 20) next
  heat <- subset(cyc, HEATER_heater == 1)
  if (nrow(heat) < 3) next
  
  baseline_vals <- sapply(sensor_cols, function(s)
    mean(heat[[s]][1:min(2, nrow(heat))], na.rm = TRUE))
  rises <- sapply(seq_along(sensor_cols), function(j)
    max(cyc[[sensor_cols[j]]], na.rm = TRUE) - baseline_vals[j])
  
  cycle_quality <- bind_rows(cycle_quality, data.frame(
    cycle = i, flow = unique(cyc$flow)[1],
    mean_rise = mean(rises), max_rise = max(rises)
  ))
}

good_cycles <- cycle_quality$cycle[
  cycle_quality$mean_rise >= MIN_RISE & cycle_quality$max_rise < MAX_SPIKE
]

cat(sprintf("\n=== QUALITY FILTER: %d / %d cycles passed ===\n",
            length(good_cycles), nrow(cycle_quality)))
print(table(cycle_quality$flow[cycle_quality$cycle %in% good_cycles]))


# ---- STEP 2: Pre-compute per-cycle baseline-corrected data ----
# Store in a list so we only do this once

cycle_list <- list()
for (i in good_cycles) {
  cyc  <- subset(dat1_15_c, cycle == i)
  heat <- subset(cyc, HEATER_heater == 1)
  cool <- subset(cyc, HEATER_heater == 0)
  if (nrow(heat) < 3 || nrow(cool) < 3) next
  
  baselines <- sapply(sensor_cols, function(s)
    mean(heat[[s]][1:min(2, nrow(heat))], na.rm = TRUE))
  names(baselines) <- sensor_cols
  
  off_cols <- paste0(sensor_cols, "_off")
  for (j in seq_along(sensor_cols))
    cyc[[off_cols[j]]] <- cyc[[sensor_cols[j]]] - baselines[j]
  
  cycle_list[[as.character(i)]] <- list(
    cyc  = cyc,
    flow = unique(cyc$flow)[1]
  )
}

cat(sprintf("Pre-computed %d cycles\n\n", length(cycle_list)))


# ---- STEP 3: Sweep lag times ----
# At each lag time, extract features and compute R² for flow prediction.
# Five metrics are evaluated:
#   1. mean_t (linear)     — mean of all 6 sensors, linear fit
#   2. mean_t (quadratic)  — mean of all 6 sensors, quadratic fit
#   3. hm_logratio (linear)  — log-ratio horiz_mag, linear fit
#   4. hm_logratio (quadratic) — log-ratio horiz_mag, quadratic fit
#   5. hm_diff (linear)    — difference-based horiz_mag, linear fit

all_cycle_lengths <- sapply(cycle_list, function(cl) max(cl$cyc$time_loop))
cat(sprintf("  Cycle lengths: min=%.0fs, median=%.0fs, max=%.0fs\n",
            min(all_cycle_lengths), median(all_cycle_lengths), max(all_cycle_lengths)))

# Use the median cycle length as the sweep ceiling (not the min).
# Cycles shorter than a given lag time are automatically skipped,
# so short cycles don't affect results — they just reduce n at that lag.
max_time <- median(all_cycle_lengths)
lag_times <- seq(10, floor(max_time / 10) * 10, by = 10)

cat(sprintf("Sweeping %d lag times from %ds to %ds ...\n",
            length(lag_times), min(lag_times), max(lag_times)))
cat(sprintf("  (cycles shorter than a given lag time are skipped for that lag)\n"))

off_cols <- paste0(sensor_cols, "_off")
angs <- c(30, 90, 150) * pi / 180

lag_results <- data.frame()

for (t_lag in lag_times) {
  
  flows   <- c()
  mean_ts <- c()
  hm_lrs  <- c()
  hm_dfs  <- c()
  
  for (cl in cycle_list) {
    cyc  <- cl$cyc
    flow <- cl$flow
    if (max(cyc$time_loop) < t_lag) next
    
    # Interpolate each sensor to t_lag
    vals <- sapply(off_cols, function(s)
      approx(cyc$time_loop, cyc[[s]], xout = t_lag)$y)
    names(vals) <- off_cols
    if (any(is.na(vals))) next
    
    flows   <- c(flows, flow)
    mean_ts <- c(mean_ts, mean(vals))
    
    A <- vals["temp_A_off"]; B <- vals["temp_B_off"]; C <- vals["temp_C_off"]
    D <- vals["temp_D_off"]; E <- vals["temp_E_off"]; F <- vals["temp_F_off"]
    
    # Difference-based horiz_mag
    tx_d <- (A-D)*sin(angs[1]) + (B-E)*sin(angs[2]) + (C-F)*sin(angs[3])
    tz_d <- (A-D)*cos(angs[1]) + (B-E)*cos(angs[2]) + (C-F)*cos(angs[3])
    hm_dfs <- c(hm_dfs, sqrt(tx_d^2 + tz_d^2))
    
    # Log-ratio horiz_mag (requires all > 0)
    if (min(A, B, C, D, E, F) > 0) {
      lr_ad <- log(A / D)
      lr_be <- log(B / E)
      lr_cf <- log(C / F)
      tx_lr <- lr_ad*sin(angs[1]) + lr_be*sin(angs[2]) + lr_cf*sin(angs[3])
      tz_lr <- lr_ad*cos(angs[1]) + lr_be*cos(angs[2]) + lr_cf*cos(angs[3])
      hm_lrs <- c(hm_lrs, sqrt(tx_lr^2 + tz_lr^2))
    } else {
      hm_lrs <- c(hm_lrs, NA)
    }
  }
  
  if (length(flows) < 10) next
  
  # Compute R² for each metric
  y <- flows
  
  # mean_t linear
  m1 <- lm(y ~ mean_ts)
  r2_mean_lin <- summary(m1)$r.squared
  
  # mean_t quadratic
  m2 <- lm(y ~ mean_ts + I(mean_ts^2))
  r2_mean_quad <- summary(m2)$r.squared
  
  # hm_logratio linear
  valid_lr <- !is.na(hm_lrs)
  if (sum(valid_lr) >= 10) {
    m3 <- lm(y[valid_lr] ~ hm_lrs[valid_lr])
    r2_hm_lr_lin <- summary(m3)$r.squared
    m4 <- lm(y[valid_lr] ~ hm_lrs[valid_lr] + I(hm_lrs[valid_lr]^2))
    r2_hm_lr_quad <- summary(m4)$r.squared
  } else {
    r2_hm_lr_lin  <- NA
    r2_hm_lr_quad <- NA
  }
  
  # hm_diff linear
  valid_df <- !is.na(hm_dfs)
  if (sum(valid_df) >= 10) {
    m5 <- lm(y[valid_df] ~ hm_dfs[valid_df])
    r2_hm_diff_lin <- summary(m5)$r.squared
  } else {
    r2_hm_diff_lin <- NA
  }
  
  lag_results <- bind_rows(lag_results, data.frame(
    lag_time       = t_lag,
    n_cycles       = length(flows),
    r2_mean_lin    = r2_mean_lin,
    r2_mean_quad   = r2_mean_quad,
    r2_hm_lr_lin   = r2_hm_lr_lin,
    r2_hm_lr_quad  = r2_hm_lr_quad,
    r2_hm_diff_lin = r2_hm_diff_lin
  ))
}

cat("Done.\n\n")


# ---- STEP 4: Report optimal lag times ----

cat("=== OPTIMAL LAG TIMES (peak R² for each metric) ===\n")
metrics <- c("r2_mean_lin", "r2_mean_quad", "r2_hm_lr_lin", "r2_hm_lr_quad", "r2_hm_diff_lin")
labels  <- c("Mean temp (linear)", "Mean temp (quadratic)",
             "Log-ratio mag (linear)", "Log-ratio mag (quadratic)",
             "Difference mag (linear)")

for (k in seq_along(metrics)) {
  col <- metrics[k]
  valid <- lag_results[!is.na(lag_results[[col]]), ]
  if (nrow(valid) == 0) next
  best_row <- valid[which.max(valid[[col]]), ]
  cat(sprintf("  %-30s  R² = %.4f  at t = %ds  (n = %d cycles)\n",
              labels[k], best_row[[col]], best_row$lag_time, best_row$n_cycles))
}


# ---- STEP 5: Plot R² vs lag time ----

library(reshape2)

plot_df <- lag_results[, c("lag_time", metrics)]
plot_melt <- melt(plot_df, id.vars = "lag_time",
                  variable.name = "metric", value.name = "r2")

# Rename for display
plot_melt$metric <- factor(plot_melt$metric, levels = metrics, labels = labels)

p_lag <- ggplot(plot_melt, aes(x = lag_time, y = r2, color = metric)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  # Annotate cycle count along the bottom
  geom_text(data = lag_results[seq(1, nrow(lag_results), by = 5), ],
            aes(x = lag_time, y = -0.04, label = n_cycles),
            inherit.aes = FALSE, size = 2, color = "gray50") +
  scale_color_manual(values = c(
    "Mean temp (linear)"         = "#E24B4A",
    "Mean temp (quadratic)"      = "#BA7517",
    "Log-ratio mag (linear)"     = "#1D9E75",
    "Log-ratio mag (quadratic)"  = "#534AB7",
    "Difference mag (linear)"    = "#73726c"
  )) +
  labs(
    x        = "Lag time (seconds after cycle start)",
    y        = expression(R^2 ~ "(flow prediction)"),
    title    = expression(R^2 ~ "vs lag time for different flow metrics"),
    subtitle = "Numbers along bottom = cycles available at each lag time",
    color    = "Metric"
  ) +
  ylim(-0.08, 1) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))
print(p_lag)


# ---- STEP 6: Extract final features at OPTIMAL lag time ----
# Use the lag time that maximizes R² for log-ratio horiz_mag (quadratic)

valid_lr_rows <- lag_results[!is.na(lag_results$r2_hm_lr_quad), ]

# Only consider lag times where at least 75% of cycles are available
min_n <- round(0.75 * max(valid_lr_rows$n_cycles))
valid_lr_rows <- valid_lr_rows[valid_lr_rows$n_cycles >= min_n, ]

best_lag <- valid_lr_rows$lag_time[which.max(valid_lr_rows$r2_hm_lr_quad)]
best_r2  <- max(valid_lr_rows$r2_hm_lr_quad)
best_n   <- valid_lr_rows$n_cycles[which.max(valid_lr_rows$r2_hm_lr_quad)]

cat(sprintf("\n=== USING OPTIMAL LAG TIME: %ds ===\n", best_lag))
cat(sprintf("  R² = %.4f  (log-ratio horiz_mag, quadratic)\n", best_r2))
cat(sprintf("  n = %d cycles  (required >= %d, i.e. 75%% of max)\n\n", best_n, min_n))


flow_results <- data.frame()

for (cl in cycle_list) {
  cyc      <- cl$cyc
  flow_val <- cl$flow
  if (max(cyc$time_loop) < best_lag) next
  
  vals <- sapply(off_cols, function(s)
    approx(cyc$time_loop, cyc[[s]], xout = best_lag)$y)
  names(vals) <- off_cols
  if (any(is.na(vals))) next
  if (any(vals <= 0)) next
  
  i <- unique(cyc$cycle)
  
  # Speed
  mean_t    <- mean(vals)
  peak_mean <- max(rowMeans(cyc[, off_cols], na.rm = TRUE))
  
  # Direction — log ratios
  A <- vals["temp_A_off"]; B <- vals["temp_B_off"]; C <- vals["temp_C_off"]
  D <- vals["temp_D_off"]; E <- vals["temp_E_off"]; F <- vals["temp_F_off"]
  
  AD_ratio <- A / D;  BE_ratio <- B / E;  CF_ratio <- C / F
  AD_lr <- log(AD_ratio); BE_lr <- log(BE_ratio); CF_lr <- log(CF_ratio)
  
  total_x <- AD_lr * sin(angs[1]) + BE_lr * sin(angs[2]) + CF_lr * sin(angs[3])
  total_z <- AD_lr * cos(angs[1]) + BE_lr * cos(angs[2]) + CF_lr * cos(angs[3])
  
  horiz_mag     <- sqrt(total_x^2 + total_z^2)
  direction_deg <- (atan2(total_x, total_z) * 180 / pi) %% 360
  
  flow_results <- bind_rows(flow_results, data.frame(
    cycle = i, flow = flow_val,
    mean_t = mean_t, peak_mean = peak_mean,
    horiz_mag = horiz_mag, direction_deg = direction_deg,
    total_x = total_x, total_z = total_z,
    AD_ratio = as.numeric(AD_ratio), BE_ratio = as.numeric(BE_ratio),
    CF_ratio = as.numeric(CF_ratio),
    AD_logratio = as.numeric(AD_lr), BE_logratio = as.numeric(BE_lr),
    CF_logratio = as.numeric(CF_lr)
  ))
}

cat(sprintf("Extracted features for %d cycles at t=%ds\n", nrow(flow_results), best_lag))


# ---- STEP 7: Calibrate flow speed ----

flow_results$mean_t_sq <- flow_results$mean_t^2
speed_model <- lm(flow ~ mean_t + mean_t_sq, data = flow_results)
flow_results$predicted_flow_mean_t <- predict(speed_model, flow_results)

# Also fit on horiz_mag (log-ratio)
flow_results$horiz_mag_sq <- flow_results$horiz_mag^2
hmag_model <- lm(flow ~ horiz_mag + horiz_mag_sq, data = flow_results)
flow_results$predicted_flow_hmag <- predict(hmag_model, flow_results)

cat("\n=== SPEED CALIBRATION: mean_t (quadratic) ===\n")
cat(sprintf("  flow = %.2f*T + %.2f*T² + %.2f\n",
            coef(speed_model)["mean_t"], coef(speed_model)["mean_t_sq"],
            coef(speed_model)["(Intercept)"]))
cat(sprintf("  R² = %.4f\n", summary(speed_model)$r.squared))

cat("\n=== SPEED CALIBRATION: horiz_mag log-ratio (quadratic) ===\n")
cat(sprintf("  flow = %.2f*H + %.2f*H² + %.2f\n",
            coef(hmag_model)["horiz_mag"], coef(hmag_model)["horiz_mag_sq"],
            coef(hmag_model)["(Intercept)"]))
cat(sprintf("  R² = %.4f\n", summary(hmag_model)$r.squared))

# LOO for both
loo_mean_t <- numeric(nrow(flow_results))
loo_hmag   <- numeric(nrow(flow_results))
for (k in 1:nrow(flow_results)) {
  m1 <- lm(flow ~ mean_t + mean_t_sq, data = flow_results[-k, ])
  loo_mean_t[k] <- predict(m1, flow_results[k, , drop = FALSE])
  m2 <- lm(flow ~ horiz_mag + horiz_mag_sq, data = flow_results[-k, ])
  loo_hmag[k] <- predict(m2, flow_results[k, , drop = FALSE])
}
cat(sprintf("\n  LOO MAE (mean_t):     %.1f ml/min\n", mean(abs(flow_results$flow - loo_mean_t))))
cat(sprintf("  LOO MAE (horiz_mag):  %.1f ml/min\n", mean(abs(flow_results$flow - loo_hmag))))


# ---- STEP 8: Direction summary ----

overall_x   <- mean(flow_results$total_x)
overall_z   <- mean(flow_results$total_z)
overall_dir <- (atan2(overall_x, overall_z) * 180 / pi) %% 360

cat(sprintf("\n=== DIRECTION (at t=%ds, log-ratio based) ===\n", best_lag))
cat(sprintf("  Vector-averaged: %.1f°\n\n", overall_dir))

dir_summary <- flow_results %>%
  group_by(flow) %>%
  summarise(
    n = n(),
    dir_mean = round(mean(direction_deg), 1),
    dir_sd   = round(sd(direction_deg), 1),
    hmag_mean = round(mean(horiz_mag), 3),
    hmag_sd   = round(sd(horiz_mag), 3),
    .groups  = "drop"
  )
print(as.data.frame(dir_summary), row.names = FALSE)

summary(lm(predicted_flow_hmag~flow,flow_results))
# ---- STEP 9: Additional plots ----

# Predicted vs actual (horiz_mag model)
p_pred <- ggplot(flow_results, aes(x = flow, y = predicted_flow_hmag)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = as.factor(flow)), size = 2.5, alpha = 0.7) +
  labs(x = "Actual flow (ml/min)", y = "Predicted flow (ml/min)",
       title = sprintf("Predicted vs actual at t=%ds (log-ratio horiz_mag)", best_lag),
       color = "Flow") +
  theme_minimal() + theme(legend.position = "bottom")
print(p_pred)

# Calibration: horiz_mag vs flow
p_cal <- ggplot(flow_results, aes(x = horiz_mag, y = flow)) +
  geom_point(aes(color = as.factor(flow)), size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2),
              se = TRUE, color = "gray40", linetype = "dashed") +
  labs(x = sprintf("Log-ratio horiz_mag at t=%ds", best_lag),
       y = "Flow rate (ml/min)",
       title = "Speed calibration: log-ratio magnitude vs flow",
       color = "Flow") +
  theme_minimal() + theme(legend.position = "bottom")
print(p_cal)

# Direction vectors
p_vec <- ggplot(flow_results, aes(x = total_x, y = total_z, color = as.factor(flow))) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_segment(aes(xend = 0, yend = 0), alpha = 0.15) +
  coord_equal() +
  labs(x = "X (log-ratio)", y = "Z (log-ratio)",
       title = sprintf("Flow direction vectors at t=%ds", best_lag),
       color = "Flow") +
  theme_minimal() + theme(legend.position = "bottom")
print(p_vec)

# Sensor ratios vs flow
library(reshape2)
ratio_melt <- melt(
  flow_results[, c("cycle", "flow", "AD_ratio", "BE_ratio", "CF_ratio")],
  id.vars = c("cycle", "flow"), variable.name = "pair", value.name = "ratio"
)
p_ratios <- ggplot(ratio_melt, aes(x = flow, y = ratio, color = pair)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  scale_color_manual(values = c("AD_ratio"="#534AB7","BE_ratio"="#1D9E75","CF_ratio"="#D85A30"),
                     labels = c("A/D (30°)", "B/E (90°)", "C/F (150°)")) +
  labs(x = "Flow rate (ml/min)", y = sprintf("Sensor ratio at t=%ds", best_lag),
       title = "Opposing sensor pair ratios vs flow", color = "Pair") +
  theme_minimal() + theme(legend.position = "bottom")
print(p_ratios)


cat("\n=== DONE ===\n")
cat("Key objects:\n")
cat("  lag_results   — R² vs lag time for all metrics (for the sweep plot)\n")
cat("  flow_results  — per-cycle results at optimal lag time\n")
cat("  speed_model   — lm() for flow ~ mean_t (quadratic)\n")
cat("  hmag_model    — lm() for flow ~ horiz_mag (quadratic)\n")
cat(sprintf("  best_lag      — optimal lag time: %ds\n", best_lag))
