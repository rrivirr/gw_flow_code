# file: scripts/facet_and_cycle_diffs.R
rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# =================== INPUT ===================
in_csv <- "C:/Users/cade/Desktop/Lab-test/September-LabTesting-data-20250922T085222Z-1-001/October_Lab_Test/Oct_30/0037459.csv"
#C:\Users\cade\Desktop\Lab-test\September-LabTesting-data-20250922T085222Z-1-001\October_Lab_Test
# =============================================

# ---- derive output dir & base name from CSV path ----
stopifnot(file.exists(in_csv))
out_dir <- dirname(in_csv)
base    <- tools::file_path_sans_ext(basename(in_csv))

# ---- read & basic conversions ----
dat <- read.csv(in_csv, check.names = TRUE, stringsAsFactors = FALSE)

# If time.s is UNIX seconds, convert to POSIX; otherwise use elapsed as.numeric
dat$time.s <- suppressWarnings(as.numeric(dat$time.s))
dat$dtp    <- as_datetime(dat$time.s)

# =====================================================
# 1) FACET PLOT OF A..F OVER FULL FILE (and save PNG)
# =====================================================
sensor_regex <- "^RING\\d+_Traw[A-F]$"

long_df <- dat %>%
  select(dtp, matches(sensor_regex)) %>%
  pivot_longer(matches(sensor_regex),
               names_to = "sensor_col", values_to = "temp_c") %>%
  mutate(
    sensor = factor(sub(".*Traw([A-F])$", "\\1", sensor_col),
                    levels = c("A","B","C","D","E","F")),
    temp_c = suppressWarnings(as.numeric(temp_c))
  )

p_facet <- ggplot(long_df, aes(dtp, temp_c)) +
  geom_line() +
  geom_point(size = 0.35) +
  facet_wrap(~ sensor, ncol = 3, scales = "fixed") +
  labs(title = paste0("Thermistor Temperatures (Facet A–F) — ", base),
       x = "Time", y = "Temperature (°C)") +
  scale_x_datetime(date_labels = "%H:%M\n%b %d") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "grey90"),
        plot.title = element_text(face = "bold"))

ggsave(file.path(out_dir, sprintf("%s_facet_AF.png", base)),
       p_facet, width = 12, height = 8, dpi = 400)

# =====================================================
# 2) CYCLE LABELING + DIFFS AT 0/10/20 MIN AFTER HEAT
# =====================================================

# Helper: label cycles by rising edges in HEATER_heater (0->1), with simple debounce
add_cycle_column <- function(df, var_name = "HEATER_heater", min_sep = 10) {
  stopifnot(var_name %in% names(df))
  h <- suppressWarnings(as.integer(df[[var_name]]))
  h[is.na(h)] <- 0L
  h[h != 0L] <- 1L
  prev   <- c(0L, head(h, -1L))
  starts <- which((prev == 0L) & (h == 1L))
  if (length(starts) > 1 && min_sep > 1) {
    keep <- c(TRUE, diff(starts) >= min_sep)
    starts <- starts[keep]
  }
  cyc <- integer(nrow(df)); cyc[] <- NA_integer_
  if (length(starts)) {
    cyc <- cut(seq_len(nrow(df)),
               breaks = c(starts, nrow(df) + 1L),
               right = FALSE, labels = FALSE)
  }
  df$cycle <- as.integer(cyc)
  df
}

# Coerce temps; (optionally) drop obviously bad epochs
dat$temp_A <- suppressWarnings(as.numeric(dat$RING01_TrawA))
dat$temp_B <- suppressWarnings(as.numeric(dat$RING01_TrawB))
dat$temp_C <- suppressWarnings(as.numeric(dat$RING01_TrawC))
dat$temp_D <- suppressWarnings(as.numeric(dat$RING01_TrawD))
dat$temp_E <- suppressWarnings(as.numeric(dat$RING01_TrawE))
dat$temp_F <- suppressWarnings(as.numeric(dat$RING01_TrawF))

# If you want to drop ancient timestamps, keep this; otherwise comment out
dat <- subset(dat, time.s > 5e8)

# Label cycles
dat2 <- add_cycle_column(dat, "HEATER_heater", min_sep = 10)
cycles <- max(dat2$cycle, na.rm = TRUE)

flow_data_compiled <- data.frame()

for (i in seq_len(cycles)) {
  cycle0 <- subset(dat2, cycle == i)
  if (!nrow(cycle0)) next

  heat_cycle <- subset(cycle0, HEATER_heater == 1)
  if (!nrow(heat_cycle)) next

  end_heat <- max(heat_cycle$time.s, na.rm = TRUE)
  t_plus_10 <- end_heat + (10 * 60)
  t_plus_20 <- end_heat + (20 * 60)

  # Interpolate temps at end_heat, +10min, +20min
  A0  <- approx(cycle0$time.s, cycle0$temp_A, xout = end_heat)$y
  A10 <- approx(cycle0$time.s, cycle0$temp_A, xout = t_plus_10)$y
  A20 <- approx(cycle0$time.s, cycle0$temp_A, xout = t_plus_20)$y

  B0  <- approx(cycle0$time.s, cycle0$temp_B, xout = end_heat)$y
  B10 <- approx(cycle0$time.s, cycle0$temp_B, xout = t_plus_10)$y
  B20 <- approx(cycle0$time.s, cycle0$temp_B, xout = t_plus_20)$y

  D0  <- approx(cycle0$time.s, cycle0$temp_D, xout = end_heat)$y
  D10 <- approx(cycle0$time.s, cycle0$temp_D, xout = t_plus_10)$y
  D20 <- approx(cycle0$time.s, cycle0$temp_D, xout = t_plus_20)$y

  E0  <- approx(cycle0$time.s, cycle0$temp_E, xout = end_heat)$y
  E10 <- approx(cycle0$time.s, cycle0$temp_E, xout = t_plus_10)$y
  E20 <- approx(cycle0$time.s, cycle0$temp_E, xout = t_plus_20)$y

  # Opposite differences
  AD0  <- A0  - D0
  AD10 <- A10 - D10
  AD20 <- A20 - D20

  BE0  <- B0  - E0
  BE10 <- B10 - E10
  BE20 <- B20 - E20

  flow_data <- data.frame(
    cycle = i,
    end_heat = end_heat,
    A_D_0 = AD0,   A_D_10 = AD10, A_D_20 = AD20,
    B_E_0 = BE0,   B_E_10 = BE10, B_E_20 = BE20
  )
  flow_data_compiled <- bind_rows(flow_data_compiled, flow_data)
}

# Diffs relative to '0' (end of heat) + vector reconstruction
flow_data_compiled <- flow_data_compiled %>%
  mutate(
    A_D_Diff_10 = A_D_10 - A_D_0,
    A_D_Diff_20 = A_D_20 - A_D_0,
    B_E_Diff_10 = B_E_10 - B_E_0,
    B_E_Diff_20 = B_E_20 - B_E_0,
    # components (hex geometry: B–E axis at ±60°)
    Sum_X_10 = B_E_Diff_10 / 2,
    Sum_X_20 = B_E_Diff_20 / 2,
    Sum_Y_10 = (B_E_Diff_10 / 2) * sqrt(3) + A_D_Diff_10,
    Sum_Y_20 = (B_E_Diff_20 / 2) * sqrt(3) + A_D_Diff_20,
    angle_10 = atan2(Sum_Y_10, Sum_X_10),   # radians
    angle_20 = atan2(Sum_Y_20, Sum_X_20),   # radians
    flow_mag_10 = sqrt(Sum_X_10^2 + Sum_Y_10^2),
    flow_mag_20 = sqrt(Sum_X_20^2 + Sum_Y_20^2),
    angle_deg_10 = (angle_10 * 180/pi + 360) %% 360,
    angle_deg_20 = (angle_20 * 180/pi + 360) %% 360
  )

# ========== PRINT TABLES WITH RESULTANT ==========
resultant_df <- flow_data_compiled %>%
  select(cycle, flow_mag_10, angle_deg_10, flow_mag_20, angle_deg_20)
print(head(resultant_df, 12))

# Save a CSV of the resultant info next to data
readr::write_csv(resultant_df,
                 file.path(out_dir, sprintf("%s_resultant_mag_dir.csv", base)))

# ========== PLOTS: RESULTANT MAGNITUDE & DIRECTION ==========
# Magnitude vs cycle (10 & 20 min)
p_mag <- ggplot(flow_data_compiled, aes(cycle, flow_mag_10)) +
  geom_point() +
  geom_point(aes(y = flow_mag_20), color = "red") +
  labs(title = "Resultant magnitude (°C-equivalent) vs Cycle",
       x = "Cycle", y = "Magnitude",
       subtitle = "Black: 10 min; Red: 20 min") +
  theme_bw()
ggsave(file.path(out_dir, sprintf("%s_resultant_magnitude.png", base)),
       p_mag, width = 9, height = 5.5, dpi = 400)

# Direction vs cycle (degrees)
p_dir <- ggplot(flow_data_compiled, aes(cycle, angle_deg_10)) +
  geom_point() +
  geom_point(aes(y = angle_deg_20), color = "red") +
  labs(title = "Resultant direction (deg from +X, CCW) vs Cycle",
       x = "Cycle", y = "Direction (deg)",
       subtitle = "Black: 10 min; Red: 20 min") +
  scale_y_continuous(breaks = seq(0, 360, by = 30), limits = c(0, 360)) +
  theme_bw()
ggsave(file.path(out_dir, sprintf("%s_resultant_direction.png", base)),
       p_dir, width = 9, height = 5.5, dpi = 400)

# =====================================================
# 3) SAVE THE FOUR SCATTER PLOTS TO THE DATA FOLDER
#    (A−D & B−E diffs vs cycle; common y scale)
# =====================================================
yrng <- range(
  flow_data_compiled$A_D_Diff_10,
  flow_data_compiled$A_D_Diff_20,
  flow_data_compiled$B_E_Diff_10,
  flow_data_compiled$B_E_Diff_20,
  na.rm = TRUE
)
pad  <- diff(yrng) * 0.05
ylim <- c(yrng[1] - pad, yrng[2] + pad)
xlim <- range(flow_data_compiled$cycle, na.rm = TRUE)

p1 <- ggplot(flow_data_compiled, aes(cycle, A_D_Diff_10)) +
  geom_point() +
  labs(title = "A − D @ 10 min", x = "Cycle", y = "ΔT (°C)") +
  scale_y_continuous(limits = ylim) +
  scale_x_continuous(limits = xlim)

p2 <- ggplot(flow_data_compiled, aes(cycle, A_D_Diff_20)) +
  geom_point() +
  labs(title = "A − D @ 20 min", x = "Cycle", y = "ΔT (°C)") +
  scale_y_continuous(limits = ylim) +
  scale_x_continuous(limits = xlim)

p3 <- ggplot(flow_data_compiled, aes(cycle, B_E_Diff_10)) +
  geom_point() +
  labs(title = "B − E @ 10 min", x = "Cycle", y = "ΔT (°C)") +
  scale_y_continuous(limits = ylim) +
  scale_x_continuous(limits = xlim)

p4 <- ggplot(flow_data_compiled, aes(cycle, B_E_Diff_20)) +
  geom_point() +
  labs(title = "B − E @ 20 min", x = "Cycle", y = "ΔT (°C)") +
  scale_y_continuous(limits = ylim) +
  scale_x_continuous(limits = xlim)

ggsave(file.path(out_dir, sprintf("%s_A-D_10min.png", base)), p1, width = 8, height = 5, dpi = 400)
ggsave(file.path(out_dir, sprintf("%s_A-D_20min.png", base)), p2, width = 8, height = 5, dpi = 400)
ggsave(file.path(out_dir, sprintf("%s_B-E_10min.png", base)), p3, width = 8, height = 5, dpi = 400)
ggsave(file.path(out_dir, sprintf("%s_B-E_20min.png", base)), p4, width = 8, height = 5, dpi = 400)

# =====================================================
# 4) CALIBRATION (OPTIONAL BUT REQUESTED)
#    Fit V_known ~ 0 + flow_mag_10 if 'calib' data.frame exists
#    with columns: V_known (e.g., ft/day) and flow_mag_10
# =====================================================
if (exists("calib") &&
    all(c("V_known","flow_mag_10") %in% names(calib)) &&
    nrow(calib) > 1) {

  fit <- lm(V_known ~ 0 + flow_mag_10, data = calib)  # through origin
  k <- as.numeric(coef(fit)[["flow_mag_10"]])
  cat(sprintf("\nCalibration slope k (units per °C-equivalent): %.6f\n", k))

  # predict velocities for this file from resultant magnitudes
  flow_data_compiled <- flow_data_compiled %>%
    mutate(
      pred_velocity_10 = k * flow_mag_10,
      pred_velocity_20 = k * flow_mag_20
    )

  # print a few rows
  print(head(flow_data_compiled %>%
               select(cycle, flow_mag_10, pred_velocity_10,
                      flow_mag_20, pred_velocity_20), 12))

  # Save predictions
  readr::write_csv(flow_data_compiled,
                   file.path(out_dir, sprintf("%s_with_predicted_velocity.csv", base)))

  # (optional) plot predicted velocity vs cycle
  p_vel <- ggplot(flow_data_compiled, aes(cycle, pred_velocity_10)) +
    geom_point() +
    geom_point(aes(y = pred_velocity_20), color = "red") +
    labs(title = "Predicted velocity vs Cycle (from calibration)",
         x = "Cycle", y = "Velocity (units of V_known)",
         subtitle = "Black: 10 min; Red: 20 min") +
    theme_bw()
  ggsave(file.path(out_dir, sprintf("%s_predicted_velocity.png", base)),
         p_vel, width = 9, height = 5.5, dpi = 400)

} else {
  message("Calibration skipped: provide a data.frame `calib` with columns {V_known, flow_mag_10}.")
}

message("All outputs saved to: ", out_dir)
