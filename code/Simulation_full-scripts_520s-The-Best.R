# =============================================================================
# SIMULATION CALIBRATION — COMBINED SCRIPT
# Part 0: Build master CSV from ANSYS output files (skips if CSV exists)
# Part 1: Calibration — speed, XZ angle, YZ angle, 3D recovery
# =============================================================================
rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
  library(patchwork)
})

# =============================================================================
# SHARED PATHS
# =============================================================================
ANSYS_ROOT <- "/home/ayobami/Desktop/simulation_master-data/240sec_ON-time"
DATA_FILE  <- "/home/ayobami/Desktop/simulation_master-data/updated-master/update-master-data.csv"
OUT_DIR    <- "/home/ayobami/Desktop/simulation_master-data/updated-master/calibration_output/final-plots-520s"
dir.create(OUT_DIR,  recursive=TRUE, showWarnings=FALSE)
dir.create(dirname(DATA_FILE), recursive=TRUE, showWarnings=FALSE)

# =============================================================================
# ██████████████████████████████████████████████████████
# PART 0 — BUILD MASTER CSV FROM ANSYS OUTPUT FILES
# ██████████████████████████████████████████████████████
# =============================================================================
# Reads *-rfile.out sensor files from XZ-angles/ and YZ-angles/ folders,
# pivots to wide format, adds metadata, and saves as master CSV.
# Skips automatically if DATA_FILE already exists.
# =============================================================================

XZ_BASE_DIR <- file.path(ANSYS_ROOT, "XZ-angles")
YZ_BASE_DIR <- file.path(ANSYS_ROOT, "YZ-angles")

ON_TIME_S        <- 240.0
CYCLE_DURATION_S <- 1440.0
TYPE_LABEL       <- "Simulated"

EXPECTED_SENSORS <- paste0(
  rep(c("A","B","C","D","E","F"), times=3),
  rep(1:3, each=6))

# ── Part 0 helpers ────────────────────────────────────────────────────────────
sensor_name_from_filename <- function(fname) {
  m <- regmatches(fname, regexec("^([a-fA-F])([123])\\-rfile", fname))[[1]]
  if (length(m) == 3) return(paste0(toupper(m[2]), m[3]))
  if (grepl("^heater-rfile", fname, ignore.case=TRUE)) return("HEATER")
  return(toupper(tools::file_path_sans_ext(fname)))
}

parse_rfile_raw <- function(path) {
  lines <- readLines(path, warn=FALSE)
  times <- numeric(); vals <- numeric(); line_nos <- integer()
  for (i in seq_along(lines)) {
    s <- trimws(lines[i])
    if (nchar(s) == 0) next
    parts <- strsplit(s, "\\s+")[[1]]
    if (length(parts) < 3) next
    iter_val <- suppressWarnings(as.integer(parts[1]))
    v        <- suppressWarnings(as.numeric(parts[2]))
    t        <- suppressWarnings(as.numeric(parts[length(parts)]))
    if (is.na(iter_val) || is.na(v) || is.na(t)) next
    line_nos <- c(line_nos, i); vals <- c(vals, v); times <- c(times, t)
  }
  data.frame(Time=times, Value=vals, LineNo=line_nos, stringsAsFactors=FALSE)
}

parse_angle_from_dir <- function(dir_name) {
  m <- regmatches(dir_name, regexec("([0-9]+\\.?[0-9]*)[_-]degree$", dir_name,
                                    ignore.case=TRUE))[[1]]
  if (length(m) >= 2) return(as.numeric(m[2])); return(NA)
}

angle_str_from_dir <- function(dir_name) {
  sub("[_-]degree$", "", dir_name, ignore.case=TRUE)
}

parse_velocity_from_folder <- function(folder_name) {
  m <- regmatches(folder_name,
                  regexec("degree[a-zA-Z]*_([0-9]+\\.?[0-9]*)ft-day$",
                          folder_name, ignore.case=TRUE))[[1]]
  if (length(m) >= 2) return(as.numeric(m[2])); return(NA)
}

discover_angle_dirs <- function(base_dir) {
  if (!dir.exists(base_dir)) return(character(0))
  all_dirs <- list.dirs(base_dir, full.names=TRUE, recursive=FALSE)
  sort(all_dirs[grepl("[_-]degree$", basename(all_dirs), ignore.case=TRUE)])
}

discover_velocity_dirs <- function(angle_dir, angle_str) {
  if (!dir.exists(angle_dir)) return(character(0))
  all_dirs <- list.dirs(angle_dir, full.names=TRUE, recursive=FALSE)
  pat <- sprintf("^%sdegree[a-zA-Z]*_.*ft-day$",
                 gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", angle_str))
  sort(all_dirs[grepl(pat, basename(all_dirs), ignore.case=TRUE)])
}

load_long_from_out_files <- function(data_dir) {
  files <- sort(list.files(data_dir, pattern="-rfile\\.out$",
                           full.names=TRUE, ignore.case=TRUE))
  if (length(files) == 0) stop(sprintf("No *-rfile.out files in %s", data_dir))
  long_frames <- list()
  for (p in files) {
    sensor <- sensor_name_from_filename(basename(p))
    if (sensor == "HEATER") next
    df <- parse_rfile_raw(p); if (nrow(df) == 0) next
    df$Sensor <- sensor; df$File <- basename(p)
    long_frames[[length(long_frames)+1]] <- df
  }
  if (length(long_frames) == 0) stop(sprintf("No sensor rows in %s", data_dir))
  long_df <- bind_rows(long_frames)
  long_df$Sensor <- toupper(trimws(as.character(long_df$Sensor)))
  long_df$Time <- as.numeric(long_df$Time); long_df$Value <- as.numeric(long_df$Value)
  long_df <- long_df[!is.na(long_df$Sensor) & !is.na(long_df$Time) & !is.na(long_df$Value), ]
  long_df[order(long_df$Sensor, long_df$LineNo), ]
}

pivot_long_to_master_wide <- function(df_long) {
  wide <- df_long %>%
    group_by(Time, Sensor) %>%
    summarise(Value=mean(Value, na.rm=TRUE), .groups="drop") %>%
    pivot_wider(names_from=Sensor, values_from=Value) %>% arrange(Time)
  for (s in EXPECTED_SENSORS) if (!(s %in% names(wide))) wide[[s]] <- NA
  as.data.frame(wide[, c("Time", EXPECTED_SENSORS)])
}

compute_cycle_column <- function(time_series) {
  floor(as.numeric(time_series) / CYCLE_DURATION_S) + 1L
}

build_case_dataframe <- function(data_dir, plane_tag, angle_deg, velocity_ft_day) {
  wide <- pivot_long_to_master_wide(load_long_from_out_files(data_dir))
  if (toupper(plane_tag)=="XZ") { xz<-angle_deg; yz<-0 } else { xz<-0; yz<-angle_deg }
  out <- data.frame(Time=wide$Time, Type=TYPE_LABEL,
                    `Velocity (ft/day)`=velocity_ft_day, `XZ angle (degrees)`=xz,
                    `YZ angle (degrees)`=yz, ON_time_s=ON_TIME_S,
                    cycle_duration_s=CYCLE_DURATION_S, Cycle=compute_cycle_column(wide$Time),
                    stringsAsFactors=FALSE, check.names=FALSE)
  for (s in EXPECTED_SENSORS) out[[s]] <- wide[[s]]; out
}

process_plane <- function(base_dir, plane_tag) {
  all_cases <- list()
  if (!dir.exists(base_dir)) { cat(sprintf("[WARN] Missing: %s\n",base_dir)); return(all_cases) }
  for (angle_dir in discover_angle_dirs(base_dir)) {
    dir_name <- basename(angle_dir)
    if (tolower(dir_name) %in% c("analyses","validation_outputs")) next
    angle_deg <- parse_angle_from_dir(dir_name)
    angle_str <- angle_str_from_dir(dir_name)
    if (is.na(angle_deg)) next
    vel_dirs <- discover_velocity_dirs(angle_dir, angle_str)
    if (length(vel_dirs)==0) next
    cat(sprintf("\n%s | angle = %g\u00b0 | folders = %d\n", plane_tag, angle_deg, length(vel_dirs)))
    for (data_dir in vel_dirs) {
      vel <- parse_velocity_from_folder(basename(data_dir))
      if (is.na(vel)) next
      tryCatch({
        case_df <- build_case_dataframe(data_dir, plane_tag, angle_deg, vel)
        all_cases[[length(all_cases)+1]] <- case_df
        cat(sprintf("  [OK] %s -> rows: %d\n", basename(data_dir), nrow(case_df)))
      }, error=function(e) cat(sprintf("  [ERROR] %s: %s\n", basename(data_dir), conditionMessage(e))))
    }
  }
  all_cases
}

# ── Run Part 0 ────────────────────────────────────────────────────────────────
cat("\n", strrep("=",70), "\n", sep="")
cat("PART 0 — BUILD MASTER CSV FROM ANSYS OUTPUT FILES\n")
cat(strrep("=",70), "\n")

if (file.exists(DATA_FILE)) {
  cat(sprintf("  Master CSV already exists: %s\n", DATA_FILE))
  cat("  Skipping rebuild. Delete the file to force rebuild.\n")
} else {
  cat(sprintf("  ANSYS root: %s\n", ANSYS_ROOT))
  all_frames <- c(process_plane(XZ_BASE_DIR, "XZ"),
                  process_plane(YZ_BASE_DIR, "YZ"))
  if (length(all_frames)==0) stop("No data processed from XZ or YZ folders.")
  master_df <- bind_rows(all_frames) %>%
    arrange(`Velocity (ft/day)`, `XZ angle (degrees)`, `YZ angle (degrees)`, Time)
  final_cols <- c("Time","Type","Velocity (ft/day)","XZ angle (degrees)",
                  "YZ angle (degrees)","ON_time_s","cycle_duration_s","Cycle",
                  EXPECTED_SENSORS)
  master_df <- master_df[, final_cols]
  write.csv(master_df, DATA_FILE, row.names=FALSE)
  cat(sprintf("\n  MASTER CSV SAVED: %s\n", DATA_FILE))
  cat(sprintf("  Rows: %d | Cols: %d\n", nrow(master_df), ncol(master_df)))
  cat(sprintf("  Velocities: %s ft/day\n",
              paste(sort(unique(master_df[["Velocity (ft/day)"]])), collapse=", ")))
}

# =============================================================================
# ██████████████████████████████████████████████████████
# PART 1 — SIMULATION CALIBRATION | tf = 520s
# ██████████████████████████████████████████████████████
# =============================================================================

T_FEATURE  <- 520
T_BASELINE <- 1

A_D_Angle  <- (30  + 60) * pi / 180
B_E_Angle  <- (90  + 60) * pi / 180
F_C_Angle  <- (150 + 60) * pi / 180
RING1_COLS <- c("A1","B1","C1","D1","E1","F1")
RING2_COLS <- c("A2","B2","C2","D2","E2","F2")
RING3_COLS <- c("A3","B3","C3","D3","E3","F3")
RING_Z     <- c(3, 0, -3)

annot_size <- 7.0    # ~20pt bold statistics on all plots
annot_col  <- "black"
annot_face <- "bold"

# Helper to save with title as filename (title NOT shown on plot)
save_fig <- function(p, title, w=12, h=8) {
  fname <- gsub("[|/\\\\:*?\"<> ]", "_", title)
  fname <- gsub("_+", "_", fname)
  fname <- gsub("_$", "", fname)
  path  <- file.path(OUT_DIR, paste0(fname, ".png"))
  ggsave(path, p, width=w, height=h, dpi=600, bg="white")
  cat(sprintf("  Saved: %s.png\n", fname))
}

cal_theme <- theme_bw(base_size = 16) +
  theme(panel.grid.minor  = element_blank(),
        plot.title        = element_blank(),   # title removed — used as filename
        plot.subtitle     = element_blank(),
        axis.title        = element_text(face = "bold", size = 16),
        axis.text         = element_text(face = "bold", size = 15),
        legend.text       = element_text(face = "bold", size = 14),
        legend.title      = element_text(face = "bold", size = 15),
        strip.text        = element_text(face = "bold", size = 15),
        legend.background = element_rect(fill = "white", colour = "grey80"))

vel_colours  <- c("0.5"="#7B2D8B","1"="#2D6DB5","5"="#2196A6",
                  "10"="#1B8A5A","15"="#276419","20"="#E55C00","30"="#B22222")
vel6_colours <- c("1"="#2D6DB5","5"="#2196A6","10"="#1B8A5A",
                  "15"="#276419","20"="#E55C00","30"="#B22222")
xz_colours   <- c("0"="#1B8A5A","15"="#CC5500","30"="#9B59B6",
                  "45"="#E91E8C","60"="#2D6DB5")
yz_colours   <- c("0"="#B22222","30"="#2D6DB5","60"="#1B8A5A","90"="#9B59B6")
d1_colours   <- c("0.5"="#7B2D8B","5"="#2171B5","15"="#238B45","30"="#E55C00")

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading data...\n")
raw <- read.csv(DATA_FILE)
stopifnot(nrow(raw) > 0)
names(raw)[1:7] <- c("Time","Type","Speed_ft","XZ_Angle","YZ_Angle",
                     "ON_time_s","cycle_duration_s")
cat(sprintf("  Rows: %d | Speeds: %s ft/day\n", nrow(raw),
            paste(sort(unique(raw$Speed_ft)), collapse = ", ")))
cat(sprintf("  XZ angles: %s\u00b0\n", paste(sort(unique(raw$XZ_Angle)), collapse = ", ")))
cat(sprintf("  YZ angles: %s\u00b0\n", paste(sort(unique(raw$YZ_Angle)), collapse = ", ")))

# =============================================================================
# FEATURE EXTRACTION
# =============================================================================
cat("\nExtracting features at t =", T_FEATURE, "s vs baseline t =", T_BASELINE, "s...\n")

extract_features <- function(df, t_feat, t_base) {
  combos <- unique(df[, c("Speed_ft","XZ_Angle","YZ_Angle")])
  out    <- vector("list", nrow(combos))
  for (i in seq_len(nrow(combos))) {
    v  <- combos$Speed_ft[i]
    xz <- combos$XZ_Angle[i]
    yz <- combos$YZ_Angle[i]
    grp  <- subset(df, Speed_ft==v & XZ_Angle==xz & YZ_Angle==yz)
    feat <- grp[grp$Time == t_feat, ]
    base <- grp[grp$Time == t_base, ]
    if (nrow(feat)==0 || nrow(base)==0) next
    f <- feat[1,]; b <- base[1,]
    dA2D2 <- (f$A2-f$D2) - (b$A2-b$D2)
    dB2E2 <- (f$B2-f$E2) - (b$B2-b$E2)
    dC2F2 <- (f$C2-f$F2) - (b$C2-b$F2)
    total_x   <- dA2D2*sin(A_D_Angle) + dB2E2*sin(B_E_Angle) + dC2F2*sin(F_C_Angle)
    total_z   <- dA2D2*cos(A_D_Angle) + dB2E2*cos(B_E_Angle) + dC2F2*cos(F_C_Angle)
    horiz_mag <- sqrt(total_x^2 + total_z^2)
    calc_XZ   <- atan2(total_x, total_z) * 180 / pi
    vert_feat <- mean(unlist(f[,RING1_COLS])) - mean(unlist(f[,RING3_COLS]))
    vert_base <- mean(unlist(b[,RING1_COLS])) - mean(unlist(b[,RING3_COLS]))
    vert_diff <- vert_feat - vert_base
    out[[i]] <- data.frame(
      Speed_ft=v, XZ_true=xz, YZ_true=yz,
      dA2D2=dA2D2, dB2E2=dB2E2, dC2F2=dC2F2,
      total_x=total_x, total_z=total_z,
      horiz_mag=horiz_mag, calc_XZ=calc_XZ,
      vert_diff=vert_diff)
  }
  bind_rows(Filter(Negate(is.null), out))
}

tc <- extract_features(raw, T_FEATURE, T_BASELINE)
cat(sprintf("  Extracted %d feature rows.\n", nrow(tc)))

# =============================================================================
# INDEPENDENT CHANNEL CALIBRATION (Reviewer's preferred approach)
# -----------------------------------------------------------------------------
# Each thermal channel is calibrated independently against true speed,
# with an intercept, then inverted to yield a velocity component.
# No cross-channel scaling factor is needed or used.
#
# Horizontal channel (XZ plane, pure horizontal flow at YZ=0°, XZ=0°):
#   H   = b_H × V_h           →   V_h,pred = H   / b_H           [ft/day]
#
# Vertical channel (YZ plane, pure vertical flow at YZ=90°, XZ=0°):
#   ΔTv = b_v × V_v           →   V_v,pred = ΔTv / b_v           [ft/day]
#
# Both V_h,pred and V_v,pred are velocity components in ft/day.
# They are combined geometrically in velocity space:
#   V_3D = sqrt(V_h² + V_v²)           [ft/day]
#   θ_YZ = atan2(V_v, V_h)             [degrees]
#
# The different slopes b_H and b_v naturally account for any difference in
# thermal sensitivity between the horizontal and vertical sensor geometries.
# =============================================================================

# ── Horizontal channel: H = b_H × S  (origin-constrained) ───────────────────
horiz_cal_data <- subset(tc, YZ_true==0 & XZ_true==0)
lm_horiz_ch    <- lm(horiz_mag ~ Speed_ft - 1, data=horiz_cal_data)
b_H <- coef(lm_horiz_ch)[1]   # slope (K per ft/day)
s_horiz_ch     <- summary(lm_horiz_ch)
cat(sprintf("\n  Horizontal: H   = %.5f × S   R²=%.2f\n",
            b_H, floor(s_horiz_ch$r.squared * 100) / 100))

# ── Vertical channel: ΔTv = b_v × S  (origin-constrained) ───────────────────
vert_cal_data  <- subset(tc, XZ_true==0 & YZ_true==90)
lm_vert_ch     <- lm(vert_diff ~ Speed_ft - 1, data=vert_cal_data)
b_v <- coef(lm_vert_ch)[1]   # slope (K per ft/day)
s_vert_ch      <- summary(lm_vert_ch)
cat(sprintf("  Vertical:   ΔTv = %.5f × S   R²=%.2f\n",
            b_v, floor(s_vert_ch$r.squared  * 100) / 100))

# ── Invert to velocity components ────────────────────────────────────────────
tc$S_H <- tc$horiz_mag / b_H            # V_h,pred = H   / b_H  [ft/day]
tc$S_v <- tc$vert_diff  / b_v            # V_v,pred = ΔTv / b_v  [ft/day]

# ── Combine in velocity space ─────────────────────────────────────────────────
tc$total_3d    <- sqrt(tc$S_H^2 + tc$S_v^2)          # V_3D,pred (ft/day)
tc$calc_YZ     <- atan2(tc$S_v, tc$S_H) * 180 / pi   # θ_YZ (degrees)

# =============================================================================
# CALIBRATION 1 — SPEED
# =============================================================================
cat("\n--- Speed Calibration ---\n")
cal_vel          <- subset(tc, YZ_true==0)

# With-intercept fit — used for speed prediction and residuals everywhere
lm_vel           <- lm(horiz_mag ~ Speed_ft, data=cal_vel)
s1               <- summary(lm_vel)
vel_slope        <- coef(lm_vel)[2]
vel_intercept    <- coef(lm_vel)[1]
cal_vel$predicted_speed <- (cal_vel$horiz_mag - vel_intercept) / vel_slope
cal_vel$residual_speed  <- cal_vel$Speed_ft - cal_vel$predicted_speed
cal_vel$predicted_hm    <- predict(lm_vel, cal_vel)
cal_vel$XZ_fac          <- as.factor(cal_vel$XZ_true)
RMSE_vel <- sqrt(mean(cal_vel$residual_speed^2))
MAE_vel  <- mean(abs(cal_vel$residual_speed))

# Origin-constrained fit — used ONLY for p_cal1a display line
lm_vel_origin <- lm(horiz_mag ~ Speed_ft - 1, data=cal_vel)
s1_origin     <- summary(lm_vel_origin)
slope_origin  <- coef(lm_vel_origin)[1]

# Signal-space residuals (K) — for p_cal1a annotation (y-axis is signal, not speed)
resid_signal  <- cal_vel$horiz_mag - predict(lm_vel_origin, cal_vel)
RMSE_signal   <- sqrt(mean(resid_signal^2))
MAE_signal    <- mean(abs(resid_signal))

cat(sprintf("  H = %.5f × S  (origin-constrained display)\n", slope_origin))
cat(sprintf("  R²=%.2f  Signal RMSE=%.2f K  Signal MAE=%.2f K\n",
            floor(s1_origin$r.squared * 100) / 100, RMSE_signal, MAE_signal))
cat(sprintf("  Speed RMSE=%.2f ft/day  Speed MAE=%.2f ft/day  (with-intercept inversion)\n",
            RMSE_vel, MAE_vel))

vel_seq  <- seq(0, max(cal_vel$Speed_ft)*1.05, length.out=200)
pred_df  <- data.frame(Speed_ft=vel_seq,
                       fit=predict(lm_vel_origin, newdata=data.frame(Speed_ft=vel_seq)))
conf_int <- as.data.frame(predict(lm_vel_origin, newdata=data.frame(Speed_ft=vel_seq),
                                  interval="confidence"))
pred_df$lwr <- conf_int$lwr; pred_df$upr <- conf_int$upr

title_cal1a <- sprintf("Speed Calibration Curve | tf=%ds", T_FEATURE)
p_cal1a <- ggplot(cal_vel, aes(Speed_ft, horiz_mag)) +
  geom_ribbon(data=pred_df, aes(x=Speed_ft, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.15) +
  geom_line(data=pred_df, aes(x=Speed_ft, y=fit),
            colour="#2166ac", linewidth=1.3) +
  geom_point(aes(colour=XZ_fac, shape=XZ_fac), size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("H = %.5f \u00d7 S\nR\u00b2 = %.2f\nRMSE = %.2f (K)\nMAE  = %.2f (K)",
                         slope_origin, floor(s1_origin$r.squared * 100) / 100,
                         RMSE_signal, MAE_signal),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=xz_colours, name="XZ Angle (\u00b0)") +
  scale_shape_manual(values=c("0"=16,"15"=17,"30"=15,"45"=18,"60"=8),
                     name="XZ Angle (\u00b0)") +
  labs(
    x="True Speed (ft/day)",
    y="Thermal Horizontal Magnitude (K)") +
  cal_theme
print(p_cal1a)
save_fig(p_cal1a, title_cal1a)

# Speed residuals
title_cal1b <- sprintf("Speed Calibration Residuals | tf=%ds", T_FEATURE)
p_cal1b <- ggplot(cal_vel, aes(Speed_ft, residual_speed, colour=XZ_fac)) +
  geom_hline(yintercept=0, linetype="dashed", linewidth=1) +
  geom_hline(yintercept= RMSE_vel, linetype="dotted", colour="grey50") +
  geom_hline(yintercept=-RMSE_vel, linetype="dotted", colour="grey50") +
  geom_point(size=5, alpha=0.9) +
  geom_smooth(aes(group=1), method="loess", se=FALSE,
              colour="red", linewidth=0.9) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.2,
           label=sprintf("Speed RMSE = %.2f ft/day\nSpeed MAE  = %.2f ft/day\nDotted = \u00b1RMSE",
                         RMSE_vel, MAE_vel),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=xz_colours, name="XZ Angle (\u00b0)") +
  labs(
    x="True Speed (ft/day)",
    y="Residual: True \u2212 Predicted Speed (ft/day)") +
  cal_theme
print(p_cal1b)
save_fig(p_cal1b, title_cal1b)

# Speed residual heatmap
heat_data <- cal_vel %>% group_by(XZ_true, Speed_ft) %>%
  summarise(mean_resid=mean(residual_speed), .groups="drop")
heat_lim <- max(abs(heat_data$mean_resid))
title_cal1d <- sprintf("Speed Residual Heatmap | tf=%ds | RMSE=%.2f ft/day",
                       T_FEATURE, RMSE_vel)
p_cal1d <- ggplot(heat_data, aes(as.factor(Speed_ft),
                                 as.factor(XZ_true), fill=mean_resid)) +
  geom_tile(colour="white", linewidth=0.6) +
  geom_text(aes(label=sprintf("%+.2f", mean_resid)),
            size=4, fontface="bold", colour="black") +
  scale_fill_gradient2(low="#d6604d", mid="white", high="#4393c3",
                       midpoint=0, limits=c(-heat_lim, heat_lim),
                       name="Residual\n(ft/day)") +
  labs(
    x="True Speed (ft/day)",
    y="XZ Angle (\u00b0)") +
  cal_theme + theme(panel.grid=element_blank())
print(p_cal1d)
save_fig(p_cal1d, title_cal1d)

# =============================================================================
# CALIBRATION 2 — XZ ANGLE RECOVERY
# =============================================================================
cat("\n--- XZ Angle Calibration ---\n")
cal_xz        <- subset(tc, YZ_true==0 & Speed_ft>=1)
cal_xz$XZ_err <- cal_xz$calc_XZ - cal_xz$XZ_true

xz_summary <- cal_xz %>% group_by(XZ_true) %>%
  summarise(n=n(),
            bias               = round(mean(XZ_err), 4),
            root_mean_sq_error = round(sqrt(mean(XZ_err^2)), 4),
            mean_abs_error     = round(mean(abs(XZ_err)), 4),
            std_deviation      = round(sd(XZ_err), 4), .groups="drop")

lm_xz <- lm(calc_XZ ~ XZ_true - 1, data=cal_xz); s_xz <- summary(lm_xz)
xz_mae  <- mean(abs(cal_xz$XZ_err))
xz_rmse <- sqrt(mean(cal_xz$XZ_err^2))
cat(sprintf("  XZ  R\u00b2=%.2f  MAE=%.2f\u00b0  RMSE=%.2f\u00b0\n",
            floor(s_xz$r.squared * 100) / 100, xz_mae, xz_rmse))

xz_range <- range(cal_xz$XZ_true)
pred_xz  <- data.frame(XZ_true=seq(xz_range[1], xz_range[2], length.out=100))
pred_xz$calc_XZ <- predict(lm_xz, pred_xz)
conf_xz  <- as.data.frame(predict(lm_xz, pred_xz, interval="confidence"))
pred_xz$lwr <- conf_xz$lwr; pred_xz$upr <- conf_xz$upr

title_cal2a <- sprintf("XZ Angle Recovery | tf=%ds", T_FEATURE)
p_cal2a <- ggplot(cal_xz, aes(XZ_true, calc_XZ,
                              colour=as.factor(Speed_ft))) +
  geom_ribbon(data=pred_xz, aes(x=XZ_true, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.12) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="black", linewidth=1, alpha=0.6) +
  geom_line(data=pred_xz, aes(XZ_true, calc_XZ),
            colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf("R\u00b2=%.2f\nAngle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0",
                         floor(s_xz$r.squared * 100) / 100, xz_mae, xz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=vel6_colours, name="Speed (ft/day)") +
  scale_x_continuous(breaks=c(0,15,30,45,60)) +
  scale_y_continuous(breaks=c(0,15,30,45,60)) +
  coord_equal(xlim=c(-3,70), ylim=c(-30,68)) +
  labs(
    x="True XZ Angle (\u00b0)",
    y="Calculated XZ Angle (\u00b0)") +
  cal_theme
print(p_cal2a)
save_fig(p_cal2a, title_cal2a, w=11, h=11)

title_cal2b <- sprintf("XZ Angle Error Metrics | tf=%ds", T_FEATURE)
xz_long <- xz_summary %>%
  select(XZ_true, bias, mean_abs_error, root_mean_sq_error) %>%
  pivot_longer(-XZ_true, names_to="Metric", values_to="Value") %>%
  mutate(Metric=recode(Metric, bias="Bias",
                       mean_abs_error="MAE",
                       root_mean_sq_error="RMSE"))
p_cal2b <- ggplot(xz_long, aes(as.factor(XZ_true), Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.3,
           label=sprintf("Angle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0", xz_mae, xz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono") +
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837")) +
  labs(
    x="True XZ Angle (\u00b0)",
    y="Error (\u00b0)", fill="Error Metric") +
  cal_theme
print(p_cal2b)
save_fig(p_cal2b, title_cal2b, w=12, h=8)

# =============================================================================
# CALIBRATION 3 — YZ ANGLE RECOVERY
# =============================================================================
cat("\n--- YZ Angle Calibration ---\n")
cal_yz        <- subset(tc, XZ_true==0 & Speed_ft>=1)
cal_yz$YZ_err <- cal_yz$calc_YZ - cal_yz$YZ_true

yz_summary <- cal_yz %>% group_by(YZ_true) %>%
  summarise(n=n(),
            bias               = round(mean(YZ_err), 4),
            root_mean_sq_error = round(sqrt(mean(YZ_err^2)), 4),
            mean_abs_error     = round(mean(abs(YZ_err)), 4),
            std_deviation      = round(sd(YZ_err), 4), .groups="drop")

lm_yz <- lm(calc_YZ ~ YZ_true - 1, data=cal_yz); s_yz <- summary(lm_yz)
yz_mae  <- mean(abs(cal_yz$YZ_err))
yz_rmse <- sqrt(mean(cal_yz$YZ_err^2))
cat(sprintf("  YZ  R\u00b2=%.2f  MAE=%.2f\u00b0  RMSE=%.2f\u00b0\n",
            floor(s_yz$r.squared * 100) / 100, yz_mae, yz_rmse))

yz_range <- range(cal_yz$YZ_true)
pred_yz  <- data.frame(YZ_true=seq(yz_range[1], yz_range[2], length.out=100))
pred_yz$calc_YZ <- predict(lm_yz, pred_yz)
conf_yz  <- as.data.frame(predict(lm_yz, pred_yz, interval="confidence"))
pred_yz$lwr <- conf_yz$lwr; pred_yz$upr <- conf_yz$upr

title_cal3a <- sprintf("YZ Angle Recovery Ring Average | tf=%ds", T_FEATURE)
p_cal3a <- ggplot(cal_yz, aes(YZ_true, calc_YZ,
                              colour=as.factor(Speed_ft))) +
  geom_ribbon(data=pred_yz, aes(x=YZ_true, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.12) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="black", linewidth=1, alpha=0.6) +
  geom_line(data=pred_yz, aes(YZ_true, calc_YZ),
            colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf("R\u00b2=%.2f\nAngle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0",
                         floor(s_yz$r.squared * 100) / 100, yz_mae, yz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=vel6_colours, name="Speed (ft/day)") +
  scale_x_continuous(breaks=c(0,30,60,90)) +
  scale_y_continuous(breaks=c(0,30,60,90)) +
  coord_equal(xlim=c(-3,100), ylim=c(-30,98)) +
  labs(
    x="True YZ Angle (\u00b0)",
    y="Calculated YZ Angle (\u00b0)") +
  cal_theme
print(p_cal3a)
save_fig(p_cal3a, title_cal3a, w=11, h=11)

title_cal3b <- sprintf("YZ Angle Error Metrics | tf=%ds", T_FEATURE)
yz_long <- yz_summary %>%
  select(YZ_true, bias, mean_abs_error, root_mean_sq_error) %>%
  pivot_longer(-YZ_true, names_to="Metric", values_to="Value") %>%
  mutate(Metric=recode(Metric, bias="Bias",
                       mean_abs_error="MAE",
                       root_mean_sq_error="RMSE"))
p_cal3b <- ggplot(yz_long, aes(as.factor(YZ_true), Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.3,
           label=sprintf("Angle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0", yz_mae, yz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono") +
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837")) +
  labs(
    x="True YZ Angle (\u00b0)",
    y="Error (\u00b0)", fill="Error Metric") +
  cal_theme
print(p_cal3b)
save_fig(p_cal3b, title_cal3b, w=12, h=8)

# Vertical channel calibration at YZ=90
# Uses lm_vert_ch (with intercept, same fit used for a_v, b_v above)
vert_vel     <- subset(tc, XZ_true==0 & YZ_true==90)
vert_vel$vv_pred <- predict(lm_vert_ch, vert_vel)
rmse_vv <- sqrt(mean((vert_vel$vert_diff - vert_vel$vv_pred)^2))
mae_vv  <- mean(abs(vert_vel$vert_diff  - vert_vel$vv_pred))
vv_seq  <- seq(0, max(vert_vel$Speed_ft)*1.05, length.out=200)
pred_vv <- data.frame(Speed_ft=vv_seq,
                      fit=predict(lm_vert_ch, newdata=data.frame(Speed_ft=vv_seq)))
conf_vv <- as.data.frame(predict(lm_vert_ch, newdata=data.frame(Speed_ft=vv_seq),
                                 interval="confidence"))
pred_vv$lwr <- conf_vv$lwr; pred_vv$upr <- conf_vv$upr
s_vv    <- summary(lm_vert_ch)

title_cal3c <- sprintf("Vertical Channel Calibration YZ=90 | tf=%ds", T_FEATURE)
p_cal3c <- ggplot(vert_vel, aes(Speed_ft, vert_diff)) +
  geom_ribbon(data=pred_vv, aes(x=Speed_ft, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.15) +
  geom_line(data=pred_vv, aes(x=Speed_ft, y=fit),
            colour="#2166ac", linewidth=1.3) +
  geom_point(size=5.5, alpha=0.9, colour="#d6604d") +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Vertical Signal = %.4f \u00d7 S\nR\u00b2=%.2f\nSignal MAE  = %.2f (K)\nSignal RMSE = %.2f (K)",
                         b_v,
                         floor(s_vv$r.squared * 100) / 100, mae_vv, rmse_vv),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  labs(
    x="True Speed (ft/day)",
    y="Vertical Signal (K)") +
  cal_theme
print(p_cal3c)
save_fig(p_cal3c, title_cal3c)

# =============================================================================
# SPEED CALIBRATION — HORIZONTAL AND VERTICAL CHANNELS (COMBO1)
# =============================================================================
cat("\n--- Speed Calibration: Horizontal and Vertical Channels ---\n")
# Horizontal: YZ=0, XZ=0 only
horiz_vel <- subset(tc, YZ_true==0 & XZ_true==0)
lm_h <- lm(horiz_mag ~ Speed_ft - 1, data=horiz_vel)
s_h  <- summary(lm_h)
h_seq  <- seq(0, max(horiz_vel$Speed_ft)*1.05, length.out=200)
pred_h <- data.frame(Speed_ft=h_seq,
                     fit=predict(lm_h, newdata=data.frame(Speed_ft=h_seq)))
conf_h <- as.data.frame(predict(lm_h, newdata=data.frame(Speed_ft=h_seq),
                                interval="confidence"))
pred_h$lwr <- conf_h$lwr; pred_h$upr <- conf_h$upr
rmse_h <- sqrt(mean((horiz_vel$horiz_mag - predict(lm_h))^2))
mae_h  <- mean(abs(horiz_vel$horiz_mag - predict(lm_h)))

# Compute shared y-axis range for combo1 with padding to avoid ribbon clipping
combo1_ylim <- range(c(horiz_vel$horiz_mag, vert_vel$vert_diff,
                       pred_h$upr, pred_vv$upr), na.rm=TRUE)
combo1_ylim <- c(min(0, combo1_ylim[1]),
                 combo1_ylim[2] * 1.05)

# Build long-format data for facet approach — same y-axis automatically
combo1_data <- bind_rows(
  horiz_vel %>%
    mutate(signal   = horiz_mag,
           channel  = "Horizontal Channel (XZ Plane)"),
  vert_vel %>%
    mutate(signal   = vert_diff,
           channel  = "Vertical Channel (YZ Plane)")
)
combo1_data$channel <- factor(combo1_data$channel,
                              levels=c("Horizontal Channel (XZ Plane)",
                                       "Vertical Channel (YZ Plane)"))

# Build prediction ribbons for both channels
pred_combo1 <- bind_rows(
  pred_h  %>%
    mutate(channel="Horizontal Channel (XZ Plane)") %>%
    rename(signal=fit),
  pred_vv %>%
    mutate(channel="Vertical Channel (YZ Plane)") %>%
    rename(signal=fit)
)
pred_combo1$channel <- factor(pred_combo1$channel,
                              levels=c("Horizontal Channel (XZ Plane)",
                                       "Vertical Channel (YZ Plane)"))

# Annotation data — one label per facet
annot_combo1 <- data.frame(
  channel = factor(
    c("Horizontal Channel (XZ Plane)", "Vertical Channel (YZ Plane)"),
    levels=c("Horizontal Channel (XZ Plane)", "Vertical Channel (YZ Plane)")),
  label = c(
    sprintf("Horizontal Magnitude = %.4f \u00d7 Speed\nR\u00b2 = %.2f\nSignal RMSE = %.2f (K)\nSignal MAE  = %.2f (K)",
            coef(lm_h)[1], floor(s_h$r.squared * 100) / 100, rmse_h, mae_h),
    sprintf("Vertical Signal = %.4f \u00d7 Speed\nR\u00b2 = %.2f\nSignal MAE  = %.2f (K)\nSignal RMSE = %.2f (K)",
            b_v, floor(s_vv$r.squared * 100) / 100, mae_vv, rmse_vv)
  )
)

p_combo1 <- ggplot(combo1_data, aes(x=Speed_ft, y=signal)) +
  geom_ribbon(data=pred_combo1,
              aes(x=Speed_ft, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.15) +
  geom_line(data=pred_combo1,
            aes(x=Speed_ft, y=signal),
            colour="#2166ac", linewidth=1.3, inherit.aes=FALSE) +
  geom_point(size=5, colour="#d6604d", alpha=0.9) +
  geom_text(data=annot_combo1,
            aes(x=-Inf, y=Inf, label=label),
            hjust=-0.05, vjust=1.2, inherit.aes=FALSE,
            size=annot_size, fontface=annot_face,
            colour=annot_col, family="mono", lineheight=0.85) +
  facet_wrap(~channel, nrow=1, scales="fixed") +
  scale_y_continuous(limits=combo1_ylim) +
  labs(x="True Speed (ft/day)", y="Calibration Signal (K)") +
  cal_theme
title_combo1 <- "Speed Calibration Horizontal and Vertical Channels"
print(p_combo1)
save_fig(p_combo1, title_combo1, w=18, h=9)

# =============================================================================
# ANGLE RECOVERY — XZ AND YZ CHANNELS (COMBO2)
# =============================================================================
title_combo2 <- "Angle Recovery XZ Horizontal and YZ Vertical Channels"
# Shared y-axis for combo2: both go 0 to 90 (max of XZ=60, YZ=90)
combo2_ylim <- c(-30, 95)
combo2_breaks <- c(0, 15, 30, 45, 60, 75, 90)

p_combo2_xz <- ggplot(cal_xz, aes(XZ_true, calc_XZ,
                                  colour=as.factor(Speed_ft))) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1) +
  geom_line(data=pred_xz, aes(XZ_true, calc_XZ),
            colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Calculated XZ = %.4f \u00d7 True XZ\nR\u00b2 = %.2f\nAngle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0",
                         coef(lm_xz)[1],
                         floor(s_xz$r.squared * 100) / 100, xz_mae, xz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=vel6_colours, name="Speed (ft/day)") +
  scale_x_continuous(breaks=c(0,15,30,45,60), limits=c(-3,70)) +
  scale_y_continuous(breaks=combo2_breaks, limits=combo2_ylim) +
  labs(x="True Angle (\u00b0)", y="Calculated Angle (\u00b0)") + cal_theme

p_combo2_yz <- ggplot(cal_yz, aes(YZ_true, calc_YZ,
                                  colour=as.factor(Speed_ft))) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1) +
  geom_line(data=pred_yz, aes(YZ_true, calc_YZ),
            colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Calculated YZ = %.4f \u00d7 True YZ\nR\u00b2 = %.2f\nAngle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0",
                         coef(lm_yz)[1],
                         floor(s_yz$r.squared * 100) / 100, yz_mae, yz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=vel6_colours, name="Speed (ft/day)") +
  scale_x_continuous(breaks=c(0,30,60,90), limits=c(-3,100)) +
  scale_y_continuous(breaks=combo2_breaks, limits=combo2_ylim) +
  labs(x="True Angle (\u00b0)", y="Calculated Angle (\u00b0)") + cal_theme

p_combo2 <- (p_combo2_xz | p_combo2_yz) +
  plot_annotation(theme=theme(plot.title=element_blank()))
print(p_combo2)
save_fig(p_combo2, title_combo2, w=18, h=9)

# =============================================================================
# ANGLE ERROR METRICS — XZ AND YZ (COMBO3a)
# =============================================================================
title_combo3a <- "Angle Error Metrics for XZ Plane and YZ Plane"
all_xz_long <- xz_long %>% mutate(plane="XZ Horizontal Angle")
all_yz_long <- yz_long %>% mutate(plane="YZ Vertical Angle") %>%
  rename(XZ_true=YZ_true)
combo_err <- bind_rows(all_xz_long, all_yz_long)

# Add overall stats as annotation data
xz_stats_label <- sprintf("Angle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0", xz_mae, xz_rmse)
yz_stats_label <- sprintf("Angle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0", yz_mae, yz_rmse)
annot_combo3a <- data.frame(
  plane=c("XZ Horizontal Angle","YZ Vertical Angle"),
  label=c(xz_stats_label, yz_stats_label))

p_combo3a <- ggplot(combo_err, aes(as.factor(XZ_true), Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_text(data=annot_combo3a, aes(x=Inf, y=Inf, label=label),
            hjust=1.05, vjust=1.3, inherit.aes=FALSE,
            size=annot_size, fontface=annot_face, colour=annot_col,
            family="mono") +
  facet_wrap(~plane, scales="fixed") +   # fixed = shared y-axis
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837"),
                    name="Error Metric") +
  labs(
    x="True Angle (\u00b0)",
    y="Error (\u00b0)") + cal_theme
print(p_combo3a)
save_fig(p_combo3a, title_combo3a, w=18, h=9)

# =============================================================================
# COMPLETE 3D VECTOR RECOVERY PANEL
# =============================================================================
cat("\n--- Complete 3D Vector Recovery ---\n")
tc$predicted_speed    <- (tc$horiz_mag - vel_intercept) / vel_slope
tc$speed_err          <- tc$predicted_speed - tc$Speed_ft

title_3d_speed <- sprintf("Speed Recovery XZ Plane | tf=%ds", T_FEATURE)
p_3d_speed <- ggplot(subset(tc, YZ_true==0),
                     aes(Speed_ft, predicted_speed,
                         colour=as.factor(XZ_true))) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1, alpha=0.8) +
  geom_smooth(aes(x=Speed_ft, y=predicted_speed),
              method="lm", formula=y~x, colour="#2166ac",
              linewidth=1.3, se=TRUE, inherit.aes=FALSE,
              data=subset(tc, YZ_true==0)) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("YZ = 0\u00b0\nR\u00b2=%.2f\nSpeed MAE  = %.2f ft/day\nSpeed RMSE = %.2f ft/day",
                         floor(s1$r.squared * 100) / 100, MAE_vel, RMSE_vel),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=xz_colours, name="XZ Angle (\u00b0)") +
  scale_x_continuous(breaks=c(0,1,5,10,15,20,30)) +
  scale_y_continuous(breaks=c(0,1,5,10,15,20,30)) +
  coord_equal(xlim=c(-0.5, max(tc$Speed_ft)*1.05),
              ylim=c(-0.5, max(tc$Speed_ft)*1.05)) +
  labs(x="True Speed (ft/day)",
       y="Predicted Speed (ft/day)") + cal_theme
print(p_3d_speed)
save_fig(p_3d_speed, title_3d_speed, w=11, h=11)

# ── Speed recovery for YZ plane ───────────────────────────────────────────────
yz_speed_data          <- subset(tc, XZ_true==0)
yz_speed_data$pred_s   <- (yz_speed_data$horiz_mag - vel_intercept) / vel_slope
yz_r2    <- cor(yz_speed_data$Speed_ft, yz_speed_data$pred_s)^2
yz_mae_s  <- mean(abs(yz_speed_data$Speed_ft - yz_speed_data$pred_s))
yz_rmse_s <- sqrt(mean((yz_speed_data$Speed_ft - yz_speed_data$pred_s)^2))

title_3d_speed_yz <- sprintf("Speed Recovery YZ Plane Horizontal Channel Only | tf=%ds", T_FEATURE)
p_3d_speed_yz <- ggplot(yz_speed_data,
                        aes(Speed_ft, pred_s,
                            colour=as.factor(YZ_true))) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1, alpha=0.8) +
  geom_smooth(aes(x=Speed_ft, y=pred_s),
              method="lm", formula=y~x, colour="#2166ac",
              linewidth=1.3, se=TRUE, inherit.aes=FALSE,
              data=yz_speed_data) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("XZ = 0\u00b0, all YZ angles\nHoriz. channel only\nR\u00b2=%.2f\nSpeed MAE  = %.2f ft/day\nSpeed RMSE = %.2f ft/day",
                         floor(yz_r2 * 100) / 100, yz_mae_s, yz_rmse_s),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=yz_colours, name="YZ Angle (\u00b0)") +
  scale_x_continuous(breaks=c(0,1,5,10,15,20,30)) +
  scale_y_continuous(breaks=c(0,1,5,10,15,20,30)) +
  coord_equal(xlim=c(-0.5, max(tc$Speed_ft)*1.05),
              ylim=c(-0.5, max(tc$Speed_ft)*1.05)) +
  labs(x="True Speed (ft/day)",
       y="Predicted Speed (ft/day)") + cal_theme
print(p_3d_speed_yz)
save_fig(p_3d_speed_yz, title_3d_speed_yz, w=11, h=11)

p_3d_xz <- ggplot(cal_xz, aes(XZ_true, calc_XZ,
                              colour=as.factor(Speed_ft))) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1, alpha=0.8) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf("R\u00b2=%.2f\nAngle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0",
                         floor(s_xz$r.squared * 100) / 100, xz_mae, xz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=vel6_colours, name="Speed (ft/day)") +
  scale_x_continuous(breaks=c(0,15,30,45,60), limits=c(-3,70)) +
  scale_y_continuous(breaks=combo2_breaks, limits=combo2_ylim) +
  labs(
    x="True XZ Angle (\u00b0)",
    y="Calculated XZ Angle (\u00b0)") + cal_theme

p_3d_yz <- ggplot(cal_yz, aes(YZ_true, calc_YZ,
                              colour=as.factor(Speed_ft))) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1, alpha=0.8) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf("R\u00b2=%.2f\nAngle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0",
                         floor(s_yz$r.squared * 100) / 100, yz_mae, yz_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=vel6_colours, name="Speed (ft/day)") +
  scale_x_continuous(breaks=c(0,30,60,90), limits=c(-3,100)) +
  scale_y_continuous(breaks=combo2_breaks, limits=combo2_ylim) +
  labs(
    x="True YZ Angle (\u00b0)",
    y="Calculated YZ Angle (\u00b0)") + cal_theme

# =============================================================================
# TOTAL 3D MAGNITUDE vs TRUE SPEED — ALL YZ ANGLES (D3)
# =============================================================================
# OPTION A — 3D SIGNAL INVARIANCE
# =============================================================================
cat("\n--- Option A: 3D Speed Invariance (all XZ x YZ angles) ---\n")

lm_3d_A   <- lm(total_3d ~ Speed_ft - 1, data=tc)
s_3d_A    <- summary(lm_3d_A)
rmse_3d_A <- sqrt(mean((tc$total_3d - predict(lm_3d_A))^2))
mae_3d_A  <- mean(abs(tc$total_3d  - predict(lm_3d_A)))
cat(sprintf("  Slope=%.4f  R2=%.2f  Speed MAE=%.2f ft/day  Speed RMSE=%.2f ft/day\n",
            coef(lm_3d_A)[1], floor(s_3d_A$r.squared * 100) / 100, mae_3d_A, rmse_3d_A))

v3d_seq   <- seq(0, max(tc$Speed_ft)*1.05, length.out=200)
pred_3d_A <- data.frame(Speed_ft=v3d_seq,
                        fit=predict(lm_3d_A, newdata=data.frame(Speed_ft=v3d_seq)))
conf_3d_A <- as.data.frame(predict(lm_3d_A, newdata=data.frame(Speed_ft=v3d_seq),
                                   interval="confidence"))
pred_3d_A$lwr <- conf_3d_A$lwr; pred_3d_A$upr <- conf_3d_A$upr

tc$angle_label <- ifelse(
  tc$YZ_true == 0,
  paste0("XZ=", tc$XZ_true, "\u00b0"),
  paste0("YZ=", tc$YZ_true, "\u00b0")
)

angle_colours_3d <- c(
  "XZ=0\u00b0"  = "#1B8A5A",
  "XZ=15\u00b0" = "#CC5500",
  "XZ=30\u00b0" = "#9B59B6",
  "XZ=45\u00b0" = "#E91E8C",
  "XZ=60\u00b0" = "#2D6DB5",
  "YZ=30\u00b0" = "#2196A6",
  "YZ=60\u00b0" = "#E55C00",
  "YZ=90\u00b0" = "#B22222"
)

angle_shapes_3d <- c(
  "XZ=0\u00b0"  = 16,
  "XZ=15\u00b0" = 16,
  "XZ=30\u00b0" = 16,
  "XZ=45\u00b0" = 16,
  "XZ=60\u00b0" = 16,
  "YZ=30\u00b0" = 17,
  "YZ=60\u00b0" = 15,
  "YZ=90\u00b0" = 18
)

tc$angle_label <- factor(tc$angle_label, levels=names(angle_colours_3d))

title_d3_A <- "3D Speed Invariance Predicted vs True Speed All XZ and YZ Angles"
p_d3_A <- ggplot(tc, aes(Speed_ft, total_3d,
                         colour=angle_label, shape=angle_label)) +
  geom_ribbon(data=pred_3d_A, aes(x=Speed_ft, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.15) +
  geom_line(data=pred_3d_A, aes(x=Speed_ft, y=fit),
            colour="#2166ac", linewidth=1.3, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("V_3D = %.4f \u00d7 S\nR\u00b2 = %.2f\nSpeed MAE  = %.2f ft/day\nSpeed RMSE = %.2f ft/day",
                         coef(lm_3d_A)[1], floor(s_3d_A$r.squared * 100) / 100,
                         mae_3d_A, rmse_3d_A),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="sans", lineheight=0.85) +
  scale_colour_manual(values=angle_colours_3d, name="Flow Direction") +
  scale_shape_manual(values=angle_shapes_3d,   name="Flow Direction") +
  labs(x="True Speed (ft/day)",
       y="Predicted 3D Speed (ft/day)") + cal_theme
print(p_d3_A)
save_fig(p_d3_A, title_d3_A)

# =============================================================================
# OPTION B — 3D SPEED RECOVERY
# =============================================================================
cat("\n--- Option B: 3D Speed Recovery (all XZ x YZ angles) ---\n")

slope_3d_B     <- coef(lm_3d_A)[1]

tc$pred_speed_3d  <- tc$total_3d / slope_3d_B
tc$resid_speed_3d <- tc$Speed_ft - tc$pred_speed_3d
r2_3d_B    <- cor(tc$Speed_ft, tc$pred_speed_3d)^2
mae_3d_B   <- mean(abs(tc$resid_speed_3d))
rmse_3d_B  <- sqrt(mean(tc$resid_speed_3d^2))

cat(sprintf("  R2=%.2f  Speed MAE=%.2f ft/day  Speed RMSE=%.2f ft/day\n",
            floor(r2_3d_B * 100) / 100, mae_3d_B, rmse_3d_B))

vel_lim_3d <- c(-0.5, max(tc$Speed_ft)*1.05)

title_d3_B <- "3D Speed Recovery Predicted vs True Speed All XZ and YZ Angles"
p_d3_B <- ggplot(tc, aes(Speed_ft, pred_speed_3d,
                         colour=angle_label, shape=angle_label)) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1) +
  geom_smooth(aes(x=Speed_ft, y=pred_speed_3d),
              method="lm", formula=y~x, colour="#2166ac",
              linewidth=1.3, se=TRUE, inherit.aes=FALSE, data=tc) +
  geom_point(size=5, alpha=0.85) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("S = V_3D / %.4f\nR\u00b2 = %.2f\nSpeed MAE  = %.2f ft/day\nSpeed RMSE = %.2f ft/day",
                         slope_3d_B, floor(r2_3d_B * 100) / 100, mae_3d_B, rmse_3d_B),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="sans", lineheight=0.85) +
  scale_colour_manual(values=angle_colours_3d, name="Flow Direction") +
  scale_shape_manual(values=angle_shapes_3d,   name="Flow Direction") +
  coord_equal(xlim=vel_lim_3d, ylim=vel_lim_3d) +
  labs(x="True Speed (ft/day)",
       y="Predicted Speed (ft/day)") + cal_theme
print(p_d3_B)
save_fig(p_d3_B, title_d3_B)

# Combined side-by-side panel A + B
title_d3 <- "3D Speed Recovery versus True Speed All YZ Angles Included"
p_d3_panel <- (p_d3_A | p_d3_B) +
  plot_annotation(theme=theme(plot.title=element_blank()))
print(p_d3_panel)
save_fig(p_d3_panel, title_d3, w=24, h=9)

# =============================================================================
# OPTION C — UNIVERSAL 3D ANGLE RECOVERY
# -----------------------------------------------------------------------------
# Analogous to total_3d being one 3D magnitude across all XZ × YZ conditions,
# calc_3D_angle is one 3D angle across all conditions.
#
# The 3D flow vector has three components:
#   horizontal (XZ plane):  V_h = H   / b_H               [ft/day]
#   vertical (YZ plane):    V_v = \u0394Tv / b_v            [ft/day]
#
# The single 3D elevation angle of this vector from the horizontal plane is:
#   calc_3D_angle = atan2(V_v, V_h)   [degrees]
#
# This is identical to calc_YZ but evaluated universally across ALL
# XZ × YZ angle combinations — not just XZ=0 — confirming that the azimuth
# (XZ rotation) does not corrupt the elevation recovery.
#
# True 3D elevation angle for each condition = YZ_true (the XZ rotation
# does not change the elevation of the flow vector).
#
# A single linear regression of calc_3D_angle ~ YZ_true across all conditions
# is the direct analogue of the total_3d ~ Speed_ft calibration.
# =============================================================================
cat("\n--- Option C: Universal 3D Angle Recovery (all XZ x YZ angles) ---\n")

# Compute universal 3D angle for every row in tc
tc$calc_3D_angle <- atan2(tc$S_v, tc$S_H) * 180 / pi
tc$angle_3D_err  <- tc$calc_3D_angle - tc$YZ_true

# Fit universal linear regression — all XZ and YZ conditions pooled
# Exclude Speed_ft < 1 (same threshold as separate XZ/YZ calibrations)
tc_3d_ang        <- subset(tc, Speed_ft >= 1)
lm_3d_C          <- lm(calc_3D_angle ~ YZ_true - 1, data=tc_3d_ang)
s_3d_C           <- summary(lm_3d_C)
angle_3d_mae     <- mean(abs(tc_3d_ang$angle_3D_err))
angle_3d_rmse    <- sqrt(mean(tc_3d_ang$angle_3D_err^2))
angle_3d_bias    <- mean(tc_3d_ang$angle_3D_err)

cat(sprintf("  Slope=%.5f  R\u00b2=%.2f\n",
            coef(lm_3d_C)[1], floor(s_3d_C$r.squared * 100) / 100))
cat(sprintf("  Angle MAE=%.2f\u00b0  RMSE=%.2f\u00b0  Bias=%.2f\u00b0\n",
            angle_3d_mae, angle_3d_rmse, angle_3d_bias))

# Prediction ribbon for the regression line
ang3d_range  <- range(tc_3d_ang$YZ_true)
pred_3d_C    <- data.frame(YZ_true=seq(ang3d_range[1], ang3d_range[2], length.out=200))
pred_3d_C$calc_3D_angle <- predict(lm_3d_C, pred_3d_C)
conf_3d_C    <- as.data.frame(predict(lm_3d_C, pred_3d_C, interval="confidence"))
pred_3d_C$lwr <- conf_3d_C$lwr; pred_3d_C$upr <- conf_3d_C$upr

# Per-angle error summary
angle_3d_summary <- tc_3d_ang %>%
  group_by(XZ_true, YZ_true) %>%
  summarise(
    bias      = round(mean(angle_3D_err), 4),
    MAE       = round(mean(abs(angle_3D_err)), 4),
    RMSE      = round(sqrt(mean(angle_3D_err^2)), 4),
    .groups   = "drop"
  )
cat("  Per-condition angle errors:\n")
print(as.data.frame(angle_3d_summary))

# ── C1: Universal 3D Angle Recovery — scatter with regression ─────────────────
title_d3_C1 <- "3D Angle Recovery Calculated vs True Elevation Angle All XZ and YZ Angles"
p_d3_C1 <- ggplot(tc_3d_ang, aes(YZ_true, calc_3D_angle,
                                 colour=angle_label, shape=angle_label)) +
  geom_ribbon(data=pred_3d_C,
              aes(x=YZ_true, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.12) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1, alpha=0.8) +
  geom_line(data=pred_3d_C,
            aes(x=YZ_true, y=calc_3D_angle),
            colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Calculated 3D = %.4f \u00d7 True YZ\nR\u00b2 = %.2f\nAngle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0\nAngle Bias = %.2f\u00b0",
                         coef(lm_3d_C)[1],
                         floor(s_3d_C$r.squared * 100) / 100,
                         angle_3d_mae, angle_3d_rmse, angle_3d_bias),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="sans", lineheight=0.85) +
  scale_colour_manual(values=angle_colours_3d, name="Flow Direction") +
  scale_shape_manual(values=angle_shapes_3d,   name="Flow Direction") +
  scale_x_continuous(breaks=c(0,30,60,90)) +
  scale_y_continuous(breaks=c(0,30,60,90)) +
  coord_equal(xlim=c(-5, 98), ylim=c(-15, 98)) +
  labs(x="True 3D Elevation Angle (\u00b0)",
       y="Calculated 3D Elevation Angle (\u00b0)") +
  cal_theme
print(p_d3_C1)
save_fig(p_d3_C1, title_d3_C1, w=11, h=11)

# ── C2: 3D Angle Residuals vs True Angle — coloured by flow direction ──────────
title_d3_C2 <- "3D Angle Residuals vs True Elevation Angle All XZ and YZ Angles"
p_d3_C2 <- ggplot(tc_3d_ang, aes(YZ_true, angle_3D_err,
                                 colour=angle_label, shape=angle_label)) +
  geom_hline(yintercept=0,             linetype="dashed", colour="grey40", linewidth=1) +
  geom_hline(yintercept= angle_3d_mae, linetype="dotted", colour="grey50", linewidth=0.8) +
  geom_hline(yintercept=-angle_3d_mae, linetype="dotted", colour="grey50", linewidth=0.8) +
  geom_point(size=5, alpha=0.9) +
  geom_smooth(aes(x=YZ_true, y=angle_3D_err), method="loess", formula=y~x,
              colour="red", linewidth=0.9, se=FALSE, inherit.aes=FALSE,
              data=tc_3d_ang) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.2,
           label=sprintf("Angle MAE  = %.2f\u00b0\nAngle RMSE = %.2f\u00b0\nBias       = %.2f\u00b0\nDotted = \u00b1MAE",
                         angle_3d_mae, angle_3d_rmse, angle_3d_bias),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="sans", lineheight=0.85) +
  scale_colour_manual(values=angle_colours_3d, name="Flow Direction") +
  scale_shape_manual(values=angle_shapes_3d,   name="Flow Direction") +
  scale_x_continuous(breaks=c(0,30,60,90)) +
  labs(x="True 3D Elevation Angle (\u00b0)",
       y="Angle Residual: Calculated \u2212 True (\u00b0)") +
  cal_theme
print(p_d3_C2)
save_fig(p_d3_C2, title_d3_C2)

# ── C3: 3D Angle Error Metrics per Condition — bar chart ──────────────────────
ang3d_long <- angle_3d_summary %>%
  pivot_longer(cols=c(bias, MAE, RMSE), names_to="Metric", values_to="Value") %>%
  mutate(Metric = recode(Metric, bias="Bias", MAE="MAE", RMSE="RMSE"),
         condition = sprintf("XZ=%d\u00b0\nYZ=%d\u00b0", XZ_true, YZ_true))

title_d3_C3 <- "3D Angle Error Metrics per Flow Direction Condition"
p_d3_C3 <- ggplot(ang3d_long, aes(condition, Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.3,
           label=sprintf("Overall MAE  = %.2f\u00b0\nOverall RMSE = %.2f\u00b0",
                         angle_3d_mae, angle_3d_rmse),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono") +
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837"),
                    name="Error Metric") +
  labs(x="Flow Direction Condition",
       y="Error (\u00b0)") +
  cal_theme + theme(axis.text.x=element_text(size=11))
print(p_d3_C3)
save_fig(p_d3_C3, title_d3_C3, w=16, h=8)

# ── C4: Side-by-side panel — 3D magnitude (A) | 3D angle (C1) ─────────────────
# The natural pair: total_3d is the one 3D magnitude; calc_3D_angle is the one 3D angle
title_d3_mag_ang <- "Universal 3D Speed and 3D Angle Recovery All Conditions"
p_d3_mag_ang <- (p_d3_A | p_d3_C1) +
  plot_annotation(theme=theme(plot.title=element_blank()))
print(p_d3_mag_ang)
save_fig(p_d3_mag_ang, title_d3_mag_ang, w=22, h=11)

# =============================================================================
# SPEED RECOVERY — YZ PLANE USING total_3d (corrected full 3D)
# =============================================================================
yz_speed_data$pred_s_3d  <- yz_speed_data$total_3d / slope_3d_B
yz_r2_3d    <- cor(yz_speed_data$Speed_ft, yz_speed_data$pred_s_3d)^2
yz_mae_3d   <- mean(abs(yz_speed_data$Speed_ft - yz_speed_data$pred_s_3d))
yz_rmse_3d  <- sqrt(mean((yz_speed_data$Speed_ft - yz_speed_data$pred_s_3d)^2))

cat(sprintf("\n  YZ speed recovery (total_3d): R2=%.2f  MAE=%.2f  RMSE=%.2f ft/day\n",
            floor(yz_r2_3d * 100) / 100, yz_mae_3d, yz_rmse_3d))

title_3d_speed_yz_3d <- sprintf("Speed Recovery YZ Plane Using V_3D | tf=%ds", T_FEATURE)
p_3d_speed_yz_3d <- ggplot(yz_speed_data,
                           aes(Speed_ft, pred_s_3d,
                               colour=as.factor(YZ_true))) +
  geom_abline(slope=1, intercept=0, linetype="dashed",
              colour="grey40", linewidth=1, alpha=0.8) +
  geom_smooth(aes(x=Speed_ft, y=pred_s_3d),
              method="lm", formula=y~x, colour="#2166ac",
              linewidth=1.3, se=TRUE, inherit.aes=FALSE,
              data=yz_speed_data) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("XZ = 0\u00b0, all YZ angles\nV_3D used\nR\u00b2=%.2f\nSpeed MAE  = %.2f ft/day\nSpeed RMSE = %.2f ft/day",
                         floor(yz_r2_3d * 100) / 100, yz_mae_3d, yz_rmse_3d),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=yz_colours, name="YZ Angle (\u00b0)") +
  scale_x_continuous(breaks=c(0,1,5,10,15,20,30)) +
  scale_y_continuous(breaks=c(0,1,5,10,15,20,30)) +
  coord_equal(xlim=c(-0.5, max(tc$Speed_ft)*1.05),
              ylim=c(-0.5, max(tc$Speed_ft)*1.05)) +
  labs(x="True Speed (ft/day)",
       y="Predicted Speed (ft/day)") + cal_theme
print(p_3d_speed_yz_3d)
save_fig(p_3d_speed_yz_3d, title_3d_speed_yz_3d, w=11, h=11)

# ── Side-by-side: XZ vs YZ speed recovery ─────────────────────────────────────
title_speed_compare_xz_yz <- sprintf("Speed Recovery Comparison XZ vs YZ Plane | tf=%ds", T_FEATURE)
p_speed_xz_yz <- (p_3d_speed | p_3d_speed_yz_3d) +
  plot_annotation(theme=theme(plot.title=element_blank()))
print(p_speed_xz_yz)
save_fig(p_speed_xz_yz, title_speed_compare_xz_yz, w=22, h=11)

# ── Updated complete 3D panel: speed(XZ) | speed(YZ) | XZ angle | YZ angle ──
title_3d <- sprintf("Complete 3D Flow Vector Recovery Speed XZ Azimuth YZ Elevation | tf=%ds",
                    T_FEATURE)
p_3d_full <- (p_3d_speed | p_3d_speed_yz_3d | p_3d_xz | p_3d_yz) +
  plot_annotation(theme=theme(plot.title=element_blank()))
print(p_3d_full)
save_fig(p_3d_full, title_3d, w=36, h=9)

# =============================================================================
# VERTICAL THERMAL SIGNAL vs YZ ANGLE
# =============================================================================
title_vert_yz <- "Vertical Thermal Signal versus YZ Angle XZ = 0 degrees"
vert_yz_data  <- subset(tc, XZ_true==0) %>%
  filter(Speed_ft %in% c(0.5, 5, 15, 30))
lm_vyz <- lm(vert_diff ~ YZ_true - 1,
             data=subset(tc, XZ_true==0))
s_vyz  <- summary(lm_vyz)
rmse_vyz <- sqrt(mean((subset(tc, XZ_true==0)$vert_diff -
                         predict(lm_vyz))^2))
mae_vyz  <- mean(abs(subset(tc, XZ_true==0)$vert_diff -
                       predict(lm_vyz)))

p_vert_yz <- ggplot(vert_yz_data,
                    aes(YZ_true, vert_diff,
                        colour=as.factor(Speed_ft))) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Vertical Signal = %.4f \u00d7 YZ Angle\nR\u00b2 = %.2f\nSignal MAE  = %.2f (K)\nSignal RMSE = %.2f (K)",
                         coef(lm_vyz)[1],
                         floor(s_vyz$r.squared * 100) / 100, mae_vyz, rmse_vyz),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(
    values=d1_colours,
    labels=c("0.5"="0.5 ft/day","5"="5 ft/day",
             "15"="15 ft/day","30"="30 ft/day"),
    name="Speed (ft/day)") +
  scale_x_continuous(breaks=c(0,30,60,90)) +
  labs(
    x="True YZ Angle (\u00b0)",
    y="Vertical Thermal Signal (K)") + cal_theme
print(p_vert_yz)
save_fig(p_vert_yz, title_vert_yz)

# =============================================================================
# SENSOR PAIR SIGNAL DECOMPOSITION ACROSS XZ ANGLES
# =============================================================================
title_decomp <- "Sensor Pair Signal Decomposition across XZ Angles YZ = 0 degrees"
decomp_data <- subset(tc, YZ_true==0 &
                        Speed_ft %in% c(5, 15, 30)) %>%
  select(Speed_ft, XZ_true, dA2D2, dB2E2, dC2F2, vert_diff) %>%
  pivot_longer(cols=c(dA2D2, dB2E2, dC2F2, vert_diff),
               names_to="pair", values_to="delta_T") %>%
  mutate(pair=recode(pair,
                     dA2D2="A \u2212 D",
                     dB2E2="B \u2212 E",
                     dC2F2="C \u2212 F",
                     vert_diff="Ring1 \u2212 Ring3"),
         vel_label=paste0(Speed_ft, " ft/day"))

p_decomp <- ggplot(decomp_data,
                   aes(XZ_true, delta_T, colour=pair,
                       group=pair, shape=pair)) +
  geom_hline(yintercept=0, linetype="dotted",
             colour="grey50", linewidth=0.8) +
  geom_line(linewidth=1.1) +
  geom_point(size=4, alpha=0.9) +
  facet_wrap(~vel_label, nrow=1) +
  scale_colour_manual(
    values=c("A \u2212 D"="#d6604d",
             "B \u2212 E"="#4393c3",
             "C \u2212 F"="#8B4513",
             "Ring1 \u2212 Ring3"="#1b7837"),
    name="Sensor pair") +
  scale_shape_manual(
    values=c("A \u2212 D"=16,
             "B \u2212 E"=17,
             "C \u2212 F"=18,
             "Ring1 \u2212 Ring3"=15),
    name="Sensor pair") +
  scale_x_continuous(breaks=c(0,15,30,45,60)) +
  labs(
    x="XZ Angle (\u00b0)",
    y="\u0394Temperature Signal (K)") + cal_theme
print(p_decomp)
save_fig(p_decomp, title_decomp, w=18, h=8)

# =============================================================================
# RESIDUAL ANALYSIS — 6-panel
# =============================================================================
cat("\n--- Building Residual Analysis Panel ---\n")
tc_vel_res           <- subset(tc, YZ_true==0)
tc_vel_res$speed_resid <- tc_vel_res$predicted_speed - tc_vel_res$Speed_ft
tc_xz_res            <- subset(tc, YZ_true==0 & Speed_ft>=1)
tc_xz_res$xz_resid   <- tc_xz_res$calc_XZ - tc_xz_res$XZ_true
tc_yz_res            <- subset(tc, XZ_true==0 & Speed_ft>=1)
tc_yz_res$yz_resid   <- tc_yz_res$calc_YZ - tc_yz_res$YZ_true
tc_vel_yz            <- subset(tc, XZ_true==0)
tc_vel_yz$speed_resid <- tc_vel_yz$predicted_speed - tc_vel_yz$Speed_ft
pt <- 5

pa <- ggplot(tc_vel_res, aes(Speed_ft, speed_resid,
                             colour=as.factor(XZ_true))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=xz_colours, name="XZ (\u00b0)") +
  labs(
    x="Speed (ft/day)",
    y="Residual (ft/day)") + cal_theme

pb <- ggplot(tc_vel_res, aes(XZ_true, speed_resid,
                             colour=as.factor(Speed_ft))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=vel_colours, name="Speed (ft/day)") +
  labs(
    x="XZ Angle (\u00b0)",
    y="Residual (ft/day)") + cal_theme

pc <- ggplot(tc_vel_yz, aes(YZ_true, speed_resid,
                            colour=as.factor(Speed_ft))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=vel_colours, name="Speed (ft/day)") +
  labs(
    x="YZ Angle (\u00b0)",
    y="Residual (ft/day)") + cal_theme

pd <- ggplot(tc_xz_res, aes(Speed_ft, xz_resid,
                            colour=as.factor(XZ_true))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=xz_colours, name="XZ (\u00b0)") +
  labs(
    x="Speed (ft/day)",
    y="XZ Error (\u00b0)") + cal_theme

pe <- ggplot(tc_yz_res, aes(Speed_ft, yz_resid,
                            colour=as.factor(YZ_true))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=yz_colours, name="YZ (\u00b0)") +
  labs(
    x="Speed (ft/day)",
    y="YZ Error (\u00b0)") + cal_theme

pf <- ggplot(tc_yz_res, aes(YZ_true, yz_resid,
                            colour=as.factor(Speed_ft))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=vel_colours, name="Speed (ft/day)") +
  labs(
    x="YZ Angle (\u00b0)",
    y="YZ Error (\u00b0)") + cal_theme

p_resid_panel <- (pa|pb|pc) / (pd|pe|pf) +
  plot_annotation(theme=theme(plot.title=element_blank()))
print(p_resid_panel)
ggsave(file.path(OUT_DIR, "Residual_Analysis.png"),
       p_resid_panel, width=26, height=14, dpi=600, bg="white")
cat("  Saved: Residual_Analysis.png\n")

# =============================================================================
# COMBO3b — ALL ERROR PANELS COMBINED
# =============================================================================
title_combo3b <- "All Error Panels"
p_speed_resid_top <- ggplot(cal_vel, aes(Speed_ft, residual_speed,
                                         colour=XZ_fac)) +
  geom_hline(yintercept=0, linetype="dashed", linewidth=1) +
  geom_hline(yintercept= RMSE_vel, linetype="dotted", colour="grey50") +
  geom_hline(yintercept=-RMSE_vel, linetype="dotted", colour="grey50") +
  geom_point(size=5, alpha=0.9) +
  geom_smooth(aes(group=1), method="loess", se=FALSE,
              colour="red", linewidth=0.9) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.2,
           label=sprintf("Speed RMSE = %.2f ft/day\nSpeed MAE  = %.2f ft/day\nDotted = \u00b1RMSE",
                         RMSE_vel, MAE_vel),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_manual(values=xz_colours, name="XZ Angle (\u00b0)") +
  labs(
    x="True Speed (ft/day)",
    y="Residual: True \u2212 Predicted Speed (ft/day)") + cal_theme

p_combo3b <- p_speed_resid_top / p_combo3a +
  plot_annotation(theme=theme(plot.title=element_blank()))
print(p_combo3b)
save_fig(p_combo3b, title_combo3b, w=18, h=15)

# =============================================================================
# SAVE CSV OUTPUTS + FINAL SUMMARY
# =============================================================================
coeff_df <- data.frame(
  Parameter = c(
    "T_FEATURE (s)",
    "Speed slope (H per ft/day)",
    "Speed intercept",
    "Speed R2",
    "Speed RMSE (ft/day)",
    "Speed MAE (ft/day)",
    "XZ R2",
    "XZ MAE (degrees)",
    "XZ RMSE (degrees)",
    "Horiz channel slope b_H (K per ft/day)",
    "Vert channel slope b_v (K per ft/day)",
    "YZ R2",
    "YZ MAE (degrees)",
    "YZ RMSE (degrees)",
    "3D Angle R2 (universal)",
    "3D Angle slope",
    "3D Angle MAE (degrees)",
    "3D Angle RMSE (degrees)",
    "3D Angle Bias (degrees)"
  ),
  Value = c(
    T_FEATURE,
    vel_slope, vel_intercept, floor(s1$r.squared * 100) / 100, RMSE_vel, MAE_vel,
    floor(s_xz$r.squared * 100) / 100, xz_mae, xz_rmse,
    b_H, b_v,
    floor(s_yz$r.squared * 100) / 100, yz_mae, yz_rmse,
    floor(s_3d_C$r.squared * 100) / 100, coef(lm_3d_C)[1],
    angle_3d_mae, angle_3d_rmse, angle_3d_bias
  )
)

write.csv(coeff_df,          file.path(OUT_DIR,"calibration_coefficients.csv"),  row.names=FALSE)
write.csv(xz_summary,        file.path(OUT_DIR,"XZ_angle_error_summary.csv"),    row.names=FALSE)
write.csv(yz_summary,        file.path(OUT_DIR,"YZ_angle_error_summary.csv"),    row.names=FALSE)
write.csv(angle_3d_summary,  file.path(OUT_DIR,"angle_3D_error_summary.csv"),    row.names=FALSE)
write.csv(tc,                file.path(OUT_DIR,"features_extracted.csv"),         row.names=FALSE)

cat("\n", strrep("=",60), "\n", sep="")
cat(sprintf("SIMULATION CALIBRATION SUMMARY | tf=%ds\n", T_FEATURE))
cat(strrep("=",60), "\n")
cat(sprintf("  %-35s %.5f\n",       "Speed slope (H per ft/day):", vel_slope))
cat(sprintf("  %-35s %.5f\n",       "Speed intercept:",            vel_intercept))
cat(sprintf("  %-35s %.2f\n",       "Speed R\u00b2:",              floor(s1$r.squared * 100) / 100))
cat(sprintf("  %-35s %.2f ft/day\n","Speed RMSE:",                 RMSE_vel))
cat(sprintf("  %-35s %.2f ft/day\n","Speed MAE:",                  MAE_vel))
cat(strrep("-",60), "\n")
cat(sprintf("  %-35s %.2f\n",       "XZ R\u00b2:",                 floor(s_xz$r.squared * 100) / 100))
cat(sprintf("  %-35s %.2f\u00b0\n", "XZ MAE:",                    xz_mae))
cat(sprintf("  %-35s %.2f\u00b0\n", "XZ RMSE:",                   xz_rmse))
cat(strrep("-",60), "\n")
cat(sprintf("  %-35s %.5f K/(ft/day)\n","Horiz slope b_H:",           b_H))
cat(sprintf("  %-35s %.5f K/(ft/day)\n","Vert slope b_v:",            b_v))
cat(sprintf("  %-35s %.2f\n",       "YZ R\u00b2 (XZ=0 only):",    floor(s_yz$r.squared * 100) / 100))
cat(sprintf("  %-35s %.2f\u00b0\n", "YZ MAE  (XZ=0 only):",      yz_mae))
cat(sprintf("  %-35s %.2f\u00b0\n", "YZ RMSE (XZ=0 only):",      yz_rmse))
cat(strrep("-",60), "\n")
cat("  3D ANGLE (universal — all XZ x YZ conditions):\n")
cat(sprintf("  %-35s %.2f\n",       "3D Angle R\u00b2:",           floor(s_3d_C$r.squared * 100) / 100))
cat(sprintf("  %-35s %.5f\n",       "3D Angle slope:",             coef(lm_3d_C)[1]))
cat(sprintf("  %-35s %.2f\u00b0\n", "3D Angle MAE:",               angle_3d_mae))
cat(sprintf("  %-35s %.2f\u00b0\n", "3D Angle RMSE:",              angle_3d_rmse))
cat(sprintf("  %-35s %.2f\u00b0\n", "3D Angle Bias:",              angle_3d_bias))
cat(strrep("=",60), "\n")
cat(sprintf("All outputs saved to: %s\n", OUT_DIR))