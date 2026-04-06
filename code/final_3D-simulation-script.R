rm(list = ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
  library(patchwork)
})

# =============================================================================
# GROUNDWATER FLOW SENSOR — CALIBRATION SCRIPT
# Based on: update-master-data.csv
#
# SENSOR GEOMETRY:
#   Ring 1 — 3 cm above Ring 2 (passive thermistors)
#   Ring 2 — heater midpoint  (active thermistors + heater)
#   Ring 3 — 3 cm below Ring 2 (passive thermistors)
#   Each ring: 6 sensors at 60-degree intervals (A, B, C, D, E, F)
#
# 3D FLOW VECTOR RECOVERY:
#   Speed      : horiz_mag from Ring 2 calibration curve
#   XZ angle   : atan2(total_x, total_z) from Ring 2 — horizontal direction
#   YZ angle   : atan2(vert_scaled, horiz_mag) from Ring1/Ring3 average
#   Tilt azimuth: identical to XZ angle — the horizontal projection of any
#                 tilted flow points in the same compass direction as the
#                 full horizontal flow, which Ring 2 already recovers.
#
# WHY RING AVERAGES WORK FOR ALL YZ ANGLES (0 to 90 degrees):
#   At 3 cm spacing, Rings 1 and 3 measure bulk axial temperature.
#   When flow has a vertical component, the heat plume is advected
#   asymmetrically: Ring 1 warms more than Ring 3 (or vice versa)
#   proportionally to sin(YZ). atan2 converts this continuous signal
#   into the elevation angle at any intermediate value (30, 45, 60, 75
#   degrees), not just at 90 degrees. The ring-average difference is
#   the theoretically correct extraction in the linear diffusion regime.
# =============================================================================

DATA_FILE <- "/home/ayobami/Desktop/simulation_master-data/updated-master/update-master-data.csv"
OUT_DIR   <- "/home/ayobami/Desktop/simulation_master-data/updated-master/calibration_output/final-plots"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

T_FEATURE  <- 240
T_BASELINE <- 1
# Sensor positions (angles measured clockwise from -x axis):
#   A = 90°  (+z, north)     D = 270° (-z, south)   → A-D axis along z-axis
#   B = 150° (upper right)   E = 330° (lower left)  → B-E axis at 150-330°
#   F = 30°  (upper left)    C = 210° (lower right) → F-C axis at 30-210°
#
# At XZ = 0° (flow along -x, westward):
#   A-D axis is perpendicular to flow → dAD ≈ 0
#   B-E axis has maximum projection onto -x → dBE is largest (negative: E side warm)
#   F-C axis also has projection onto -x → dCF is also non-zero
#
# The (sensor_position + 60°) offset in each angle converts the sensor position
# to the correct projection angle for the sin/cos vector decomposition.
A_D_Angle  <- (30  + 60) * pi / 180   # A at 90°  (+z), D at 270° (-z)
B_E_Angle  <- (90  + 60) * pi / 180   # B at 150°,      E at 330°
F_C_Angle  <- (150 + 60) * pi / 180   # F at 30°,       C at 210°
RING1_COLS <- c("A1","B1","C1","D1","E1","F1")
RING3_COLS <- c("A3","B3","C3","D3","E3","F3")

annot_size <- 4.2
annot_col  <- "black"
annot_face <- "bold"

cal_theme <- theme_bw(base_size = 14) +
  theme(panel.grid.minor  = element_blank(),
        plot.title        = element_text(face = "bold"),
        plot.subtitle     = element_blank(),
        axis.title        = element_text(face = "bold"),
        axis.text         = element_text(face = "bold"),
        legend.text       = element_text(face = "bold"),
        legend.title      = element_text(face = "bold"),
        strip.text        = element_text(face = "bold"),
        legend.background = element_rect(fill = "white", colour = "grey80"))

# =============================================================================
# LOAD DATA
# =============================================================================
cat("Loading data...\n")
raw <- read.csv(DATA_FILE)
stopifnot(nrow(raw) > 0)

names(raw)[1:7] <- c("Time","Type","Velocity_ft","XZ_Angle","YZ_Angle",
                     "ON_time_s","cycle_duration_s")

cat(sprintf("  Rows: %d | Velocities: %s ft/day\n", nrow(raw),
            paste(sort(unique(raw$Velocity_ft)), collapse = ", ")))
cat(sprintf("  XZ angles: %s degrees\n", paste(sort(unique(raw$XZ_Angle)), collapse = ", ")))
cat(sprintf("  YZ angles: %s degrees\n", paste(sort(unique(raw$YZ_Angle)), collapse = ", ")))

# =============================================================================
# FEATURE EXTRACTION
# =============================================================================
cat("\nExtracting features at t =", T_FEATURE, "s versus baseline t =", T_BASELINE, "s...\n")

extract_features <- function(df, t_feat, t_base) {
  combos <- unique(df[, c("Velocity_ft","XZ_Angle","YZ_Angle")])
  out    <- vector("list", nrow(combos))
  
  for (i in seq_len(nrow(combos))) {
    v  <- combos$Velocity_ft[i]
    xz <- combos$XZ_Angle[i]
    yz <- combos$YZ_Angle[i]
    
    grp  <- subset(df, Velocity_ft==v & XZ_Angle==xz & YZ_Angle==yz)
    feat <- grp[grp$Time == t_feat, ]
    base <- grp[grp$Time == t_base, ]
    if (nrow(feat)==0 || nrow(base)==0) next
    
    f <- feat[1,]; b <- base[1,]
    
    # ── Horizontal pairs: Ring 2 opposite sensors ─────────────────────────────
    dA2D2 <- (f$A2 - f$D2) - (b$A2 - b$D2)
    dB2E2 <- (f$B2 - f$E2) - (b$B2 - b$E2)
    dC2F2 <- (f$C2 - f$F2) - (b$C2 - b$F2)
    
    # ── Horizontal vector decomposition (Ring 2) ──────────────────────────────
    total_x   <- dA2D2*sin(A_D_Angle) + dB2E2*sin(B_E_Angle) + dC2F2*sin(F_C_Angle)
    total_z   <- dA2D2*cos(A_D_Angle) + dB2E2*cos(B_E_Angle) + dC2F2*cos(F_C_Angle)
    horiz_mag <- sqrt(total_x^2 + total_z^2)
    calc_XZ   <- atan2(total_x, total_z) * 180 / pi
    
    # ── Vertical signal: ring-average difference (Ring 1 minus Ring 3) ────────
    # At 3 cm spacing, individual sensors in Rings 1 and 3 see bulk axial
    # temperature — azimuthal asymmetry from horizontal flow has diffused
    # away by this distance. The ring average correctly isolates the pure
    # vertical advection component at any YZ angle (0 through 90 degrees).
    # This is the theoretically correct extraction in the linear regime.
    vert_feat <- mean(unlist(f[, RING1_COLS])) - mean(unlist(f[, RING3_COLS]))
    vert_base <- mean(unlist(b[, RING1_COLS])) - mean(unlist(b[, RING3_COLS]))
    vert_diff <- vert_feat - vert_base
    
    out[[i]] <- data.frame(
      Velocity_ft = v, XZ_true = xz, YZ_true = yz,
      dA2D2 = dA2D2, dB2E2 = dB2E2, dC2F2 = dC2F2,
      total_x = total_x, total_z = total_z,
      horiz_mag = horiz_mag, calc_XZ = calc_XZ,
      vert_diff = vert_diff
    )
  }
  bind_rows(Filter(Negate(is.null), out))
}

tc <- extract_features(raw, T_FEATURE, T_BASELINE)
cat(sprintf("  Extracted %d feature rows.\n", nrow(tc)))

# =============================================================================
# VERTICAL SCALE FACTOR
# =============================================================================
vert_scales <- sapply(sort(unique(tc$Velocity_ft)), function(v) {
  h <- tc$horiz_mag[tc$Velocity_ft==v & tc$XZ_true==0 & tc$YZ_true==0]
  d <- tc$vert_diff[tc$Velocity_ft==v & tc$XZ_true==0 & tc$YZ_true==90]
  if (length(h)==0 || length(d)==0 || d==0) return(NA)
  h / d
})
vert_scales <- vert_scales[!is.na(vert_scales)]
VERT_SCALE  <- mean(vert_scales)
cat(sprintf("  Vertical scale factor — Mean = %.4f  SD = %.4f  CV = %.1f%%\n",
            VERT_SCALE, sd(vert_scales), sd(vert_scales)/VERT_SCALE*100))

# ── Full 3D vector ─────────────────────────────────────────────────────────
tc$vert_scaled <- tc$vert_diff * VERT_SCALE
tc$total_3d    <- sqrt(tc$horiz_mag^2 + tc$vert_scaled^2)
tc$calc_YZ     <- atan2(tc$vert_scaled, tc$horiz_mag) * 180 / pi

# Tilt azimuth = XZ angle (horizontal projection of any tilted flow)
# A flow at XZ=45, YZ=30 tilts upward toward the north-east direction.
# The north-east direction IS the XZ angle Ring 2 already measures.
tc$tilt_azimuth <- tc$calc_XZ

cat("\nFull 3D vector summary:\n")
cat(sprintf("  XZ angle recovery  — overall MAE: %.4f degrees\n",
            mean(abs(tc$calc_XZ  - tc$XZ_true), na.rm=TRUE)))
cat(sprintf("  YZ angle recovery  — overall MAE: %.4f degrees\n",
            mean(abs(tc$calc_YZ  - tc$YZ_true), na.rm=TRUE)))

# =============================================================================
# CALIBRATION 1 — VELOCITY
# =============================================================================
cal_vel       <- subset(tc, YZ_true==0)
lm_vel        <- lm(horiz_mag ~ Velocity_ft, data=cal_vel)
s1            <- summary(lm_vel)
vel_slope     <- coef(lm_vel)[2]
vel_intercept <- coef(lm_vel)[1]

cal_vel$predicted_vel <- (cal_vel$horiz_mag - vel_intercept) / vel_slope
cal_vel$residual_vel  <- cal_vel$Velocity_ft - cal_vel$predicted_vel
cal_vel$predicted_hm  <- predict(lm_vel, cal_vel)
cal_vel$XZ_fac        <- as.factor(cal_vel$XZ_true)

RMSE_vel <- sqrt(mean(cal_vel$residual_vel^2))
MAE_vel  <- mean(abs(cal_vel$residual_vel))

vel_seq     <- seq(0, max(cal_vel$Velocity_ft)*1.05, length.out=200)
pred_df     <- data.frame(Velocity_ft=vel_seq,
                          fit=predict(lm_vel, newdata=data.frame(Velocity_ft=vel_seq)))
conf_int    <- as.data.frame(predict(lm_vel, newdata=data.frame(Velocity_ft=vel_seq),
                                     interval="confidence"))
pred_df$lwr <- conf_int$lwr
pred_df$upr <- conf_int$upr

p_cal1a_xz <- ggplot(cal_vel, aes(Velocity_ft, horiz_mag)) +
  geom_ribbon(data=pred_df, aes(x=Velocity_ft, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.15) +
  geom_line(data=pred_df, aes(x=Velocity_ft, y=fit),
            colour="#2166ac", linewidth=1.3) +
  geom_point(aes(colour=XZ_fac, shape=XZ_fac), size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf(
             "Horizontal Magnitude = %.4f \u00d7 True Velocity\n  %+.4f\nR\u00b2 = %.6f\nRMSE = %.4f ft/day\nMAE = %.4f ft/day",
             vel_slope, vel_intercept, s1$r.squared, RMSE_vel, MAE_vel),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_brewer(palette="Dark2") +
  scale_shape_manual(values=c(16,17,15,18,8)) +
  labs(title="Velocity Calibration Curve (Horizontal Plane)",
       x="True Velocity (ft/day)", y="Thermal Horizontal Magnitude (Kelvin)",
       colour="XZ Angle (degrees)", shape="XZ Angle (degrees)") +
  cal_theme

print(p_cal1a_xz)
ggsave(file.path(OUT_DIR,"CAL1a_XZ_velocity_calibration_curve.png"),
       p_cal1a_xz, width=10, height=7, dpi=800)
cat("CAL1a saved\n")

p_cal1b <- ggplot(cal_vel, aes(Velocity_ft, residual_vel, colour=XZ_fac)) +
  geom_hline(yintercept= 0,        linetype="dashed", linewidth=1) +
  geom_hline(yintercept= RMSE_vel, linetype="dotted", colour="grey50") +
  geom_hline(yintercept=-RMSE_vel, linetype="dotted", colour="grey50") +
  geom_point(size=5, alpha=0.9) +
  geom_smooth(aes(group=1), method="loess", se=FALSE, colour="red", linewidth=0.9) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.2,
           label=sprintf("RMSE = %.4f ft/day\nMAE = %.4f ft/day\nDotted lines = \u00b1RMSE",
                         RMSE_vel, MAE_vel),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_brewer(palette="Dark2") +
  labs(title="Velocity Calibration Residuals",
       x="True Velocity (ft/day)",
       y="Residual: True Velocity minus Predicted Velocity (ft/day)",
       colour="XZ Angle (degrees)") +
  cal_theme

print(p_cal1b)
ggsave(file.path(OUT_DIR,"CAL1b_velocity_residuals.png"),
       p_cal1b, width=10, height=6, dpi=600)
cat("CAL1b saved\n")

heat_data <- cal_vel %>%
  group_by(XZ_true, Velocity_ft) %>%
  summarise(mean_resid=mean(residual_vel), .groups="drop")
heat_lim <- max(abs(heat_data$mean_resid))

p_cal1d <- ggplot(heat_data,
                  aes(as.factor(Velocity_ft), as.factor(XZ_true), fill=mean_resid)) +
  geom_tile(colour="white", linewidth=0.6) +
  geom_text(aes(label=sprintf("%+.4f", mean_resid)), size=4, fontface="bold", colour="black") +
  scale_fill_gradient2(low="#d6604d", mid="white", high="#4393c3",
                       midpoint=0, limits=c(-heat_lim, heat_lim), name="Residual\n(ft/day)") +
  labs(title=sprintf("Velocity Residual Heatmap — RMSE = %.4f ft/day | White Cells Indicate Zero Residual",
                     RMSE_vel),
       x="True Velocity (ft/day)", y="XZ Angle (degrees)") +
  cal_theme + theme(panel.grid=element_blank())

print(p_cal1d)
ggsave(file.path(OUT_DIR,"CAL1d_velocity_error_heatmap.png"),
       p_cal1d, width=10, height=6, dpi=600)
cat("CAL1d saved\n")

# =============================================================================
# CALIBRATION 2 — XZ ANGLE RECOVERY
# =============================================================================
cal_xz        <- subset(tc, YZ_true==0 & Velocity_ft>=1)
cal_xz$XZ_err <- cal_xz$calc_XZ - cal_xz$XZ_true

xz_summary <- cal_xz %>%
  group_by(XZ_true) %>%
  summarise(n=n(), bias=round(mean(XZ_err),4),
            root_mean_sq_error=round(sqrt(mean(XZ_err^2)),4),
            mean_abs_error=round(mean(abs(XZ_err)),4),
            std_deviation=round(sd(XZ_err),4), .groups="drop")

lm_xz <- lm(calc_XZ ~ XZ_true, data=cal_xz)
s_xz  <- summary(lm_xz)

xz_range <- range(cal_xz$XZ_true)
pred_xz  <- data.frame(XZ_true=seq(xz_range[1], xz_range[2], length.out=100))
pred_xz$calc_XZ <- predict(lm_xz, pred_xz)
conf_xz  <- as.data.frame(predict(lm_xz, pred_xz, interval="confidence"))
pred_xz$lwr <- conf_xz$lwr; pred_xz$upr <- conf_xz$upr

p_cal2a <- ggplot(cal_xz, aes(XZ_true, calc_XZ, colour=as.factor(Velocity_ft))) +
  geom_ribbon(data=pred_xz, aes(x=XZ_true, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.12) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="black", linewidth=1, alpha=0.6) +
  geom_line(data=pred_xz, aes(XZ_true, calc_XZ), colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf(
             "Calculated XZ = %.4f \u00d7 True XZ\n  %+.4f\nR\u00b2 = %.6f\nMAE = %.4f\u00b0\nRMSE = %.4f\u00b0",
             coef(lm_xz)[2], coef(lm_xz)[1], s_xz$r.squared,
             mean(abs(cal_xz$XZ_err)), sqrt(mean(cal_xz$XZ_err^2))),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_viridis_d(option="D", end=0.82) +
  scale_x_continuous(breaks=c(0,15,30,45,60)) +
  scale_y_continuous(breaks=c(0,15,30,45,60)) +
  coord_equal(xlim=c(-3,70), ylim=c(-30,68)) +
  labs(title="XZ Horizontal Angle Recovery",
       x="True XZ Angle (degrees)", y="Calculated XZ Angle (degrees)",
       colour="Velocity (ft/day)") +
  cal_theme

print(p_cal2a)
ggsave(file.path(OUT_DIR,"CAL2a_XZ_angle_recovery.png"), p_cal2a, width=8, height=8, dpi=600)
cat("CAL2a saved\n")

xz_long <- xz_summary %>%
  select(XZ_true, bias, mean_abs_error, root_mean_sq_error) %>%
  pivot_longer(-XZ_true, names_to="Metric", values_to="Value") %>%
  mutate(Metric=recode(Metric, bias="Bias", mean_abs_error="MAE", root_mean_sq_error="RMSE"))

p_cal2b <- ggplot(xz_long, aes(as.factor(XZ_true), Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.3,
           label=sprintf("MAE = %.3f\u00b0\nRMSE = %.3f\u00b0",
                         mean(abs(cal_xz$XZ_err)), sqrt(mean(cal_xz$XZ_err^2))),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837")) +
  labs(title="Angle Error Metrics for XZ",
       x="True XZ Angle (degrees)", y="Error (degrees)", fill="Error Metric") +
  cal_theme

print(p_cal2b)
ggsave(file.path(OUT_DIR,"CAL2b_XZ_angle_error_metrics.png"), p_cal2b, width=9, height=6, dpi=600)
cat("CAL2b saved\n")

# =============================================================================
# CALIBRATION 3 — YZ ANGLE RECOVERY
# =============================================================================
cal_yz        <- subset(tc, XZ_true==0 & Velocity_ft>=1)
cal_yz$YZ_err <- cal_yz$calc_YZ - cal_yz$YZ_true

yz_summary <- cal_yz %>%
  group_by(YZ_true) %>%
  summarise(n=n(), bias=round(mean(YZ_err),4),
            root_mean_sq_error=round(sqrt(mean(YZ_err^2)),4),
            mean_abs_error=round(mean(abs(YZ_err)),4),
            std_deviation=round(sd(YZ_err),4), .groups="drop")

lm_yz <- lm(calc_YZ ~ YZ_true, data=cal_yz)
s_yz  <- summary(lm_yz)

yz_range <- range(cal_yz$YZ_true)
pred_yz  <- data.frame(YZ_true=seq(yz_range[1], yz_range[2], length.out=100))
pred_yz$calc_YZ <- predict(lm_yz, pred_yz)
conf_yz  <- as.data.frame(predict(lm_yz, pred_yz, interval="confidence"))
pred_yz$lwr <- conf_yz$lwr; pred_yz$upr <- conf_yz$upr

p_cal3a <- ggplot(cal_yz, aes(YZ_true, calc_YZ, colour=as.factor(Velocity_ft))) +
  geom_ribbon(data=pred_yz, aes(x=YZ_true, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.12) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="black", linewidth=1, alpha=0.6) +
  geom_line(data=pred_yz, aes(YZ_true, calc_YZ), colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf(
             "Calculated YZ = %.4f \u00d7 True YZ\n  %+.4f\nR\u00b2 = %.6f\nMAE = %.4f\u00b0\nRMSE = %.4f\u00b0",
             coef(lm_yz)[2], coef(lm_yz)[1], s_yz$r.squared,
             mean(abs(cal_yz$YZ_err)), sqrt(mean(cal_yz$YZ_err^2))),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_viridis_d(option="C", end=0.82) +
  scale_x_continuous(breaks=c(0,30,60,90)) +
  scale_y_continuous(breaks=c(0,30,60,90)) +
  coord_equal(xlim=c(-3,100), ylim=c(-30,98)) +
  labs(title="YZ Vertical Angle Recovery — Ring Average Method",
       x="True YZ Angle (degrees)", y="Calculated YZ Angle (degrees)",
       colour="Velocity (ft/day)") +
  cal_theme

print(p_cal3a)
ggsave(file.path(OUT_DIR,"CAL3a_YZ_angle_recovery.png"), p_cal3a, width=8, height=8, dpi=600)
cat("CAL3a saved\n")

yz_long <- yz_summary %>%
  select(YZ_true, bias, mean_abs_error, root_mean_sq_error) %>%
  pivot_longer(-YZ_true, names_to="Metric", values_to="Value") %>%
  mutate(Metric=recode(Metric, bias="Bias", mean_abs_error="MAE", root_mean_sq_error="RMSE"))

p_cal3b <- ggplot(yz_long, aes(as.factor(YZ_true), Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.3,
           label=sprintf("MAE = %.3f\u00b0\nRMSE = %.3f\u00b0",
                         mean(abs(cal_yz$YZ_err)), sqrt(mean(cal_yz$YZ_err^2))),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837")) +
  labs(title="Angle Error Metrics for YZ",
       x="True YZ Angle (degrees)", y="Error (degrees)", fill="Error Metric") +
  cal_theme

print(p_cal3b)
ggsave(file.path(OUT_DIR,"CAL3b_YZ_angle_error_metrics.png"), p_cal3b, width=9, height=6, dpi=600)
cat("CAL3b saved\n")

vert_vel         <- subset(tc, XZ_true==0 & YZ_true==90)
lm_vv            <- lm(vert_diff ~ Velocity_ft, data=vert_vel)
s_vv             <- summary(lm_vv)
vert_vel$vv_pred <- predict(lm_vv, vert_vel)
rmse_vv          <- sqrt(mean((vert_vel$vert_diff - vert_vel$vv_pred)^2))

vv_seq  <- seq(0, max(vert_vel$Velocity_ft)*1.05, length.out=200)
pred_vv <- data.frame(Velocity_ft=vv_seq,
                      fit=predict(lm_vv, newdata=data.frame(Velocity_ft=vv_seq)))
conf_vv <- as.data.frame(predict(lm_vv, newdata=data.frame(Velocity_ft=vv_seq),
                                 interval="confidence"))
pred_vv$lwr <- conf_vv$lwr; pred_vv$upr <- conf_vv$upr

p_cal3c <- ggplot(vert_vel, aes(Velocity_ft, vert_diff)) +
  geom_ribbon(data=pred_vv, aes(x=Velocity_ft, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.15) +
  geom_line(data=pred_vv, aes(x=Velocity_ft, y=fit), colour="#2166ac", linewidth=1.3) +
  geom_point(size=5.5, alpha=0.9, colour="#d6604d") +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Vertical Signal = %.5f \u00d7 True Velocity\n  %+.5f\nR\u00b2 = %.6f\nMAE = %.5f K\nRMSE = %.5f K",
                         coef(lm_vv)[2], coef(lm_vv)[1], s_vv$r.squared,
                         mean(abs(vert_vel$vert_diff - vert_vel$vv_pred)), rmse_vv),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  labs(title="Vertical Channel Velocity Calibration — YZ = 90 degrees, XZ = 0 degrees",
       x="True Velocity (ft/day)", y="Vertical Thermal Signal (Kelvin)") +
  cal_theme

print(p_cal3c)
ggsave(file.path(OUT_DIR,"CAL3c_vertical_velocity_calibration.png"), p_cal3c, width=10, height=6, dpi=600)
cat("CAL3c saved\n")

# =============================================================================
# NEW — FULL 3D VECTOR RECOVERY PLOT
# Shows all three components: speed, XZ azimuth, YZ elevation
# Demonstrates the sensor recovers the complete 3D flow vector
# =============================================================================
cat("Building full 3D vector recovery plots...\n")
suppressPackageStartupMessages(library(patchwork))

# Speed recovery (from horiz_mag across all conditions)
tc$predicted_speed <- (tc$horiz_mag - vel_intercept) / vel_slope
tc$speed_err       <- tc$predicted_speed - tc$Velocity_ft

p_3d_speed <- ggplot(subset(tc, YZ_true==0),
                     aes(Velocity_ft, predicted_speed, colour=as.factor(XZ_true))) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey40",
              linewidth=1, alpha=0.8) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("R\u00b2 = %.6f\nMAE = %.4f ft/day\nRMSE = %.4f ft/day",
                         s1$r.squared, MAE_vel, RMSE_vel),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_brewer(palette="Dark2") +
  labs(title="Speed Recovery",
       x="True Velocity (ft/day)", y="Predicted Velocity (ft/day)",
       colour="XZ Angle (degrees)") +
  cal_theme

# XZ azimuth recovery — use coord_fixed(ratio=1) instead of coord_equal
# so patchwork can size the panel without black gaps
p_3d_xz <- ggplot(cal_xz, aes(XZ_true, calc_XZ, colour=as.factor(Velocity_ft))) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey40",
              linewidth=1, alpha=0.8) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf("R\u00b2 = %.6f\nMAE = %.4f\u00b0\nRMSE = %.4f\u00b0",
                         s_xz$r.squared,
                         mean(abs(cal_xz$XZ_err)), sqrt(mean(cal_xz$XZ_err^2))),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_viridis_d(option="D", end=0.82) +
  scale_x_continuous(breaks=c(0,15,30,45,60), limits=c(-3,70)) +
  scale_y_continuous(breaks=c(0,15,30,45,60), limits=c(-30,68)) +
  labs(title="Horizontal Direction (XZ Azimuth)",
       x="True XZ Angle (degrees)", y="Calculated XZ Angle (degrees)",
       colour="Velocity (ft/day)") +
  cal_theme

# YZ elevation recovery
p_3d_yz <- ggplot(cal_yz, aes(YZ_true, calc_YZ, colour=as.factor(Velocity_ft))) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey40",
              linewidth=1, alpha=0.8) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=-10, hjust=-0.05, vjust=1,
           label=sprintf("R\u00b2 = %.6f\nMAE = %.4f\u00b0\nRMSE = %.4f\u00b0",
                         s_yz$r.squared,
                         mean(abs(cal_yz$YZ_err)), sqrt(mean(cal_yz$YZ_err^2))),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.85) +
  scale_colour_viridis_d(option="C", end=0.82) +
  scale_x_continuous(breaks=c(0,30,60,90), limits=c(-3,100)) +
  scale_y_continuous(breaks=c(0,30,60,90), limits=c(-30,98)) +
  labs(title="Elevation Angle (YZ) — All Intermediate Angles",
       x="True YZ Angle (degrees)", y="Calculated YZ Angle (degrees)",
       colour="Velocity (ft/day)") +
  cal_theme

# Combine with patchwork — clean, no black gaps
p_3d_full <- (p_3d_speed | p_3d_xz | p_3d_yz) +
  plot_annotation(
    title="Complete 3D Flow Vector Recovery: Speed | XZ Azimuth | YZ Elevation",
    theme=theme(plot.title=element_text(face="bold", size=14, hjust=0.5))
  )

# Print to RStudio viewer
print(p_3d_full)

ggsave(file.path(OUT_DIR,"3D_full_vector_recovery.png"),
       p_3d_full, width=18, height=7, dpi=600)
cat("3D full vector recovery plot saved\n")

# =============================================================================
# NEW — YZ ELEVATION ACROSS ALL INTERMEDIATE ANGLES (key result)
# Shows that 30, 60, 90 degrees are all correctly recovered
# =============================================================================
yz_detail <- subset(tc, XZ_true==0 & Velocity_ft %in% c(1,5,15,30))
yz_detail$vel_fac <- factor(yz_detail$Velocity_ft, levels=c(1,5,15,30))

p_yz_intermediate <- ggplot(yz_detail, aes(YZ_true, calc_YZ,
                                           colour=vel_fac, group=vel_fac)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="grey40",
              linewidth=1, alpha=0.7) +
  geom_line(linewidth=1.2, alpha=0.7) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf(
             "YZ = 0\u00b0 recovered: %.1f\u00b0\nYZ = 30\u00b0 recovered: %.1f\u00b0\nYZ = 60\u00b0 recovered: %.1f\u00b0\nYZ = 90\u00b0 recovered: %.1f\u00b0",
             mean(yz_detail$calc_YZ[yz_detail$YZ_true==0],  na.rm=TRUE),
             mean(yz_detail$calc_YZ[yz_detail$YZ_true==30], na.rm=TRUE),
             mean(yz_detail$calc_YZ[yz_detail$YZ_true==60], na.rm=TRUE),
             mean(yz_detail$calc_YZ[yz_detail$YZ_true==90], na.rm=TRUE)),
           size=annot_size, colour=annot_col, fontface=annot_face,
           family="mono", lineheight=0.9) +
  scale_colour_manual(values=c("1"="#7B2D8B","5"="#2171B5","15"="#238B45","30"="#E55C00"),
                      labels=paste0(c(1,5,15,30)," ft/day")) +
  scale_x_continuous(breaks=c(0,30,60,90)) +
  scale_y_continuous(breaks=c(0,30,60,90)) +
  coord_equal(xlim=c(-3,100), ylim=c(-10,98)) +
  labs(title="YZ Elevation Angle Recovery at All Intermediate Angles",
       subtitle=NULL,
       x="True YZ Angle (degrees)",
       y="Calculated YZ Angle (degrees)",
       colour="Velocity") +
  cal_theme

print(p_yz_intermediate)
ggsave(file.path(OUT_DIR,"YZ_intermediate_angle_recovery.png"),
       p_yz_intermediate, width=9, height=8, dpi=600)
cat("YZ intermediate angle recovery plot saved\n")

# =============================================================================
# REMAINING PLOTS (D1, D3, COMBO 1-3)
# =============================================================================
D1_VELS         <- c(0.5, 5, 15, 30)
d1_data         <- subset(tc, XZ_true==0 & Velocity_ft %in% D1_VELS)
d1_data$vel_fac <- factor(d1_data$Velocity_ft, levels=D1_VELS)
lm_d1    <- lm(vert_diff ~ YZ_true, data=d1_data)
r2_d1    <- summary(lm_d1)$r.squared
mae_d1   <- mean(abs(d1_data$vert_diff - predict(lm_d1, d1_data)))
rmse_d1  <- sqrt(mean((d1_data$vert_diff - predict(lm_d1, d1_data))^2))
d1_colours <- c("0.5"="#7B2D8B","5"="#2171B5","15"="#238B45","30"="#E55C00")

p_d1 <- ggplot(d1_data, aes(YZ_true, vert_diff, colour=vel_fac)) +
  geom_point(size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Vertical Signal = %.5f \u00d7 YZ Angle\n  %+.5f\nMAE = %.4f K\nRMSE = %.4f K",
                         coef(lm_d1)[2], coef(lm_d1)[1], mae_d1, rmse_d1),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  scale_colour_manual(values=d1_colours, labels=paste0(D1_VELS," ft/day")) +
  labs(title="Vertical Thermal Signal versus YZ Angle — XZ = 0 degrees",
       x="True YZ Angle (degrees)", y="Vertical Thermal Signal (Kelvin)",
       colour="Velocity (ft/day)") +
  cal_theme

print(p_d1)
ggsave(file.path(OUT_DIR,"Vertical_signal_vs_YZ_angle.png"), p_d1, width=10, height=6, dpi=600)
cat("D1 saved\n")

d3_data        <- tc
d3_data$yz_fac <- as.factor(d3_data$YZ_true)
lm_d3    <- lm(total_3d ~ Velocity_ft, data=d3_data)
r2_d3    <- summary(lm_d3)$r.squared
mae_d3   <- mean(abs(d3_data$total_3d - predict(lm_d3, d3_data)))
rmse_d3  <- sqrt(mean((d3_data$total_3d - predict(lm_d3, d3_data))^2))

d3_seq  <- seq(0, max(d3_data$Velocity_ft)*1.05, length.out=200)
pred_d3 <- data.frame(Velocity_ft=d3_seq,
                      fit=predict(lm_d3, newdata=data.frame(Velocity_ft=d3_seq)))
conf_d3 <- as.data.frame(predict(lm_d3, newdata=data.frame(Velocity_ft=d3_seq),
                                 interval="confidence"))
pred_d3$lwr <- conf_d3$lwr; pred_d3$upr <- conf_d3$upr

p_d3 <- ggplot(d3_data, aes(Velocity_ft, total_3d)) +
  geom_ribbon(data=pred_d3, aes(x=Velocity_ft, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.15) +
  geom_line(data=pred_d3, aes(x=Velocity_ft, y=fit),
            colour="#2166ac", linewidth=1.3, inherit.aes=FALSE) +
  geom_point(aes(colour=yz_fac), size=5, alpha=0.9) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.2,
           label=sprintf("Total 3D Magnitude = %.5f \u00d7 True Velocity\n  %+.5f\nR\u00b2 = %.4f\nMAE = %.4f K\nRMSE = %.4f K",
                         coef(lm_d3)[2], coef(lm_d3)[1], r2_d3, mae_d3, rmse_d3),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  scale_colour_brewer(palette="Set1") +
  labs(title="Total 3D Magnitude versus True Velocity — All YZ Angles Included",
       x="True Velocity (ft/day)", y="Total 3D Magnitude (Kelvin)",
       colour="YZ Angle (degrees)") +
  cal_theme

print(p_d3)
ggsave(file.path(OUT_DIR,"D3_total_3D_magnitude_vs_velocity.png"), p_d3, width=10, height=6, dpi=600)
cat("D3 saved\n")

# =============================================================================
# PLOT 4 — SENSOR PAIR SIGNAL DECOMPOSITION ACROSS XZ ANGLES (YZ = 0)
# Shows how each Ring 2 opposite pair contributes as XZ angle rotates.
# dBE is shown as-is (natural sign) — negative values indicate the B-E pair
# responds in the opposite direction to A-D when the flow azimuth rotates,
# which is expected from the 60-degree sensor spacing geometry.
# =============================================================================
decomp_data <- subset(tc, YZ_true==0 & Velocity_ft %in% c(5,15,30))
decomp_data$vel_fac <- factor(decomp_data$Velocity_ft, levels=c(5,15,30),
                              labels=c("5 ft/day","15 ft/day","30 ft/day"))

decomp_long <- data.frame(
  XZ_angle  = rep(decomp_data$XZ_true, 4),
  Velocity  = rep(decomp_data$vel_fac,  4),
  Signal    = c(decomp_data$dA2D2,
                decomp_data$dB2E2,
                decomp_data$dC2F2,
                decomp_data$vert_diff),
  Pair      = rep(c("A \u2013 D",
                    "B \u2013 E",
                    "C \u2013 F",
                    "Ring1 \u2013 Ring3"),
                  each=nrow(decomp_data))
)

p_plot4 <- ggplot(decomp_long,
                  aes(XZ_angle, Signal, colour=Pair, shape=Pair, group=Pair)) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey50", linewidth=0.8) +
  geom_line(linewidth=1.2) +
  geom_point(size=4) +
  facet_wrap(~Velocity, ncol=3) +
  scale_colour_manual(values=c(
    "A \u2013 D"        = "#d6604d",
    "B \u2013 E"        = "#2166ac",
    "C \u2013 F"        = "#8B4513",
    "Ring1 \u2013 Ring3" = "#1b7837"
  )) +
  scale_shape_manual(values=c(16, 17, 18, 15)) +
  scale_x_continuous(breaks=c(0,15,30,45,60)) +
  labs(title="Sensor Pair Signal Decomposition across XZ Angles (YZ = 0\u00b0)",
       x="XZ Angle (\u00b0)",
       y="\u0394Temperature Signal (K)",
       colour="Sensor pair", shape="Sensor pair") +
  cal_theme +
  theme(legend.position="right")

print(p_plot4)
print(p_plot4)
ggsave(file.path(OUT_DIR,"plot4_signal_decomposition.png"),
       p_plot4, width=14, height=5, dpi=600)
cat("Plot 4 saved\n")

# =============================================================================
# PLOT 6 — RESIDUAL ANALYSIS: SYSTEMATIC BIAS DETECTION
# Layout: paired comparisons side by side
#   Row 1: (a) Velocity residual vs Velocity  |  (d) XZ residual vs Velocity
#   Row 2: (b) Velocity residual vs XZ angle  |  (f) YZ residual vs YZ angle
#   Row 3: (c) Velocity residual vs YZ angle  |  (e) YZ residual vs Velocity
# Yellow replaced throughout — using #E55C00 (burnt orange) and #CC6600 (dark amber).
# =============================================================================

# Compute residuals on appropriate subsets only
# vel_resid: only where YZ=0 — speed calibration was done on horizontal flow
tc_vel_res  <- subset(tc, YZ_true==0)
tc_vel_res$vel_resid <- tc_vel_res$predicted_speed - tc_vel_res$Velocity_ft

# xz_resid: only where YZ=0 — XZ angle only meaningful in horizontal plane
tc_xz_res   <- subset(tc, YZ_true==0 & Velocity_ft>=1)
tc_xz_res$xz_resid  <- tc_xz_res$calc_XZ - tc_xz_res$XZ_true

# yz_resid: only where XZ=0 — YZ angle calibrated at XZ=0
tc_yz_res   <- subset(tc, XZ_true==0 & Velocity_ft>=1)
tc_yz_res$yz_resid  <- tc_yz_res$calc_YZ - tc_yz_res$YZ_true

vel_colours <- c("0.5"="#7B2D8B","1"="#2D6DB5","5"="#2196A6",
                 "10"="#1B8A5A","15"="#5AAE61","20"="#E55C00","30"="#B22222")

xz_colours  <- c("0"="#1B8A5A","15"="#E8A000","30"="#9B59B6",
                 "45"="#E91E8C","60"="#2D6DB5")

yz_colours  <- c("0"="#B22222","30"="#2D6DB5","60"="#1B8A5A","90"="#9B59B6")

pt <- 5  # point size

# ── (a) Velocity residual vs Velocity ────────────────────────────────────────
pa <- ggplot(tc_vel_res, aes(Velocity_ft, vel_resid, colour=as.factor(XZ_true))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=xz_colours) +
  labs(title="(a) Vel residual vs velocity",
       x="Velocity (ft/day)", y="Residual (ft/day)",
       colour="XZ (\u00b0)") +
  cal_theme

# ── (b) Velocity residual vs XZ angle ────────────────────────────────────────
pb <- ggplot(tc_vel_res, aes(XZ_true, vel_resid, colour=as.factor(Velocity_ft))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=vel_colours) +
  labs(title="(b) Vel residual vs XZ angle",
       x="XZ angle (\u00b0)", y="Residual (ft/day)",
       colour="Velocity (ft/day)") +
  cal_theme

# ── (c) Velocity residual vs YZ angle ────────────────────────────────────────
tc_vel_yz <- subset(tc, XZ_true==0)
tc_vel_yz$vel_resid <- tc_vel_yz$predicted_speed - tc_vel_yz$Velocity_ft
pc <- ggplot(tc_vel_yz, aes(YZ_true, vel_resid, colour=as.factor(Velocity_ft))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=vel_colours) +
  labs(title="(c) Vel residual vs YZ angle",
       x="YZ angle (\u00b0)", y="Residual (ft/day)",
       colour="Velocity (ft/day)") +
  cal_theme

# ── (d) XZ residual vs Velocity ──────────────────────────────────────────────
pd <- ggplot(tc_xz_res, aes(Velocity_ft, xz_resid, colour=as.factor(XZ_true))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=xz_colours) +
  labs(title="(d) XZ residual vs velocity",
       x="Velocity (ft/day)", y="XZ error (\u00b0)",
       colour="XZ (\u00b0)") +
  cal_theme

# ── (e) YZ residual vs Velocity ──────────────────────────────────────────────
pe <- ggplot(tc_yz_res, aes(Velocity_ft, yz_resid, colour=as.factor(YZ_true))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=yz_colours) +
  labs(title="(e) YZ residual vs velocity",
       x="Velocity (ft/day)", y="YZ error (\u00b0)",
       colour="YZ (\u00b0)") +
  cal_theme

# ── (f) YZ residual vs YZ angle ──────────────────────────────────────────────
pf <- ggplot(tc_yz_res, aes(YZ_true, yz_resid, colour=as.factor(Velocity_ft))) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(size=pt, alpha=0.9) +
  scale_colour_manual(values=vel_colours) +
  labs(title="(f) YZ residual vs YZ angle",
       x="YZ angle (\u00b0)", y="YZ error (\u00b0)",
       colour="Velocity (ft/day)") +
  cal_theme

# ── Assemble: paired layout ───────────────────────────────────────────────────
# Row 1: (a) Velocity residual vs Velocity  |  (d) XZ residual vs Velocity
#   Same x-axis (Velocity) — direct comparison of how velocity affects
#   both speed residual and XZ angle residual
# Row 2: (b) Velocity residual vs XZ angle  |  (f) YZ residual vs YZ angle
#   Both show how a specific angle (XZ or YZ) affects its own residual
# Row 3: (c) Velocity residual vs YZ angle  |  (e) YZ residual vs Velocity
#   Both involve YZ angle — cross-channel contamination check


# Use patchwork for clean white title — no black bar from textGrob/arrangeGrob
p_plot6 <- (pa | pb | pc) / (pd | pe | pf) +
  plot_annotation(
    title = "Plot 6 — Residual analysis: systematic bias detection",
    theme = theme(
      plot.title = element_text(face="bold", size=14,
                                colour="black", hjust=0.5,
                                margin=margin(b=8))
    )
  )

# Print to RStudio viewer
print(p_plot6)

# Save to output folder
ggsave(file.path(OUT_DIR,"plot6_residual_analysis.png"),
       p_plot6, width=20, height=11, dpi=600)
cat("Plot 6 saved\n")
# ── COMBO 1 ───────────────────────────────────────────────────────────────────
vel_xz_df <- data.frame(Velocity_ft=cal_vel$Velocity_ft, Signal=cal_vel$horiz_mag,
                        Channel="Horizontal Channel (XZ Plane)")
vel_yz_df <- data.frame(Velocity_ft=vert_vel$Velocity_ft, Signal=vert_vel$vert_diff,
                        Channel="Vertical Channel (YZ Plane)")
vel_combo <- bind_rows(vel_xz_df, vel_yz_df)

# Signal fit residuals for horizontal channel (K) — consistent with vertical panel
RMSE_vel_K <- sqrt(mean((cal_vel$horiz_mag - cal_vel$predicted_hm)^2))
MAE_vel_K  <- mean(abs(cal_vel$horiz_mag - cal_vel$predicted_hm))

vel_ann <- data.frame(
  Channel = c("Horizontal Channel (XZ Plane)","Vertical Channel (YZ Plane)"),
  label   = c(
    sprintf("Horiz. Magnitude = %.4f \u00d7 Velocity\n  %+.4f\nR\u00b2 = %.6f\nRMSE = %.5f K\nMAE  = %.5f K",
            vel_slope, vel_intercept, s1$r.squared, RMSE_vel_K, MAE_vel_K),
    sprintf("Vertical Signal = %.5f \u00d7 Velocity\n  %+.5f\nR\u00b2 = %.6f\nMAE = %.5f K\nRMSE = %.5f K",
            coef(lm_vv)[2], coef(lm_vv)[1], s_vv$r.squared,
            mean(abs(vert_vel$vert_diff - vert_vel$vv_pred)), rmse_vv)
  ),
  x=c(-Inf,-Inf), y=c(Inf,Inf), hjust=c(-0.05,-0.05), vjust=c(1.2,1.2)
)

p_combo_vel <- ggplot(vel_combo, aes(Velocity_ft, Signal)) +
  geom_smooth(method="lm", se=TRUE, colour="#2166ac", fill="#2166ac", alpha=0.15, linewidth=1.3) +
  geom_point(colour="#d6604d", size=5, alpha=0.9) +
  geom_text(data=vel_ann, aes(x=x, y=y, label=label, hjust=hjust, vjust=vjust),
            size=annot_size-0.3, colour=annot_col, fontface=annot_face,
            family="mono", lineheight=0.85, inherit.aes=FALSE) +
  facet_wrap(~Channel, scales="free", ncol=2) +
  labs(title="Velocity Calibration: Horizontal and Vertical Channels",
       x="True Velocity (ft/day)", y="Calibration Signal (Kelvin)") +
  cal_theme

print(p_combo_vel)
ggsave(file.path(OUT_DIR,"COMBO1_velocity_calibration_XZ_YZ.png"),
       p_combo_vel, width=14, height=6, dpi=600)
cat("COMBO1 saved\n")

# ── COMBO 2 ───────────────────────────────────────────────────────────────────
ang_xz <- cal_xz %>% transmute(true_angle=XZ_true, calc_angle=calc_XZ,
                               Velocity_ft=Velocity_ft, Channel="XZ Horizontal Angle")
ang_yz <- cal_yz %>% transmute(true_angle=YZ_true, calc_angle=calc_YZ,
                               Velocity_ft=Velocity_ft, Channel="YZ Vertical Angle")
ang_combo <- bind_rows(ang_xz, ang_yz)

lm_xz_c <- lm(calc_angle ~ true_angle, data=ang_combo[ang_combo$Channel=="XZ Horizontal Angle",])
lm_yz_c <- lm(calc_angle ~ true_angle, data=ang_combo[ang_combo$Channel=="YZ Vertical Angle",])

ang_ann <- data.frame(
  Channel = c("XZ Horizontal Angle","YZ Vertical Angle"),
  label   = c(
    sprintf("Calculated XZ = %.4f \u00d7 True XZ\n  %+.4f\nR\u00b2 = %.6f\nMAE = %.4f\u00b0\nRMSE = %.4f\u00b0",
            coef(lm_xz_c)[2], coef(lm_xz_c)[1], s_xz$r.squared,
            mean(abs(cal_xz$XZ_err)), sqrt(mean(cal_xz$XZ_err^2))),
    sprintf("Calculated YZ = %.4f \u00d7 True YZ\n  %+.4f\nR\u00b2 = %.6f\nMAE = %.4f\u00b0\nRMSE = %.4f\u00b0",
            coef(lm_yz_c)[2], coef(lm_yz_c)[1], s_yz$r.squared,
            mean(abs(cal_yz$YZ_err)), sqrt(mean(cal_yz$YZ_err^2)))
  ),
  x=c(-Inf,-Inf), y=c(-10,-10), hjust=c(-0.05,-0.05), vjust=c(1,1)
)

pred_xz_c <- data.frame(true_angle=seq(0,60,length.out=100), Channel="XZ Horizontal Angle")
pred_yz_c <- data.frame(true_angle=seq(0,90,length.out=100), Channel="YZ Vertical Angle")
pred_xz_c$calc_angle <- predict(lm_xz_c, newdata=data.frame(true_angle=pred_xz_c$true_angle))
pred_yz_c$calc_angle <- predict(lm_yz_c, newdata=data.frame(true_angle=pred_yz_c$true_angle))
ci_xz <- predict(lm_xz_c, newdata=data.frame(true_angle=pred_xz_c$true_angle), interval="confidence")
ci_yz <- predict(lm_yz_c, newdata=data.frame(true_angle=pred_yz_c$true_angle), interval="confidence")
pred_xz_c$lwr <- ci_xz[,"lwr"]; pred_xz_c$upr <- ci_xz[,"upr"]
pred_yz_c$lwr <- ci_yz[,"lwr"]; pred_yz_c$upr <- ci_yz[,"upr"]
pred_ang_combo <- bind_rows(pred_xz_c, pred_yz_c)

p_combo_ang <- ggplot(ang_combo, aes(true_angle, calc_angle, colour=as.factor(Velocity_ft))) +
  geom_ribbon(data=pred_ang_combo, aes(x=true_angle, ymin=lwr, ymax=upr),
              inherit.aes=FALSE, fill="#2166ac", alpha=0.12) +
  geom_abline(slope=1, intercept=0, linetype="dashed", colour="black", linewidth=1, alpha=0.6) +
  geom_line(data=pred_ang_combo, aes(true_angle, calc_angle),
            colour="#2166ac", linewidth=1.2, inherit.aes=FALSE) +
  geom_point(size=5, alpha=0.9) +
  geom_text(data=ang_ann, aes(x=x, y=y, label=label, hjust=hjust, vjust=vjust),
            size=annot_size-0.3, colour=annot_col, fontface=annot_face,
            family="mono", lineheight=0.85, inherit.aes=FALSE) +
  facet_wrap(~Channel, scales="fixed", ncol=2) +
  coord_cartesian(ylim=c(-30,98)) +
  scale_colour_viridis_d(option="D", end=0.82) +
  scale_x_continuous(breaks=c(0,15,30,45,60,90)) +
  scale_y_continuous(breaks=c(0,15,30,45,60,90)) +
  labs(title="Angle Recovery: XZ Horizontal and YZ Vertical Channels",
       x="True Angle (degrees)", y="Calculated Angle (degrees)",
       colour="Velocity (ft/day)") +
  cal_theme

print(p_combo_ang)
ggsave(file.path(OUT_DIR,"COMBO2_angle_recovery_XZ_YZ.png"),
       p_combo_ang, width=14, height=7, dpi=600)
cat("COMBO2 saved\n")

# ── COMBO 3 ───────────────────────────────────────────────────────────────────
err_xz <- xz_summary %>%
  select(XZ_true, bias, mean_abs_error, root_mean_sq_error) %>%
  pivot_longer(-XZ_true, names_to="Metric", values_to="Value") %>%
  mutate(Angle=XZ_true, Channel="XZ Horizontal Angle",
         Metric=recode(Metric, bias="Bias", mean_abs_error="MAE", root_mean_sq_error="RMSE"))

err_yz <- yz_summary %>%
  select(YZ_true, bias, mean_abs_error, root_mean_sq_error) %>%
  pivot_longer(-YZ_true, names_to="Metric", values_to="Value") %>%
  mutate(Angle=YZ_true, Channel="YZ Vertical Angle",
         Metric=recode(Metric, bias="Bias", mean_abs_error="MAE", root_mean_sq_error="RMSE"))

err_combo <- bind_rows(err_xz, err_yz)

err_stats <- data.frame(
  Channel = c("XZ Horizontal Angle","YZ Vertical Angle"),
  label   = c(
    sprintf("MAE = %.3f\u00b0\nRMSE = %.3f\u00b0", mean(abs(cal_xz$XZ_err)), sqrt(mean(cal_xz$XZ_err^2))),
    sprintf("MAE = %.3f\u00b0\nRMSE = %.3f\u00b0", mean(abs(cal_yz$YZ_err)), sqrt(mean(cal_yz$YZ_err^2)))
  )
)

p_combo_err_bars <- ggplot(err_combo, aes(as.factor(Angle), Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_text(data=err_stats, aes(x=Inf, y=Inf, label=label),
            hjust=1.05, vjust=1.3, inherit.aes=FALSE,
            size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837")) +
  facet_wrap(~Channel, scales="fixed", ncol=2) +
  labs(title="Angle Error Metrics for XZ Plane and YZ Plane",
       x="True Angle (degrees)", y="Error (degrees)", fill="Error Metric") +
  cal_theme

print(p_combo_err_bars)
ggsave(file.path(OUT_DIR,"COMBO3a_angle_error_metrics_XZ_YZ.png"),
       p_combo_err_bars, width=14, height=6, dpi=600)
cat("COMBO3a saved\n")

p_cal1b_combo <- ggplot(cal_vel, aes(Velocity_ft, residual_vel, colour=XZ_fac)) +
  geom_hline(yintercept= 0,        linetype="dashed", linewidth=1) +
  geom_hline(yintercept= RMSE_vel, linetype="dotted", colour="grey50") +
  geom_hline(yintercept=-RMSE_vel, linetype="dotted", colour="grey50") +
  geom_point(size=5, alpha=0.9) +
  geom_smooth(aes(group=1), method="loess", se=FALSE, colour="red", linewidth=0.9) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.3,
           label=sprintf("RMSE = %.4f ft/day\nMAE  = %.4f ft/day\nDotted = \u00b1RMSE", RMSE_vel, MAE_vel),
           size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  scale_colour_brewer(palette="Dark2") +
  labs(title="Velocity Calibration Residuals",
       x="True Velocity (ft/day)", y="Residual: True minus Predicted Velocity (ft/day)",
       colour="XZ Angle (degrees)") +
  cal_theme

p_err_combo <- ggplot(err_combo, aes(as.factor(Angle), Value, fill=Metric)) +
  geom_col(position="dodge", width=0.7) +
  geom_hline(yintercept=0, linetype="dashed") +
  geom_text(data=err_stats, aes(x=Inf, y=Inf, label=label),
            hjust=1.05, vjust=1.3, inherit.aes=FALSE,
            size=annot_size, colour=annot_col, fontface=annot_face, family="mono", lineheight=0.85) +
  scale_fill_manual(values=c("RMSE"="#d6604d","MAE"="#4393c3","Bias"="#1b7837")) +
  facet_wrap(~Channel, scales="fixed", ncol=2) +
  labs(title="Angle Error Metrics for XZ Plane and YZ Plane",
       x="True Angle (degrees)", y="Error (degrees)", fill="Error Metric") +
  cal_theme

print(p_cal1b_combo)
print(p_err_combo)

grob_top    <- ggplotGrob(p_cal1b_combo)
grob_bottom <- ggplotGrob(p_err_combo)
p_combo_all_err <- gridExtra::arrangeGrob(grob_top, grob_bottom, ncol=1, heights=c(1,1))

# Print to RStudio viewer
grid::grid.newpage()
grid::grid.draw(p_combo_all_err)

ggsave(file.path(OUT_DIR,"COMBO3b_all_error_panels.png"),
       p_combo_all_err, width=14, height=12, dpi=600)
cat("COMBO3b saved\n")

# =============================================================================
# SAVE OUTPUT FILES
# =============================================================================
coeff_df <- data.frame(
  Parameter = c(
    "Velocity slope (Horizontal Magnitude per ft/day)",
    "Velocity intercept",
    "Velocity R squared",
    "Velocity Root Mean Square Error (ft/day)",
    "Velocity Mean Absolute Error (ft/day)",
    "XZ Angle R squared",
    "XZ Angle Mean Absolute Error (degrees)",
    "XZ Angle Root Mean Square Error (degrees)",
    "Vertical scale factor",
    "YZ Angle R squared",
    "YZ Angle Mean Absolute Error (degrees)",
    "YZ Angle Root Mean Square Error (degrees)"
  ),
  Value = c(
    vel_slope, vel_intercept,
    s1$r.squared, RMSE_vel, MAE_vel,
    s_xz$r.squared, mean(abs(cal_xz$XZ_err)), sqrt(mean(cal_xz$XZ_err^2)),
    VERT_SCALE, s_yz$r.squared,
    mean(abs(cal_yz$YZ_err)), sqrt(mean(cal_yz$YZ_err^2))
  )
)

write.csv(coeff_df,   file.path(OUT_DIR,"calibration_coefficients.csv"),  row.names=FALSE)
write.csv(xz_summary, file.path(OUT_DIR,"XZ_angle_error_summary.csv"),    row.names=FALSE)
write.csv(yz_summary, file.path(OUT_DIR,"YZ_angle_error_summary.csv"),    row.names=FALSE)
write.csv(tc,         file.path(OUT_DIR,"features_extracted.csv"),         row.names=FALSE)

cat("\n============================================================\n")
cat("ALL DONE — outputs saved to:", OUT_DIR, "\n")
cat("============================================================\n")