library(dplyr)
library(doBy)
library(ggplot2)

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

simout <- read.csv('/Users/jdh/Downloads/merged_all_experiments.csv')



names(simout)[1:6] <- c("Time","Type","Velocity_ft","XZ_Angle","YZ_Angle","Cycle")
simout<-subset(simout,!(YZ_Angle==30&Velocity_ft==15))

# ---- Middle ring opposing pairs (XZ / horizontal plane) ----
simout$A2D2 <- simout$A2 - simout$D2
simout$B2E2 <- simout$B2 - simout$E2
simout$C2F2 <- simout$C2 - simout$F2

# ---- Same-position upper-lower pairs (CLEAN vertical signal) ----
simout$A1A3 <- simout$A1 - simout$A3
simout$B1B3 <- simout$B1 - simout$B3
simout$C1C3 <- simout$C1 - simout$C3
simout$D1D3 <- simout$D1 - simout$D3
simout$E1E3 <- simout$E1 - simout$E3
simout$F1F3 <- simout$F1 - simout$F3

# ---- Ring averages (cleanest vertical metric) ----
simout$mean_ring1 <- rowMeans(simout[, c("A1","B1","C1","D1","E1","F1")])
simout$mean_ring3 <- rowMeans(simout[, c("A3","B3","C3","D3","E3","F3")])
simout$ring1_minus_ring3 <- simout$mean_ring1 - simout$mean_ring3

simout$XZ_Angle_fac  <- as.factor(simout$XZ_Angle)
simout$YZ_Angle_fac  <- as.factor(simout$YZ_Angle)
simout$Velocity_ft_fac <- as.factor(simout$Velocity_ft)


# =============================================================================
# Extract timediff features (t=480 minus t=1)
# =============================================================================

timings_compiled <- data.frame()
XZ_angles  <- unique(na.omit(simout$XZ_Angle_fac))
YZ_angles  <- unique(na.omit(simout$YZ_Angle_fac))
Velocity_ft <- unique(simout$Velocity_ft_fac)

for (i in XZ_angles) {
  for (j in Velocity_ft) {
    for (k in YZ_angles) {
      simout_sub <- subset(simout, XZ_Angle_fac == i & Velocity_ft_fac == j & YZ_Angle_fac == k)
      if (nrow(simout_sub) > 20) {

        # XZ plane (horizontal) — unchanged from your working code
        A2D2_difference_1   <- simout_sub$A2D2[simout_sub$Time == 1]
        A2D2_difference_480 <- simout_sub$A2D2[simout_sub$Time == 480]
        B2E2_difference_1   <- simout_sub$B2E2[simout_sub$Time == 1]
        B2E2_difference_480 <- simout_sub$B2E2[simout_sub$Time == 480]
        C2F2_difference_1   <- simout_sub$C2F2[simout_sub$Time == 1]
        C2F2_difference_480 <- simout_sub$C2F2[simout_sub$Time == 480]

        # YZ plane (vertical) — CORRECTED: same-position pairs
        A1A3_difference_1   <- simout_sub$A1A3[simout_sub$Time == 1]
        A1A3_difference_480 <- simout_sub$A1A3[simout_sub$Time == 480]
        B1B3_difference_1   <- simout_sub$B1B3[simout_sub$Time == 1]
        B1B3_difference_480 <- simout_sub$B1B3[simout_sub$Time == 480]
        C1C3_difference_1   <- simout_sub$C1C3[simout_sub$Time == 1]
        C1C3_difference_480 <- simout_sub$C1C3[simout_sub$Time == 480]
        D1D3_difference_1   <- simout_sub$D1D3[simout_sub$Time == 1]
        D1D3_difference_480 <- simout_sub$D1D3[simout_sub$Time == 480]
        E1E3_difference_1   <- simout_sub$E1E3[simout_sub$Time == 1]
        E1E3_difference_480 <- simout_sub$E1E3[simout_sub$Time == 480]
        F1F3_difference_1   <- simout_sub$F1F3[simout_sub$Time == 1]
        F1F3_difference_480 <- simout_sub$F1F3[simout_sub$Time == 480]

        # Ring average vertical
        ring_vert_1   <- simout_sub$ring1_minus_ring3[simout_sub$Time == 1]
        ring_vert_480 <- simout_sub$ring1_minus_ring3[simout_sub$Time == 480]

        timings_compiled <- bind_rows(timings_compiled, data.frame(
          XZ_angles = i, YZ_angles = k, Velocity_ft = j,
          A2D2_difference_1, A2D2_difference_480,
          B2E2_difference_1, B2E2_difference_480,
          C2F2_difference_1, C2F2_difference_480,
          A1A3_difference_1, A1A3_difference_480,
          B1B3_difference_1, B1B3_difference_480,
          C1C3_difference_1, C1C3_difference_480,
          D1D3_difference_1, D1D3_difference_480,
          E1E3_difference_1, E1E3_difference_480,
          F1F3_difference_1, F1F3_difference_480,
          ring_vert_1, ring_vert_480
        ))
      }
    }
  }
}


# =============================================================================
# Compute timediffs (t=480 - t=1)
# =============================================================================

# Horizontal
timings_compiled$A2D2_timediff <- timings_compiled$A2D2_difference_480 - timings_compiled$A2D2_difference_1
timings_compiled$B2E2_timediff <- timings_compiled$B2E2_difference_480 - timings_compiled$B2E2_difference_1
timings_compiled$C2F2_timediff <- timings_compiled$C2F2_difference_480 - timings_compiled$C2F2_difference_1

# Vertical (same-position pairs)
timings_compiled$A1A3_timediff <- timings_compiled$A1A3_difference_480 - timings_compiled$A1A3_difference_1
timings_compiled$B1B3_timediff <- timings_compiled$B1B3_difference_480 - timings_compiled$B1B3_difference_1
timings_compiled$C1C3_timediff <- timings_compiled$C1C3_difference_480 - timings_compiled$C1C3_difference_1
timings_compiled$D1D3_timediff <- timings_compiled$D1D3_difference_480 - timings_compiled$D1D3_difference_1
timings_compiled$E1E3_timediff <- timings_compiled$E1E3_difference_480 - timings_compiled$E1E3_difference_1
timings_compiled$F1F3_timediff <- timings_compiled$F1F3_difference_480 - timings_compiled$F1F3_difference_1

# Ring average vertical timediff (CLEANEST metric)
timings_compiled$ring_vert_timediff <- timings_compiled$ring_vert_480 - timings_compiled$ring_vert_1

# 6-pair average vertical (equivalent to ring average)
timings_compiled$vert_6pair_avg <- rowMeans(timings_compiled[, c(
  "A1A3_timediff","B1B3_timediff","C1C3_timediff",
  "D1D3_timediff","E1E3_timediff","F1F3_timediff")])


# =============================================================================
# XZ (horizontal) angle calculation — YOUR WORKING METHOD, unchanged
# =============================================================================

A_D_Angle <- 30 * pi / 180
B_E_Angle <- 90 * pi / 180
F_C_Angle <- 150 * pi / 180

timings_compiled$A_D_X <- timings_compiled$A2D2_timediff * sin(A_D_Angle)
timings_compiled$A_D_Z <- timings_compiled$A2D2_timediff * cos(A_D_Angle)
timings_compiled$B_E_X <- timings_compiled$B2E2_timediff * sin(B_E_Angle)
timings_compiled$B_E_Z <- timings_compiled$B2E2_timediff * cos(B_E_Angle)
timings_compiled$C_F_X <- timings_compiled$C2F2_timediff * sin(F_C_Angle)
timings_compiled$C_F_Z <- timings_compiled$C2F2_timediff * cos(F_C_Angle)

timings_compiled$total_x <- timings_compiled$A_D_X + timings_compiled$B_E_X + timings_compiled$C_F_X
timings_compiled$total_z <- timings_compiled$A_D_Z + timings_compiled$B_E_Z + timings_compiled$C_F_Z
timings_compiled$horiz_mag <- sqrt(timings_compiled$total_x^2 + timings_compiled$total_z^2)

timings_compiled$calc_XZ_angle <- atan2(timings_compiled$total_x, timings_compiled$total_z) * 180 / pi


# =============================================================================
# YZ (vertical) angle calculation — CORRECTED
# =============================================================================

# The vertical signal is simply the ring-average timediff.
# No sin(angle) projection is needed here because the rings are directly
# above/below the heater — the measurement axis IS the vertical axis.
timings_compiled$total_y <- timings_compiled$ring_vert_timediff

# For 3D magnitude, you need a calibration factor to put horizontal and
# vertical signals on the same scale. The horizontal signal comes from 
# sensors ~3.3 cm from the heater (in the same plane), while the vertical 
# signal comes from sensors ~4 cm above/below. The heat transport geometry 
# differs, so a scaling factor is needed.
#
# Approach: at a known velocity, the ratio of horiz_mag (at YZ=0) to 
# vert_signal (at YZ=90, XZ=0) gives the relative sensitivity.
# Then: total_mag = sqrt(horiz_mag^2 + (vert * scale_factor)^2)

# Estimate calibration from the simulation data:
# At V=5, YZ=0:  horiz_mag should represent full velocity
# At V=5, YZ=90: vert_signal should represent full velocity
horiz_ref <- timings_compiled$horiz_mag[timings_compiled$YZ_angles == 0 & 
                                         timings_compiled$XZ_angles == 0 & 
                                         timings_compiled$Velocity_ft == 5]
vert_ref  <- timings_compiled$total_y[timings_compiled$YZ_angles == 90 & 
                                       timings_compiled$XZ_angles == 0 & 
                                       timings_compiled$Velocity_ft == 5]

if (length(horiz_ref) > 0 & length(vert_ref) > 0) {
  vert_scale <- horiz_ref[1] / vert_ref[1]
  cat("Vertical calibration scale factor:", vert_scale, "\n")
  cat("  (horizontal signal at V=5, XZ=0, YZ=0):", horiz_ref[1], "\n")
  cat("  (vertical signal at V=5, XZ=0, YZ=90):", vert_ref[1], "\n")
} else {
  vert_scale <- 1  # fallback
}

timings_compiled$total_y_scaled <- timings_compiled$total_y * vert_scale
timings_compiled$total_3d_mag <- sqrt(timings_compiled$horiz_mag^2 + 
                                       timings_compiled$total_y_scaled^2)

# Recover YZ angle
timings_compiled$calc_YZ_angle <- atan2(timings_compiled$total_y_scaled, 
                                         timings_compiled$horiz_mag) * 180 / pi


# =============================================================================
# Diagnostic plots
# =============================================================================

# 1. Horizontal magnitude vs velocity (your working method)
p1 <- ggplot(subset(timings_compiled, YZ_angles == 0), 
             aes(as.numeric(Velocity_ft), horiz_mag, color = XZ_angles)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "XZ (Horizontal) Magnitude vs Velocity",
       subtitle = "Your existing method — works correctly",
       x = "Velocity (ft/day)", y = "Horizontal Magnitude")
print(p1)

# 2. Vertical signal vs YZ angle (CORRECTED ring average)
p2 <- ggplot(subset(timings_compiled, XZ_angles == 0), 
             aes(as.numeric(YZ_angles), ring_vert_timediff, 
                 color = Velocity_ft, group = Velocity_ft)) +
  geom_point(size = 4) +
  geom_line() +
  labs(title = "Vertical Signal vs YZ Angle (CORRECTED)",
       subtitle = "Ring-average method: mean(Ring1) - mean(Ring3)",
       x = "YZ Angle (degrees)", y = "Vertical Signal (ring avg timediff)")
print(p2)

# 3. Vertical signal is near-zero for purely horizontal flow
p3 <- ggplot(subset(timings_compiled, YZ_angles == 0), 
             aes(as.numeric(XZ_angles), ring_vert_timediff, 
                 color = Velocity_ft, group = Velocity_ft)) +
  geom_point(size = 4) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "Vertical Signal for Horizontal-Only Flow (YZ=0°)",
       subtitle = "Should be ~0 regardless of XZ angle — confirms no contamination",
       x = "XZ Angle (degrees)", y = "Vertical Signal (ring avg timediff)")
print(p3)

# 4. 3D magnitude vs velocity
p4 <- ggplot(timings_compiled, 
             aes(as.numeric(Velocity_ft), total_3d_mag, color = YZ_angles)) +
  geom_point(size = 4) +
  labs(title = "Total 3D Magnitude vs Velocity",
       subtitle = "Combined horizontal + corrected vertical",
       x = "Velocity (ft/day)", y = "3D Magnitude")
print(p4)

# 5. Recovered YZ angle vs actual
p5 <- ggplot(subset(timings_compiled, XZ_angles == 0 & as.numeric(Velocity_ft) >= 1),
             aes(as.numeric(YZ_angles), calc_YZ_angle, 
                 color = Velocity_ft, group = Velocity_ft)) +
  geom_point(size = 4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  labs(title = "Recovered vs Actual YZ Angle",
       subtitle = "Points should fall on the 1:1 dashed line",
       x = "Actual YZ Angle (degrees)", y = "Recovered YZ Angle (degrees)") +
  coord_equal(xlim = c(-5, 95), ylim = c(-5, 95))
print(p5)
