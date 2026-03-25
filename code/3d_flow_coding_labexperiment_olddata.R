library(dplyr)
library(doBy)
library(ggplot2)
library(reshape2)
library(lubridate)
library(stringr)



#function that labels each heating cycle numerically.
add_cycle_column <- function(df, var_name) {
  # Create a lagged version to detect transitions
  df$lag_var <- c(NA, head(df[[var_name]], -1))
  
  # Detect start of new cycles (0 -> 1 transition)
  df$cycle_start <- ifelse(is.na(df$lag_var), 
                          df[[var_name]] == 1,  # First row: start cycle if it's 1
                          df$lag_var == 0 & df[[var_name]] == 1)
  
  # Create cycle numbers using cumulative sum
  df$cycle <- cumsum(df$cycle_start)
  
  # If the first value is 0, it belongs to cycle 0 (pre-first cycle)
  if (!is.na(df[[var_name]][1]) && df[[var_name]][1] == 0) {
    df$cycle <- df$cycle
  }
  
  
  # Clean up temporary columns
  df$lag_var <- NULL
  df$cycle_start <- NULL
  
  return(df)
}





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

simout0 <- read.csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Version 1 Flow Chamber Tests/Lab_analysis/Lab_data/Master_Lab_AllRuns.csv')


#names(simout0)[5:10]<-c("A2","B2","C2","D2","E2","F2")


#simout0$time.s<-as.numeric(simout0$time.s)
simout0$A2<-as.numeric(simout0$RING01_TrawC)
simout0$B2<-as.numeric(simout0$RING01_TrawD)
simout0$C2<-as.numeric(simout0$RING01_TrawA)
simout0$D2<-as.numeric(simout0$RING01_TrawB)
simout0$E2<-as.numeric(simout0$RING01_TrawF)
simout0$F2<-as.numeric(simout0$RING01_TrawE)
simout0$time.s<-as.numeric(simout0$time.s)


simout0<-subset(simout0,!is.na(HEATER_heater))

simout<- add_cycle_column(simout0,"HEATER_heater")
simout$dtp<-as_datetime(as.integer(simout$time.s))


simout$time.s<-as.numeric(simout$time.s)
simout_cycles<-unique(simout$cycle)

simout_c<-data.frame()


for(i in simout_cycles){
	simout_temp<-subset(simout,cycle==i)
	starttime<-min(simout_temp$time.s)
	simout_temp$cycle_time<-simout_temp$time.s-starttime
	simout_c<-bind_rows(simout_c,simout_temp)
}

unique(simout_c$Velocity)

simout_v1<-subset(simout_c,Velocity==74.6)


ggplot(simout_v1,aes(x=cycle_time,y=B2,color=cycle))+
geom_point()


ggplot(simout_v1,aes(x=dtp,y=B2,color=cycle))+
geom_point()



simout_v2<-subset(simout_c,Velocity==86.6)


ggplot(simout_v2,aes(x=cycle_time,y=B2,color=cycle))+
geom_point()


ggplot(simout_v2,aes(x=dtp,y=B2,color=cycle))+
geom_point()





ggplot(simout[50000:90000,],aes(x=dtp,y=F2,color=as.factor(Velocity)))+
geom_point()




#simout[48140:48163,]


#names(simout_c)[1:6] <- c("time.s","Type","Velocity_ft","XZ_Angle","YZ_Angle","Cycle")
#simout_c<-subset(simout_c,!(YZ_Angle==30&Velocity_ft==15))

# ---- Middle ring opposing pairs (XZ / horizontal plane) ----
simout_c$A2D2 <- simout_c$A2 - simout_c$D2
simout_c$B2E2 <- simout_c$B2 - simout_c$E2
simout_c$C2F2 <- simout_c$C2 - simout_c$F2


proc_out<-data.frame()
for(i in simout_cycles){
	simout_temp<-subset(simout_c,cycle==i)
	max_A2D2<-max(simout_temp$A2D2)
	max_B2E2<-max(simout_temp$B2E2)	
	max_C2F2<-max(simout_temp$C2F2)
	vel<-unique(simout_temp$Velocity)
	temp_comp<-data.frame(max_A2D2=max_A2D2,max_B2E2=max_B2E2,max_C2F2=max_C2F2,cycle=i,vel=vel)
	proc_out<-bind_rows(proc_out,temp_comp)
}




A_D_Angle <- 30 * pi / 180
B_E_Angle <- 90 * pi / 180
F_C_Angle <- 150 * pi / 180

proc_out$A_D_X <- proc_out$max_A2D2 * sin(A_D_Angle)
proc_out$A_D_Z <- proc_out$max_A2D2 * cos(A_D_Angle)
proc_out$B_E_X <- proc_out$max_B2E2 * sin(B_E_Angle)
proc_out$B_E_Z <- proc_out$max_B2E2 * cos(B_E_Angle)
proc_out$C_F_X <- proc_out$max_C2F2 * sin(F_C_Angle)
proc_out$C_F_Z <- proc_out$max_C2F2 * cos(F_C_Angle)

proc_out$total_x <- proc_out$A_D_X + proc_out$B_E_X + proc_out$C_F_X
proc_out$total_z <- proc_out$A_D_Z + proc_out$B_E_Z + proc_out$C_F_Z
proc_out$horiz_mag <- sqrt(proc_out$total_x^2 + proc_out$total_z^2)

proc_out$calc_XZ_angle <- atan2(proc_out$total_x, proc_out$total_z) * 180 / pi




ggplot(proc_out,aes(cycle,horiz_mag,color=vel))+
geom_point()


ggplot(proc_out,aes(vel,horiz_mag,color=cycle))+
geom_point()



ggplot(simout_c,aes(dtp,C2F2,color=Velocity))+
geom_point()




# ---- Same-position upper-lower pairs (CLEAN vertical signal) ----
simout_c$A1A3 <- simout_c$A1 - simout_c$A3
simout_c$B1B3 <- simout_c$B1 - simout_c$B3
simout_c$C1C3 <- simout_c$C1 - simout_c$C3
simout_c$D1D3 <- simout_c$D1 - simout_c$D3
simout_c$E1E3 <- simout_c$E1 - simout_c$E3
simout_c$F1F3 <- simout_c$F1 - simout_c$F3

# ---- Ring averages (cleanest vertical metric) ----
simout_c$mean_ring1 <- rowMeans(simout_c[, c("A1","B1","C1","D1","E1","F1")])
simout_c$mean_ring3 <- rowMeans(simout_c[, c("A3","B3","C3","D3","E3","F3")])
simout_c$ring1_minus_ring3 <- simout_c$mean_ring1 - simout_c$mean_ring3

simout_c$XZ_Angle_fac  <- as.factor(simout_c$XZ_Angle)
simout_c$YZ_Angle_fac  <- as.factor(simout_c$YZ_Angle)
simout_c$Velocity_ft_fac <- as.factor(simout_c$Velocity_ft)


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
        A2D2_difference_1   <- simout_sub$A2D2[simout_sub$time.s == 1]
        A2D2_difference_480 <- simout_sub$A2D2[simout_sub$time.s == 480]
        B2E2_difference_1   <- simout_sub$B2E2[simout_sub$time.s == 1]
        B2E2_difference_480 <- simout_sub$B2E2[simout_sub$time.s == 480]
        C2F2_difference_1   <- simout_sub$C2F2[simout_sub$time.s == 1]
        C2F2_difference_480 <- simout_sub$C2F2[simout_sub$time.s == 480]

        # YZ plane (vertical) — CORRECTED: same-position pairs
        A1A3_difference_1   <- simout_sub$A1A3[simout_sub$time.s == 1]
        A1A3_difference_480 <- simout_sub$A1A3[simout_sub$time.s == 480]
        B1B3_difference_1   <- simout_sub$B1B3[simout_sub$time.s == 1]
        B1B3_difference_480 <- simout_sub$B1B3[simout_sub$time.s == 480]
        C1C3_difference_1   <- simout_sub$C1C3[simout_sub$time.s == 1]
        C1C3_difference_480 <- simout_sub$C1C3[simout_sub$time.s == 480]
        D1D3_difference_1   <- simout_sub$D1D3[simout_sub$time.s == 1]
        D1D3_difference_480 <- simout_sub$D1D3[simout_sub$time.s == 480]
        E1E3_difference_1   <- simout_sub$E1E3[simout_sub$time.s == 1]
        E1E3_difference_480 <- simout_sub$E1E3[simout_sub$time.s == 480]
        F1F3_difference_1   <- simout_sub$F1F3[simout_sub$time.s == 1]
        F1F3_difference_480 <- simout_sub$F1F3[simout_sub$time.s == 480]

        # Ring average vertical
        ring_vert_1   <- simout_sub$ring1_minus_ring3[simout_sub$time.s == 1]
        ring_vert_480 <- simout_sub$ring1_minus_ring3[simout_sub$time.s == 480]

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
