# =============================================================================
# FLOW MAGNITUDE AND DIRECTION CALCULATION
# Insert this code after your cycle loop that builds flow_data_compiled
# (i.e., after temp_A_600 through temp_F_600 are computed)
#
# Requires: flow_data_compiled with columns temp_A_600..temp_F_600 
#           and temp_A_0..temp_F_0 already computed from the cycle loop.
# =============================================================================


# ---- STEP 1: Compute the directional signal from opposing sensor pairs ----
# 
# Sensors A-F are spaced 60° apart around the ring. Opposing pairs 
# (A vs D, B vs E, C vs F) measure the temperature asymmetry caused by flow.
# The difference (upstream - downstream) is positive when flow pushes heat
# toward the downstream sensor.
#
# We use the temperature at t=600s (baseline-corrected) for each sensor.
# Using the value at a fixed time rather than the peak avoids timing noise.

flow_data_compiled$AD_diff <- flow_data_compiled$temp_A_600 - flow_data_compiled$temp_D_600
flow_data_compiled$BE_diff <- flow_data_compiled$temp_B_600 - flow_data_compiled$temp_E_600
flow_data_compiled$CF_diff <- flow_data_compiled$temp_C_600 - flow_data_compiled$temp_F_600


# ---- STEP 2: Project opposing-pair differences onto X and Z axes ----
#
# Each pair has a known angular orientation. These are the physical angles
# of the A-D, B-E, and C-F axes in the sensor ring coordinate system.
#
# NOTE: There is a known -60° offset between the ANSYS simulation coordinate
# system and the physical sensor positions. If your recovered angles are 
# consistently off by ~60°, adjust these three angles accordingly 
# (e.g., change to -30°, 30°, 90° or 90°, 150°, 210°).

A_D_angle_rad <- 30 * pi / 180   # 30°
B_E_angle_rad <- 90 * pi / 180   # 90°  
C_F_angle_rad <- 150 * pi / 180  # 150°

# Decompose each pair's signal into X and Z components
flow_data_compiled$AD_x <- flow_data_compiled$AD_diff * sin(A_D_angle_rad)
flow_data_compiled$AD_z <- flow_data_compiled$AD_diff * cos(A_D_angle_rad)

flow_data_compiled$BE_x <- flow_data_compiled$BE_diff * sin(B_E_angle_rad)
flow_data_compiled$BE_z <- flow_data_compiled$BE_diff * cos(B_E_angle_rad)

flow_data_compiled$CF_x <- flow_data_compiled$CF_diff * sin(C_F_angle_rad)
flow_data_compiled$CF_z <- flow_data_compiled$CF_diff * cos(C_F_angle_rad)

# Sum components to get the net flow direction vector
flow_data_compiled$vector_x <- flow_data_compiled$AD_x + flow_data_compiled$BE_x + flow_data_compiled$CF_x
flow_data_compiled$vector_z <- flow_data_compiled$AD_z + flow_data_compiled$BE_z + flow_data_compiled$CF_z

# Horizontal direction angle (degrees, 0-360)
flow_data_compiled$flow_direction_deg <- (atan2(flow_data_compiled$vector_x, 
                                                 flow_data_compiled$vector_z) * 180 / pi) %% 360

# Directional signal magnitude (proportional to flow speed, but NOT calibrated)
flow_data_compiled$directional_mag <- sqrt(flow_data_compiled$vector_x^2 + 
                                            flow_data_compiled$vector_z^2)


# ---- STEP 3: Compute calibrated flow magnitude (speed) ----
#
# The mean temperature across all 6 sensors at t=600s is inversely related
# to flow speed: faster flow carries more heat away, leaving lower temperatures.
# This is independent of flow direction (it averages over all sensor positions).
#
# Calibration was derived from quadratic fit to lab data at 
# 80, 99.3, 126.7, and 161.3 ml/min (R² = 0.98, LOO MAE = 2.9 ml/min).

flow_data_compiled$mean_t600 <- rowMeans(
  flow_data_compiled[, c("temp_A_600", "temp_B_600", "temp_C_600",
                          "temp_D_600", "temp_E_600", "temp_F_600")]
)

# Quadratic calibration: flow = a * T^2 + b * T + c
# where T = mean_t600 (mean baseline-corrected temperature at t=600s)
cal_a <- 278.95
cal_b <- -487.99
cal_c <- 291.44

flow_data_compiled$predicted_flow_ml_min <- cal_a * flow_data_compiled$mean_t600^2 + 
                                             cal_b * flow_data_compiled$mean_t600 + 
                                             cal_c


# ---- STEP 4: Summary output ----

cat("\n========== FLOW ANALYSIS RESULTS ==========\n\n")

result_summary <- flow_data_compiled[, c("cycle", "flow", "mean_t600", 
                                          "predicted_flow_ml_min", 
                                          "flow_direction_deg", 
                                          "directional_mag")]
names(result_summary) <- c("Cycle", "Actual_flow", "Mean_T600", 
                            "Predicted_flow", "Direction_deg", "Dir_magnitude")

# Round for readability
result_summary$Mean_T600 <- round(result_summary$Mean_T600, 4)
result_summary$Predicted_flow <- round(result_summary$Predicted_flow, 1)
result_summary$Direction_deg <- round(result_summary$Direction_deg, 1)
result_summary$Dir_magnitude <- round(result_summary$Dir_magnitude, 4)

print(result_summary, row.names = FALSE)

cat("\n\n--- Prediction accuracy by flow rate ---\n")
accuracy <- aggregate(
  cbind(Predicted_flow = predicted_flow_ml_min, 
        Actual = flow, 
        Direction = flow_direction_deg) ~ flow,
  data = flow_data_compiled,
  FUN = function(x) round(mean(x), 1)
)
accuracy$Error_ml_min <- round(accuracy$Predicted_flow - accuracy$Actual, 1)
print(accuracy, row.names = FALSE)


# ---- STEP 5: Diagnostic plots ----

# Plot 1: Predicted vs actual flow rate
p1 <- ggplot(flow_data_compiled, aes(x = flow, y = predicted_flow_ml_min)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = as.factor(flow)), size = 3) +
  labs(
    x = "Actual flow (ml/min)",
    y = "Predicted flow (ml/min)",
    title = "Flow speed prediction: predicted vs actual",
    color = "Flow rate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1)

# Plot 2: Calibration curve (mean_t600 vs flow)
t600_seq <- seq(min(flow_data_compiled$mean_t600, na.rm = TRUE) - 0.05,
                max(flow_data_compiled$mean_t600, na.rm = TRUE) + 0.05,
                length.out = 100)
cal_curve <- data.frame(
  mean_t600 = t600_seq,
  predicted = cal_a * t600_seq^2 + cal_b * t600_seq + cal_c
)


flow_data_compiled$flow_ftd<- flow_data_compiled$flow*0.1148 + 0.0077
cal_curve$predicted_flow_ftd<- cal_curve$predicted*0.1148 + 0.0077
flow_data_compiled$predicted_flow_ft_d<-flow_data_compiled$predicted_flow_ml_min*0.1148 + 0.0077


rmse <- sqrt(mean((flow_data_compiled$flow_ftd - flow_data_compiled$predicted_flow_ft_d)^2, na.rm = TRUE))
summary(lm(flow_data_compiled$flow_ftd ~ flow_data_compiled$predicted_flow_ft_d))

p2 <- ggplot(flow_data_compiled, aes(x = flow_ftd, y = mean_t600)) +
  geom_line(data = cal_curve, aes(x = predicted_flow_ftd, y = mean_t600),color = "gray40", linetype = "dashed") +
  geom_point(aes(color = as.factor(flow_ftd)), size = 3) +
  labs(
    x = "Flow rate (ft/d)",
    y = "Temperature Flow Differential",
    title = "Calibration curve: temperature vs flow speed",
    color = "Flow rate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# Plot 3: Flow direction by cycle (polar-ish view using standard plot)
p3 <- ggplot(flow_data_compiled, aes(x = cycle, y = flow_direction_deg, 
                                      color = as.factor(flow))) +
  geom_point(size = 3) +
  labs(
    x = "Cycle",
    y = "Recovered flow direction (degrees)",
    title = "Flow direction by cycle",
    color = "Flow rate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)





# Plot 4: Directional magnitude vs predicted flow  
# (should be correlated if direction signal scales with speed)
p4 <- ggplot(flow_data_compiled, aes(x = predicted_flow_ml_min, y = directional_mag,
                                      color = as.factor(flow))) +
  geom_point(size = 3) +
  labs(
    x = "Actual Flow (ft/d)",
    y = "Directional signal magnitude",
    title = "Directional magnitude vs flow speed",
    color = "Flow rate"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p4)

cat("\n========== END FLOW ANALYSIS ==========\n")




fdm<-melt(flow_data_compiled[,c("time0","flow_ftd","cycle","temp_A_max","temp_B_max","temp_C_max","temp_D_max","temp_E_max","temp_F_max")],id=c("time0","flow_ftd","cycle"))

ggplot(fdm,aes(flow_ftd,value,color=variable))+
facet_wrap(.~as.factor(cycle))+
geom_point()


