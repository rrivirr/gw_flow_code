import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Read the CSV file
simout = pd.read_csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Ansys_data&Analysis/Ansys_Results/240sec_ON-time/ALL_angles_ALL_velocities_simulation_data_restructured.csv')

# Rename first 6 columns
simout.columns.values[0:6] = ["Time", "Type", "Velocity_ft", "XZ_Angle", "YZ_Angle", "Cycle"]

# Create difference columns
simout['A1A3_Difference'] = simout['A1'] - simout['A3']
simout['A2D2_Difference'] = simout['A2'] - simout['D2']
simout['B2E2_Difference'] = simout['B2'] - simout['E2']
simout['C2F2_Difference'] = simout['C2'] - simout['F2']

# Create factor versions (categorical)
simout['XZ_Angle_fac'] = simout['XZ_Angle'].astype('category')
simout['YZ_Angle_fac'] = simout['YZ_Angle'].astype('category')
simout['Velocity_ft_fac'] = simout['Velocity_ft'].astype('category')

# Compare time 1 to time 480 (8 minutes)
# Note: The summaryBy code in R isn't used in the rest of the script, so I'm commenting it out
# If needed, this would be: 
# simout_max = simout.groupby(['XZ_Angle_fac', 'Velocity_ft_fac']).agg({
#     'A2D2_Difference': lambda x: x.idxmin(),
#     'B2E2_Difference': lambda x: x.idxmin(),
#     'C2F2_Difference': lambda x: x.idxmin()
# })

# Initialize empty dataframe for compiled timings
timings_compiled = pd.DataFrame()

# Get unique values
XZ_angles = simout['XZ_Angle_fac'].dropna().unique()
YZ_angles = simout['YZ_Angle_fac'].dropna().unique()
Velocity_ft = simout['Velocity_ft_fac'].unique()

# Nested loops to process data
for i in XZ_angles:
    for j in Velocity_ft:
        for k in YZ_angles:
            # Subset data
            simout_sub = simout[(simout['XZ_Angle_fac'] == i) & 
                               (simout['Velocity_ft_fac'] == j) & 
                               (simout['YZ_Angle_fac'] == k)]
            
            if len(simout_sub) > 20:
                # Find max differences
                A2D2_max_difference = simout_sub.loc[simout_sub['A2D2'].abs().idxmax(), 'Time']
                B2E2_max_difference = simout_sub.loc[simout_sub['B2E2'].abs().idxmax(), 'Time']
                C2F2_max_difference = simout_sub.loc[simout_sub['C2F2'].abs().idxmax(), 'Time']
                
                # Get values at time 1 and 480
                A2D2_difference_1 = simout_sub.loc[simout_sub['Time'] == 1, 'A2D2'].values[0] if len(simout_sub[simout_sub['Time'] == 1]) > 0 else np.nan
                A2D2_difference_480 = simout_sub.loc[simout_sub['Time'] == 480, 'A2D2'].values[0] if len(simout_sub[simout_sub['Time'] == 480]) > 0 else np.nan
                
                B2E2_difference_1 = simout_sub.loc[simout_sub['Time'] == 1, 'B2E2'].values[0] if len(simout_sub[simout_sub['Time'] == 1]) > 0 else np.nan
                B2E2_difference_480 = simout_sub.loc[simout_sub['Time'] == 480, 'B2E2'].values[0] if len(simout_sub[simout_sub['Time'] == 480]) > 0 else np.nan
                
                C2F2_difference_1 = simout_sub.loc[simout_sub['Time'] == 1, 'C2F2'].values[0] if len(simout_sub[simout_sub['Time'] == 1]) > 0 else np.nan
                C2F2_difference_480 = simout_sub.loc[simout_sub['Time'] == 480, 'C2F2'].values[0] if len(simout_sub[simout_sub['Time'] == 480]) > 0 else np.nan
                
                A1A3_difference_1 = simout_sub.loc[simout_sub['Time'] == 1, 'A1A3'].values[0] if len(simout_sub[simout_sub['Time'] == 1]) > 0 else np.nan
                A1A3_difference_480 = simout_sub.loc[simout_sub['Time'] == 480, 'A1A3'].values[0] if len(simout_sub[simout_sub['Time'] == 480]) > 0 else np.nan
                
                # Create row for this combination
                row_data = pd.DataFrame({
                    'XZ_angles': [i],
                    'YZ_angles': [k],
                    'Velocity_ft': [j],
                    'A2D2_max_difference': [A2D2_max_difference],
                    'B2E2_max_difference': [B2E2_max_difference],
                    'C2F2_max_difference': [C2F2_max_difference],
                    'A1A3_difference_1': [A1A3_difference_1],
                    'A1A3_difference_480': [A1A3_difference_480],
                    'A2D2_difference_1': [A2D2_difference_1],
                    'A2D2_difference_480': [A2D2_difference_480],
                    'B2E2_difference_1': [B2E2_difference_1],
                    'B2E2_difference_480': [B2E2_difference_480],
                    'C2F2_difference_1': [C2F2_difference_1],
                    'C2F2_difference_480': [C2F2_difference_480]
                })
                
                timings_compiled = pd.concat([timings_compiled, row_data], ignore_index=True)

# Calculate time differences
timings_compiled['B2E2_timediff'] = timings_compiled['B2E2_difference_480'] - timings_compiled['B2E2_difference_1']
timings_compiled['C2F2_timediff'] = timings_compiled['C2F2_difference_480'] - timings_compiled['C2F2_difference_1']
timings_compiled['A2D2_timediff'] = timings_compiled['A2D2_difference_480'] - timings_compiled['A2D2_difference_1']
timings_compiled['A1A3_timediff'] = timings_compiled['A1A3_difference_480'] - timings_compiled['A1A3_difference_1']

# Vector decomposition using angles
A_D_Angle = 90 * np.pi / 180
timings_compiled['A_D_X'] = timings_compiled['A2D2_timediff'] * np.sin(A_D_Angle)
timings_compiled['A_D_Z'] = timings_compiled['A2D2_timediff'] * np.cos(A_D_Angle)

B_E_Angle = 330 * np.pi / 180
timings_compiled['B_E_X'] = timings_compiled['B2E2_timediff'] * np.sin(B_E_Angle)
timings_compiled['B_E_Z'] = timings_compiled['B2E2_timediff'] * np.cos(B_E_Angle)

F_C_Angle = 30 * np.pi / 180
timings_compiled['C_F_X'] = timings_compiled['C2F2_timediff'] * np.sin(F_C_Angle)
timings_compiled['C_F_Z'] = timings_compiled['C2F2_timediff'] * np.cos(F_C_Angle)

timings_compiled['A1_A3_Y'] = timings_compiled['A1A3_timediff']

# Calculate total magnitudes
timings_compiled['total_z_mag'] = timings_compiled['C_F_Z'] + timings_compiled['B_E_Z'] + timings_compiled['A_D_Z']
timings_compiled['total_x_mag'] = timings_compiled['C_F_X'] + timings_compiled['B_E_X'] + timings_compiled['A_D_X']
timings_compiled['total_y_mag'] = timings_compiled['A1_A3_Y']
timings_compiled['total_mag'] = np.sqrt(
    (timings_compiled['total_z_mag'] / 3.312)**2 + 
    (timings_compiled['total_x_mag'] / 3.312)**2 + 
    ((timings_compiled['total_y_mag'] / 6) * 3)**2
)

# Create plots
# Plot 1: Total magnitude vs Velocity
plt.figure(figsize=(10, 6))
for yz_angle in timings_compiled['YZ_angles'].unique():
    data = timings_compiled[timings_compiled['YZ_angles'] == yz_angle]
    plt.scatter(pd.to_numeric(data['Velocity_ft']), data['total_mag'], 
                s=100, label=f'YZ_angle={yz_angle}', alpha=0.7)
plt.xlabel('Velocity (ft/s)')
plt.ylabel('Total Magnitude')
plt.legend()
plt.title('Total Magnitude vs Velocity colored by YZ Angle')
plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/plot1_total_mag_vs_velocity.png', dpi=300)
plt.show()

# Plot 2: A1_A3_Z vs YZ_angles for Velocity_ft=5
# Note: Original code references A1_A3_Z which doesn't exist in the dataframe
# I think this might be a typo and should be A1_A3_Y or another variable
# Using A1_A3_Y for now
subset_vel5 = timings_compiled[timings_compiled['Velocity_ft'] == 5]
if len(subset_vel5) > 0:
    plt.figure(figsize=(10, 6))
    for xz_angle in subset_vel5['XZ_angles'].unique():
        data = subset_vel5[subset_vel5['XZ_angles'] == xz_angle]
        plt.scatter(pd.to_numeric(data['YZ_angles']), data['A1_A3_Y'], 
                    s=100, label=f'XZ_angle={xz_angle}', alpha=0.7)
    plt.xlabel('YZ Angle')
    plt.ylabel('A1_A3_Y')
    plt.legend()
    plt.title('A1_A3_Y vs YZ Angle (Velocity = 5 ft/s)')
    plt.tight_layout()
    plt.savefig('/mnt/user-data/outputs/plot2_a1a3y_vs_yz_angle.png', dpi=300)
    plt.show()

# Plot 3: B2E2_timediff vs Velocity for XZ_angles=0
subset_xz0 = timings_compiled[timings_compiled['XZ_angles'] == 0]
if len(subset_xz0) > 0:
    plt.figure(figsize=(10, 6))
    for xz_angle in subset_xz0['XZ_angles'].unique():
        data = subset_xz0[subset_xz0['XZ_angles'] == xz_angle]
        plt.scatter(pd.to_numeric(data['Velocity_ft']), data['B2E2_timediff'], 
                    s=100, label=f'XZ_angle={xz_angle}', alpha=0.7)
    plt.xlabel('Velocity (ft/s)')
    plt.ylabel('B2E2 Time Difference')
    plt.legend()
    plt.title('B2E2 Time Diff vs Velocity (XZ Angle = 0)')
    plt.tight_layout()
    plt.savefig('/mnt/user-data/outputs/plot3_b2e2_vs_velocity.png', dpi=300)
    plt.show()

# Print sorted timings_compiled
print("\nTimings compiled (sorted by XZ_angles):")
print(timings_compiled.sort_values('XZ_angles'))

# Subset for XZ_Angle=0 and Velocity_ft=5
simout_0 = simout[(simout['XZ_Angle'] == 0) & (simout['Velocity_ft'] == 5)]

# Plot 4: A2 and A1 over time
if len(simout_0) > 0:
    plt.figure(figsize=(12, 6))
    plt.plot(simout_0['Time'], simout_0['A2'], 'o', color='green', label='A2', alpha=0.6)
    plt.plot(simout_0['Time'], simout_0['A1'], 'o', color='brown', label='A1', alpha=0.6)
    plt.axvline(x=240, color='red', linestyle='--', label='t=240s')
    plt.axvline(x=840, color='green', linestyle='--', label='t=840s')
    plt.axvline(x=1440, color='blue', linestyle='--', label='t=1440s')
    plt.xlabel('Time (s)')
    plt.ylabel('Temperature')
    plt.legend()
    plt.title('A2 and A1 over Time (XZ_Angle=0, Velocity=5 ft/s)')
    plt.tight_layout()
    plt.savefig('/mnt/user-data/outputs/plot4_a2_a1_timeseries.png', dpi=300)
    plt.show()

print("\nAnalysis complete! All plots saved to /mnt/user-data/outputs/")