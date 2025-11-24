import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from datetime import datetime

# Function that labels each heating cycle numerically
def add_cycle_column(df, var_name):
    """
    Create a column that numbers each heating cycle based on 0->1 transitions
    """
    # Create a lagged version to detect transitions
    df['lag_var'] = df[var_name].shift(1)
    
    # Detect start of new cycles (0 -> 1 transition)
    df['cycle_start'] = False
    
    # First row: start cycle if it's 1
    if not pd.isna(df[var_name].iloc[0]) and df[var_name].iloc[0] == 1:
        df.loc[df.index[0], 'cycle_start'] = True
    
    # Detect transitions from 0 to 1
    df.loc[(df['lag_var'] == 0) & (df[var_name] == 1), 'cycle_start'] = True
    
    # Create cycle numbers using cumulative sum
    df['cycle'] = df['cycle_start'].astype(int).cumsum()
    
    # Clean up temporary columns
    df = df.drop(['lag_var', 'cycle_start'], axis=1)
    
    return df


# Read the data files
flow_160 = pd.read_csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/November 2025/20251110_1247/1049094.CSV')
flow_190 = pd.read_csv('/Users/jdh/Library/CloudStorage/GoogleDrive-jakehosen@gmail.com/My Drive/RRIV/FlowSensor/Kerfoot USGS Flow Chamber/November 2025/20251110_1543/1050225.CSV')

# Add flow labels
flow_160['flow'] = 160
flow_190['flow'] = 190

# Combine datasets
flowd = pd.concat([flow_160, flow_190], ignore_index=True)

# Select relevant columns
flowds = flowd[['RING01_TrawA', 'RING01_TrawB', 'RING01_TrawC', 'RING01_TrawD', 
                'RING01_TrawE', 'RING01_TrawF', 'flow', 'time.s', 'HEATER_heater']].copy()

# Convert time to datetime
flowds['dtp'] = pd.to_datetime(flowds['time.s'], unit='s')

# Melt the dataframe for plotting
flowdsm = flowds.melt(id_vars=['flow', 'time.s', 'dtp', 'HEATER_heater'], 
                      var_name='variable', value_name='value')
flowdsm = flowdsm.dropna()

# Create faceted plot
fig, axes = plt.subplots(2, 3, figsize=(15, 10))
axes = axes.flatten()
variables = flowdsm['variable'].unique()

for i, var in enumerate(variables):
    if i < len(axes):
        data_subset = flowdsm[flowdsm['variable'] == var]
        for flow_val in data_subset['flow'].unique():
            flow_data = data_subset[data_subset['flow'] == flow_val]
            axes[i].scatter(flow_data['dtp'], flow_data['value'], 
                          label=f'Flow {flow_val}', alpha=0.6)
        axes[i].set_title(var)
        axes[i].legend()
        axes[i].tick_params(axis='x', rotation=45)

plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/temperature_sensors_plot.png', dpi=300, bbox_inches='tight')
plt.close()


# Remap temperature sensors based on actual connections
# A - U7 - 0011011 - Currently C
# B - U5 - 0011101 - currently D
# C - U4 - 0011000 - currently A
# D - U6 - 0011001 - Currently B
# E - U3 - 0011100 - currently F
# F - U2 - 0011010 - currently E

dat = flowds.copy()
dat['temp_A'] = pd.to_numeric(dat['RING01_TrawC'], errors='coerce')
dat['temp_B'] = pd.to_numeric(dat['RING01_TrawD'], errors='coerce')
dat['temp_C'] = pd.to_numeric(dat['RING01_TrawA'], errors='coerce')
dat['temp_D'] = pd.to_numeric(dat['RING01_TrawB'], errors='coerce')
dat['temp_E'] = pd.to_numeric(dat['RING01_TrawF'], errors='coerce')
dat['temp_F'] = pd.to_numeric(dat['RING01_TrawE'], errors='coerce')

# Remove rows with missing heater data
dat = dat[dat['HEATER_heater'].notna()].copy()

# Add cycle column
dat2 = add_cycle_column(dat, 'HEATER_heater')

# Get number of cycles
cycles = int(dat2['cycle'].max())

# Initialize results dataframe
flow_data_compiled = pd.DataFrame()

# Process each cycle
for i in range(1, cycles + 1):
    cycle0 = dat2[dat2['cycle'] == i].copy()
    
    if cycle0.empty:
        continue
    
    heat_cycle = cycle0[cycle0['HEATER_heater'] == 1]
    
    if heat_cycle.empty:
        continue
    
    end_heat = heat_cycle['time.s'].max()
    end_heat_plus_10 = end_heat + (10 * 60)
    end_heat_plus_20 = end_heat + (20 * 60)
    
    # Interpolate temperatures at specific time points for each sensor
    results = {'cycle': i, 'end_heat': end_heat}
    
    for sensor in ['A', 'B', 'C', 'D', 'E', 'F']:
        temp_col = f'temp_{sensor}'
        
        # Interpolate at end_heat
        temp_0 = np.interp(end_heat, cycle0['time.s'], cycle0[temp_col])
        temp_plus_10 = np.interp(end_heat_plus_10, cycle0['time.s'], cycle0[temp_col])
        temp_plus_20 = np.interp(end_heat_plus_20, cycle0['time.s'], cycle0[temp_col])
        
        results[f'temp_{sensor}_0'] = temp_0
        results[f'temp_{sensor}_plus_10'] = temp_plus_10
        results[f'temp_{sensor}_plus_20'] = temp_plus_20
    
    # Calculate temperature differences for sensor pairs
    results['A_D_0'] = results['temp_A_0'] - results['temp_D_0']
    results['A_D_10'] = results['temp_A_plus_10'] - results['temp_D_plus_10']
    results['A_D_20'] = results['temp_A_plus_20'] - results['temp_D_plus_20']
    
    results['B_E_0'] = results['temp_B_0'] - results['temp_E_0']
    results['B_E_10'] = results['temp_B_plus_10'] - results['temp_E_plus_10']
    results['B_E_20'] = results['temp_B_plus_20'] - results['temp_E_plus_20']
    
    results['C_F_0'] = results['temp_C_0'] - results['temp_F_0']
    results['C_F_10'] = results['temp_C_plus_10'] - results['temp_F_plus_10']
    results['C_F_20'] = results['temp_C_plus_20'] - results['temp_F_plus_20']
    
    results['flow'] = cycle0['flow'].unique()[0]
    
    flow_data_compiled = pd.concat([flow_data_compiled, pd.DataFrame([results])], 
                                   ignore_index=True)


# Calculate temperature difference changes
flow_data_compiled['A_D_Diff_10'] = flow_data_compiled['A_D_10'] - flow_data_compiled['A_D_0']
flow_data_compiled['A_D_Diff_20'] = flow_data_compiled['A_D_20'] - flow_data_compiled['A_D_0']

flow_data_compiled['B_E_Diff_10'] = flow_data_compiled['B_E_10'] - flow_data_compiled['B_E_0']
flow_data_compiled['B_E_Diff_20'] = flow_data_compiled['B_E_20'] - flow_data_compiled['B_E_0']

flow_data_compiled['C_F_Diff_10'] = flow_data_compiled['C_F_10'] - flow_data_compiled['C_F_0']
flow_data_compiled['C_F_Diff_20'] = flow_data_compiled['C_F_20'] - flow_data_compiled['C_F_0']

# Calculate vector components (sensors arranged at 120° intervals)
# A-D pair at 0° (vertical)
flow_data_compiled['A_D_X'] = 0
flow_data_compiled['A_D_Y'] = flow_data_compiled['A_D_Diff_10']

# B-E pair at 60°
B_E_Angle = 60 * np.pi / 180
flow_data_compiled['B_E_X'] = flow_data_compiled['B_E_Diff_10'] * np.sin(B_E_Angle)
flow_data_compiled['B_E_Y'] = flow_data_compiled['B_E_Diff_10'] * np.cos(B_E_Angle)

# C-F pair at 120°
C_F_Angle = 120 * np.pi / 180
flow_data_compiled['C_F_X'] = flow_data_compiled['C_F_Diff_10'] * np.sin(C_F_Angle)
flow_data_compiled['C_F_Y'] = flow_data_compiled['C_F_Diff_10'] * np.cos(C_F_Angle)

# Sum vector components
flow_data_compiled['Sum_X_10'] = (flow_data_compiled['A_D_X'] + 
                                   flow_data_compiled['B_E_X'] + 
                                   flow_data_compiled['C_F_X'])

# Corrected Y calculation (from later in the R script)
flow_data_compiled['Sum_Y_10'] = ((flow_data_compiled['B_E_Diff_10'] / 2) * np.sqrt(3) + 
                                   flow_data_compiled['A_D_Diff_10'])
flow_data_compiled['Sum_Y_20'] = ((flow_data_compiled['B_E_Diff_20'] / 2) * np.sqrt(3) + 
                                   flow_data_compiled['A_D_Diff_20'])

# Calculate angle and magnitude
flow_data_compiled['angle_10'] = np.arctan2(flow_data_compiled['Sum_Y_10'], 
                                             flow_data_compiled['Sum_X_10'])
flow_data_compiled['angle_20'] = np.arctan2(flow_data_compiled['Sum_Y_20'], 
                                             flow_data_compiled['Sum_X_20'])

flow_data_compiled['angle_10_deg'] = flow_data_compiled['angle_10'] * (180 / np.pi)
flow_data_compiled['angle_20_deg'] = flow_data_compiled['angle_20'] * (180 / np.pi)

flow_data_compiled['flow_mag_10'] = np.sqrt(flow_data_compiled['Sum_X_10']**2 + 
                                             flow_data_compiled['Sum_Y_10']**2)
flow_data_compiled['flow_mag_20'] = np.sqrt(flow_data_compiled['Sum_X_20']**2 + 
                                             flow_data_compiled['Sum_Y_20']**2)


# Create plots
fig, axes = plt.subplots(5, 1, figsize=(12, 15))

# Plot temperature differences
for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[0].scatter(flow_subset['cycle'], flow_subset['C_F_Diff_10'], 
                   label=f'Flow {int(flow_val)}', alpha=0.7)
axes[0].set_ylabel('C_F_Diff_10')
axes[0].legend()
axes[0].grid(True, alpha=0.3)

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[1].scatter(flow_subset['cycle'], flow_subset['A_D_Diff_10'], 
                   label=f'Flow {int(flow_val)}', alpha=0.7)
axes[1].set_ylabel('A_D_Diff_10')
axes[1].legend()
axes[1].grid(True, alpha=0.3)

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[2].scatter(flow_subset['cycle'], flow_subset['B_E_Diff_10'], 
                   label=f'Flow {int(flow_val)}', alpha=0.7)
axes[2].set_ylabel('B_E_Diff_10')
axes[2].legend()
axes[2].grid(True, alpha=0.3)

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[3].scatter(flow_subset['cycle'], flow_subset['Sum_X_10'], 
                   label=f'Flow {int(flow_val)}', alpha=0.7)
axes[3].set_ylabel('Sum_X_10')
axes[3].legend()
axes[3].grid(True, alpha=0.3)

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[4].scatter(flow_subset['cycle'], flow_subset['Sum_Y_10'], 
                   label=f'Flow {int(flow_val)}', alpha=0.7)
axes[4].set_ylabel('Sum_Y_10')
axes[4].set_xlabel('Cycle')
axes[4].legend()
axes[4].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/vector_components_plot.png', dpi=300, bbox_inches='tight')
plt.close()


# Additional plots
fig, axes = plt.subplots(2, 2, figsize=(12, 10))

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[0, 0].scatter(flow_subset['cycle'], flow_subset['A_D_Diff_10'], 
                      label=f'Flow {int(flow_val)}', alpha=0.7)
axes[0, 0].set_ylabel('A_D_Diff_10')
axes[0, 0].set_xlabel('Cycle')
axes[0, 0].legend()
axes[0, 0].grid(True, alpha=0.3)

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[0, 1].scatter(flow_subset['cycle'], flow_subset['B_E_Diff_10'], 
                      label=f'Flow {int(flow_val)}', alpha=0.7)
axes[0, 1].set_ylabel('B_E_Diff_10')
axes[0, 1].set_xlabel('Cycle')
axes[0, 1].legend()
axes[0, 1].grid(True, alpha=0.3)

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[1, 0].scatter(flow_subset['cycle'], flow_subset['C_F_Diff_10'], 
                      label=f'Flow {int(flow_val)}', alpha=0.7)
axes[1, 0].set_ylabel('C_F_Diff_10')
axes[1, 0].set_xlabel('Cycle')
axes[1, 0].legend()
axes[1, 0].grid(True, alpha=0.3)

for flow_val in flow_data_compiled['flow'].unique():
    flow_subset = flow_data_compiled[flow_data_compiled['flow'] == flow_val]
    axes[1, 1].scatter(flow_subset['cycle'], flow_subset['angle_10_deg'], 
                      label=f'Flow {int(flow_val)}', alpha=0.7)
axes[1, 1].set_ylabel('Angle (degrees)')
axes[1, 1].set_xlabel('Cycle')
axes[1, 1].legend()
axes[1, 1].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/analysis_summary_plot.png', dpi=300, bbox_inches='tight')
plt.close()


# Example plot for a specific cycle (cycle 16)
dat2l = dat2[dat2['cycle'] == 16].copy()
dat2lm = dat2l[['dtp', 'HEATER_heater', 'temp_A', 'temp_B', 'temp_C', 
                'temp_D', 'temp_E', 'temp_F', 'cycle']].melt(
    id_vars=['dtp', 'HEATER_heater', 'cycle'],
    var_name='variable',
    value_name='value'
)

fig, axes = plt.subplots(2, 3, figsize=(15, 8))
axes = axes.flatten()
temp_vars = ['temp_A', 'temp_B', 'temp_C', 'temp_D', 'temp_E', 'temp_F']

for i, temp_var in enumerate(temp_vars):
    data_subset = dat2lm[dat2lm['variable'] == temp_var]
    for heater_state in data_subset['HEATER_heater'].unique():
        heater_data = data_subset[data_subset['HEATER_heater'] == heater_state]
        axes[i].scatter(heater_data['dtp'], heater_data['value'], 
                       label=f'Heater {int(heater_state)}', alpha=0.6)
    axes[i].set_title(temp_var)
    axes[i].legend()
    axes[i].tick_params(axis='x', rotation=45)

plt.tight_layout()
plt.savefig('/mnt/user-data/outputs/cycle_16_detail.png', dpi=300, bbox_inches='tight')
plt.close()


# Save the compiled flow data
flow_data_compiled.to_csv('/mnt/user-data/outputs/flow_data_compiled.csv', index=False)

print("Analysis complete!")
print(f"\nProcessed {cycles} heating cycles")
print(f"\nFlow data shape: {flow_data_compiled.shape}")
print(f"\nColumns: {list(flow_data_compiled.columns)}")
print(f"\nFirst few rows:")
print(flow_data_compiled.head())
print("\nFiles saved:")
print("  - temperature_sensors_plot.png")
print("  - vector_components_plot.png")
print("  - analysis_summary_plot.png")
print("  - cycle_16_detail.png")
print("  - flow_data_compiled.csv")