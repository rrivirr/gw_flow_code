import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# =============================
# User inputs
# =============================
BASE_DIR_0DEG = r"C:\Users\cade\Downloads\XZ_report-new\30ft\60-degrees"
RING = 2
TARGET_FLOWTIMES = [30.0, 600.0, 1230.0]   # seconds to tabulate
FLOWTIME_TO_PLOT = 1230.0                  # which flow-time to plot
KNOWN_VELOCITY = 30                     # ft/day scale for legend

# =============================
# Helpers
# =============================
def parse_flowtime_table(sample_path):
    flow_time_to_idx = {}
    with open(sample_path, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            s = line.strip()
            if not s or s.startswith('"') or s.startswith('('):
                continue
            parts = s.split()
            if len(parts) >= 3:
                try:
                    ftime = float(parts[-1])  # flow-time
                    idx = int(parts[0])       # row index
                    flow_time_to_idx[ftime] = idx
                except ValueError:
                    pass
    if not flow_time_to_idx:
        raise RuntimeError(f"No flow-time rows found in {sample_path}")
    return flow_time_to_idx

def read_sensor_series(path):
    vals = []
    with open(path, "r", encoding="utf-8", errors="ignore") as f:
        for line in f:
            s = line.strip()
            if not s or s.startswith('"') or s.startswith('('):
                continue
            parts = s.split()
            if len(parts) >= 3:
                try:
                    vals.append(float(parts[1]))  # middle column
                except ValueError:
                    pass
    if not vals:
        raise RuntimeError(f"No numeric rows found in {path}")
    return vals

def sensor_path(letter, ring):
    return os.path.join(BASE_DIR_0DEG, f"{letter.lower()}{ring}-rfile.out")

def nearest_key(target, keys_iterable):
    keys = list(keys_iterable)
    return min(keys, key=lambda k: abs(k - target))

# =============================
# Read mapping & series
# =============================
map_file = sensor_path('e', RING)          # use e{RING} to get flow-time → index mapping
flow_index = parse_flowtime_table(map_file)

E_series = read_sensor_series(sensor_path('e', RING))
B_series = read_sensor_series(sensor_path('b', RING))
F_series = read_sensor_series(sensor_path('f', RING))
C_series = read_sensor_series(sensor_path('c', RING))

# =============================
# Build the table (θ now CLOCKWISE from +X)
# =============================
rows = []
all_keys = sorted(flow_index.keys())
for ft in TARGET_FLOWTIMES:
    used_ft = ft if ft in flow_index else nearest_key(ft, all_keys)
    if used_ft != ft:
        print(f"ℹ️ Requested {ft}s not exact; using nearest {used_ft}s.")
    i = flow_index[used_ft]

    E = E_series[i]; B = B_series[i]
    F = F_series[i]; C = C_series[i]

    X = E - B               # E-B  (X component)
    Z = F - C               # F-C  (Z component)
    Rmag = np.hypot(X, Z)

    # angle CCW then flip to CW: θ_cw = (360 - θ_ccw) % 360
    theta_ccw = (np.degrees(np.arctan2(Z, X)) + 360) % 360
    theta_cw  = (360.0 - theta_ccw) % 360.0

    rows.append({
        "flow_time_requested": ft,
        "flow_time_used": used_ft,
        "E-B": X,
        "F-C": Z,
        "Mag_raw": Rmag,
        "Dir_deg": theta_cw   # <- now CLOCKWISE
    })

df = pd.DataFrame(rows).sort_values("flow_time_requested").reset_index(drop=True)

# Normalize & scaled velocity
df["Mag_norm"] = df["Mag_raw"] / df["Mag_raw"].max() if len(df) and df["Mag_raw"].max() > 0 else 0.0
df["Velocity(ft/day)"] = df["Mag_norm"] * KNOWN_VELOCITY

# Save the table
out_csv = os.path.join(BASE_DIR_0DEG, f"ring{RING}_vectors_0deg_times.csv")
df.to_csv(out_csv, index=False)
print("Saved table →", out_csv)
print(df)

# =============================
# Plot (letters fixed; θ shown as CLOCKWISE)
# =============================
def plot_components_resultant(df, flow_time_requested, title=None, save_name=None):
    # row to plot
    if flow_time_requested in df["flow_time_requested"].values:
        row = df.loc[df["flow_time_requested"].eq(flow_time_requested)].iloc[0]
    else:
        nearest_used = nearest_key(flow_time_requested, df["flow_time_used"].values)
        row = df.loc[df["flow_time_used"].eq(nearest_used)].iloc[0]
        print(f"ℹ️ Plot requested {flow_time_requested}s; using nearest table row at {nearest_used}s.")

    EB = float(row["E-B"])
    FC = float(row["F-C"])
    R_mag = float(row["Mag_raw"])
    theta_cw = float(row["Dir_deg"])

    # *** KEEP LETTER POSITIONS EXACTLY THE SAME AS BEFORE ***
    # A=0°, F=45°, E=135°, D=180°, C=225°, B=315°
    sensor_angles_deg = {'A': 0, 'F': 45, 'E': 135, 'D': 180, 'C': 225, 'B': 315}

    R = 1.0
    fig, ax = plt.subplots(figsize=(9, 7))
    ax.set_aspect('equal'); ax.set_xlim(-1.25, 1.25); ax.set_ylim(-1.25, 1.25)

    # circle & axes
    circle = plt.Circle((0, 0), R, edgecolor='#5b3d14', facecolor='none', lw=3)
    ax.add_patch(circle)
    ax.axhline(0, color='#5b3d14', lw=2)
    ax.axvline(0, color='#5b3d14', lw=2)

    # inlet/outlet
    ax.annotate('INLET',  xy=(1.0, 0),  xytext=(1.23, 0),
                arrowprops=dict(arrowstyle='->', lw=2, color='tab:blue'),
                va='center', color='tab:blue', fontsize=12)
    ax.annotate('OUTLET', xy=(-1.0, 0), xytext=(-1.23, 0),
                arrowprops=dict(arrowstyle='->', lw=2, color='tab:red'),
                va='center', ha='right', color='tab:red', fontsize=12)

    # sensor markers (unchanged)
    for name, ang in sensor_angles_deg.items():
        ang_rad = np.radians(ang)
        x = R*np.cos(ang_rad); z = R*np.sin(ang_rad)
        ax.plot(x, z, 'o', ms=12, color='#5b3d14')
        ax.text(x*1.10, z*1.10, name, fontsize=16, fontweight='bold',
                ha='center', va='center', color='#5b3d14')

    # arrows
    scale = 0.75 / max(1e-12, max(abs(EB), abs(FC)))
    ax.arrow(0, 0,  scale*EB, 0, width=0.006, head_width=0.06, head_length=0.08,
             fc='tab:green', ec='tab:green', alpha=0.8, length_includes_head=True,
             label='X component (E-B)')
    ax.arrow(0, 0,  0, scale*FC, width=0.006, head_width=0.06, head_length=0.08,
             fc='tab:orange', ec='tab:orange', alpha=0.8, length_includes_head=True,
             label='Z component (F-C)')
    ax.arrow(0, 0, scale*EB, scale*FC, width=0.008, head_width=0.08, head_length=0.10,
             fc='crimson', ec='crimson', length_includes_head=True,
             label=f"Resultant @{row['flow_time_used']:.0f}s\n|R|={R_mag:.3f}")

    if title is None:
        title = f"Ring {RING} — Resultant at {int(row['flow_time_used'])} s"
    ax.set_title(title, fontsize=18)
    ax.legend(loc='upper right', frameon=True)

    plt.tight_layout()
    if save_name is None:
        save_name = os.path.join(BASE_DIR_0DEG, f"ring{RING}_components_resultant_{int(row['flow_time_used'])}s_cw.png")
    plt.savefig(save_name, dpi=300)
    plt.show()
    print(f"Saved plot → {save_name}")
    print(f"Resultant magnitude: {R_mag:.5f}")
    print(f"Resultant direction (CW): {theta_cw:.2f}°  (from +X, clockwise)")

# plot with CW angle
plot_components_resultant(df, FLOWTIME_TO_PLOT)
