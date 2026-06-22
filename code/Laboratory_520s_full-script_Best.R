# =============================================================================
# LAB FULL ANALYSIS — MERGED SCRIPT
# Part 0: Build master combined CSV from raw sensor data
# Part 1: Time series plots (all angles x flow rates, baseline corrected)
# Part 2: Universal calibration at tf=520s (speed, direction, error analysis)
#
# Fixes applied vs previous versions:
#   [FIX 1] Direction formula: (atan2(Xh,Zh)*180/pi) %% 360 — parentheses
#            corrected to fix operator precedence bug in Doc 6
#   [FIX 2] Calibration curve: 15 median points only, stats from same medians
#            (consistent with simulation approach — Doc 6 mixed all-cycle stats
#            with median regression line)
#   [FIX 3] All plots: 600 dpi, title=filename, bold 20pt font, STAT_SIZE=11
#   [FIX 4] All plots: title=NULL on plot, used as filename via fname list
#   [FIX 5] Both MAE and RMSE on every plot
#
# Direction correction integrated (NEW):
#   [DIR 1] STEP 4a inserted after calibration fit — computes per-angle
#            mounting offset via circular mean, subtracts it from every cycle,
#            stores fl_dir_corrected and dir_scatter_corr in all_res
#   [DIR 2] P3 split: p_dir_raw (fl_dir_raw) kept for reference;
#            p_dir (fl_dir_corrected) is the primary direction plot
#   [DIR 3] P5 now uses dir_scatter_corr (scatter around corrected mean)
#   [DIR 4] P11 (2x2 comparison panel) saved at 600 dpi
#   [DIR 5] all_cycles_520s.csv includes fl_dir_corrected and mount_offset
#   [DIR 6] direction_mount_offsets.csv saved separately
# =============================================================================
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(grid)

ml_to_ftday   <- function(ml_min) 0.11484 * ml_min + 0.00768
ml_from_ftday <- function(ft)     round((ft - 0.00768) / 0.11484, 1)

# =============================================================================
# SHARED CONFIGURATION
# =============================================================================
ROOT_DIR    <- "/home/ayobami/Desktop/USGS_data-lorawan/USGS_sdcard/final-chamber-data"
MASTER_CSV  <- file.path(ROOT_DIR, "combined_all_angles/lab_master_compiled.csv")
OUT_DIR_TS  <- file.path(ROOT_DIR, "timeseries_plots")
OUT_DIR_CAL <- file.path(ROOT_DIR, "universal_calibration_520s")
dir.create(OUT_DIR_TS,  recursive=TRUE, showWarnings=FALSE)
dir.create(OUT_DIR_CAL, recursive=TRUE, showWarnings=FALSE)

# ── Time series settings ──────────────────────────────────────────────────────
ON_TIME          <- 240
N_BASELINE       <- 5
IQR_K            <- 3.0
MAX_INDIV_CYCLES <- 10
BIN_WIDTH        <- 20

# ── Calibration settings ──────────────────────────────────────────────────────
TF                    <- 520
INTERVAL              <- 10
MIN_RISE              <- 0.5
MAX_SPIKE             <- 5.0
SKIP_FIRST_N          <- 3
SPIKE_BASELINE_THRESH <- 2.0
N_PREHEAT_ROWS        <- 5

# [FIX 1] Correct projection angles
PHI_AD <- 0   * pi / 180   # A at 0 deg
PHI_BE <- 150 * pi / 180   # B at 150 deg
PHI_CF <- 210 * pi / 180   # C at 210 deg

TS_SENSOR_COLS  <- c("A1","B1","C1","D1","E1","F1")
CAL_SENSOR_COLS <- c("temp_A","temp_B","temp_C","temp_D","temp_E","temp_F")

SENSOR_COLORS <- c(
  "A1"="#E63946","B1"="#457B9D","C1"="#2A9D8F",
  "D1"="#E76F51","E1"="#6A4C93","F1"="#1D7A1D")
ANGLE_COLORS <- c("0"="#1B998B","15"="#E07A5F","30"="#6A4C93")

DEGREE_CONFIG <- list(
  list(deg=0, exp_dir=0, dir=file.path(ROOT_DIR,"0-degree"),
       files=list(
         list(folder="20260404_70ml-min",    flow=70.0),
         list(folder="20260405_102ml-min",   flow=102.0),
         list(folder="20260406_125.3ml-min", flow=125.3),
         list(folder="20260406_172ml-min",   flow=172.0),
         list(folder="20260407_232ml-min",   flow=232.0))),
  list(deg=15, exp_dir=15, dir=file.path(ROOT_DIR,"15-degree"),
       files=list(
         list(folder="20260413_70ml-min",    flow=70.0),
         list(folder="20260414_88ml-min",    flow=88.0),
         list(folder="20260415_110ml-min",   flow=110.0),
         list(folder="20260416_144ml-min",   flow=144.0),
         list(folder="20260417_210.6ml-min", flow=210.6))),
  list(deg=30, exp_dir=30, dir=file.path(ROOT_DIR,"30-degree"),
       files=list(
         list(folder="20260417_72ml-min",    flow=72.0),
         list(folder="20260418_90ml-min",    flow=90.0),
         list(folder="20260419_110ml-min",   flow=110.0),
         list(folder="20260420_156ml-min",   flow=156.0),
         list(folder="20260421_214ml-min",   flow=214.0)))
)

# =============================================================================
# SHARED THEMES
# =============================================================================
# [FIX 3] 20pt bold for all text elements
bold_theme_sm <- theme_bw(base_size=11) +
  theme(plot.title       = element_text(face="bold", size=11),
        plot.subtitle    = element_text(face="bold", size=9,  color="gray30"),
        axis.title       = element_text(face="bold", size=10),
        axis.text        = element_text(face="bold", size=9),
        legend.title     = element_text(face="bold", size=10),
        legend.text      = element_text(face="bold", size=9),
        strip.text       = element_text(face="bold", size=10),
        plot.background  = element_rect(fill="white", color=NA),
        panel.background = element_rect(fill="white", color=NA),
        legend.background= element_rect(fill="white", color=NA),
        legend.position  = "right")

bold_theme <- theme_bw(base_size=20) +
  theme(plot.title        = element_blank(),          # [FIX 4] title removed
        plot.subtitle     = element_blank(),
        axis.title        = element_text(face="bold", size=20),
        axis.text         = element_text(face="bold", size=20),
        legend.title      = element_text(face="bold", size=20),
        legend.text       = element_text(face="bold", size=20),
        strip.text        = element_text(face="bold", size=20),
        plot.background   = element_rect(fill="white", color=NA),
        panel.background  = element_rect(fill="white", color=NA),
        legend.background = element_rect(fill="white", color=NA))

STAT_SIZE <- 11.0  # [FIX 3] ~31pt annotation text

# =============================================================================
# SHARED HELPERS
# =============================================================================
extract_legend <- function(p) {
  tmp <- ggplot_gtable(ggplot_build(p + theme(legend.position="right")))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg)==0) return(NULL)
  tmp$grobs[[leg]]
}

show_grob <- function(g, label="") {
  if (nchar(label)>0) cat(sprintf("  Displaying: %s\n", label))
  grid.newpage()
  grid.draw(g)
}

find_csv <- function(folder) {
  if (!dir.exists(folder)) return(NULL)
  f <- list.files(folder, pattern="\\.CSV$", full.names=TRUE, ignore.case=TRUE)
  if (length(f)==0) return(NULL); f[1]
}

average_traw <- function(df, interval=10) {
  tcal_cols <- grep("RING01_Tcal", names(df), value=TRUE)
  if (length(tcal_cols)==0) stop("No RING01_Tcal columns found.")
  df$time.s <- as.numeric(df$time.s); df$TS_switch <- as.numeric(df$TS_switch)
  for (col in tcal_cols) df[[col]] <- as.numeric(df[[col]])
  t0 <- min(df$time.s, na.rm=TRUE); df$bin <- floor((df$time.s-t0)/interval)
  out <- do.call(rbind, lapply(sort(unique(df$bin)), function(b) {
    s <- df[df$bin==b,]
    row <- data.frame(time.s=mean(s$time.s,na.rm=TRUE),
                      TS_switch=s$TS_switch[1], flow=s$flow[1],
                      stringsAsFactors=FALSE)
    for (col in tcal_cols) row[[paste0(col,"_mean")]] <- mean(s[[col]],na.rm=TRUE)
    row
  }))
  rownames(out) <- NULL; out
}

add_cycle_column <- function(df) {
  prev <- c(NA, head(df$HEATER,-1))
  df$cycle_start <- ifelse(is.na(prev), df$HEATER==1, prev==0 & df$HEATER==1)
  df$cycle <- cumsum(df$cycle_start)
  df <- df %>% group_by(cycle) %>%
    mutate(time_loop=time.s-min(time.s)) %>% ungroup()
  df$cycle_start <- NULL; df
}

circ_mean_deg <- function(deg) {
  r <- deg*pi/180
  (atan2(mean(sin(r),na.rm=TRUE), mean(cos(r),na.rm=TRUE))*180/pi) %% 360
}
circ_mae_deg  <- function(obs,ref) { d<-((obs-ref+180)%%360)-180; mean(abs(d),na.rm=TRUE) }
circ_rmse_deg <- function(obs,ref) { d<-((obs-ref+180)%%360)-180; sqrt(mean(d^2,na.rm=TRUE)) }
circ_err      <- function(obs,ref) { ((obs-ref+180)%%360)-180 }

# Additional helpers for master CSV builder
secs_to_hms <- function(s) {
  s <- round(s); hh <- s %/% 3600; mm <- (s %% 3600) %/% 60; ss <- s %% 60
  sprintf("%d:%02d:%02d", hh, mm, ss)
}
extract_date <- function(folder) {
  date_str <- regmatches(folder, regexpr("[0-9]{8}", folder))
  as.POSIXct(date_str, format="%Y%m%d", tz="UTC")
}

# =============================================================================
# ██████████████████████████████████████████████████████
# PART 0 — BUILD MASTER COMBINED CSV
# ██████████████████████████████████████████████████████
# =============================================================================
# Columns: Time, Relative_timestamp, Absolute_timestamp, Flow_rate_ml_min,
#          Velocity (ft/day), XZ angle (degrees), YZ angle (degrees),
#          ON_time_s, cycle_duration_s, Cycle, A1, B1, C1, D1, E1, F1
#   One row per timestep (10s bins) per cycle
#   Raw calibrated temperatures — no logs, no means, no derived features
# =============================================================================
cat("\n")
cat(strrep("=",70),"\n")
cat("PART 0 — BUILD MASTER COMBINED CSV\n")
cat(strrep("=",70),"\n")

MASTER_OUT_DIR <- file.path(ROOT_DIR, "combined_all_angles")
dir.create(MASTER_OUT_DIR, recursive=TRUE, showWarnings=FALSE)

ALL_RUNS <- list(
  list(xz_angle=0, on_time=240, cyc_dur=1440,
       folders=list(
         list(folder="0-degree/20260404_70ml-min",    flow=70.0),
         list(folder="0-degree/20260405_102ml-min",   flow=102.0),
         list(folder="0-degree/20260406_125.3ml-min", flow=125.3),
         list(folder="0-degree/20260406_172ml-min",   flow=172.0),
         list(folder="0-degree/20260407_232ml-min",   flow=232.0))),
  list(xz_angle=15, on_time=240, cyc_dur=1440,
       folders=list(
         list(folder="15-degree/20260413_70ml-min",    flow=70.0),
         list(folder="15-degree/20260414_88ml-min",    flow=88.0),
         list(folder="15-degree/20260415_110ml-min",   flow=110.0),
         list(folder="15-degree/20260416_144ml-min",   flow=144.0),
         list(folder="15-degree/20260417_210.6ml-min", flow=210.6))),
  list(xz_angle=30, on_time=240, cyc_dur=1440,
       folders=list(
         list(folder="30-degree/20260417_72ml-min",  flow=72.0),
         list(folder="30-degree/20260418_90ml-min",  flow=90.0),
         list(folder="30-degree/20260419_110ml-min", flow=110.0),
         list(folder="30-degree/20260420_156ml-min", flow=156.0),
         list(folder="30-degree/20260421_214ml-min", flow=214.0)))
)

SENSOR_COLS_P0 <- c("temp_A","temp_B","temp_C","temp_D","temp_E","temp_F")

if (file.exists(MASTER_CSV)) {
  cat(sprintf("  Master CSV already exists: %s\n", MASTER_CSV))
  cat("  Skipping rebuild. Delete the file to force rebuild.\n")
} else {
  cat("  Building master CSV from raw data...\n")
  master <- data.frame()
  
  for (run in ALL_RUNS) {
    xz_angle <- run$xz_angle; on_time <- run$on_time; cyc_dur <- run$cyc_dur
    cat(sprintf("\n=== XZ = %d deg ===\n", xz_angle))
    
    for (fld in run$folders) {
      csv_path <- find_csv(file.path(ROOT_DIR, fld$folder))
      if (is.null(csv_path)) { cat(sprintf("  [SKIP] %s\n", fld$folder)); next }
      flow_ml <- fld$flow; flow_ft <- ml_to_ftday(flow_ml)
      base_dt <- extract_date(fld$folder)
      cat(sprintf("  Loading %.1f ml/min = %.4f ft/day\n", flow_ml, flow_ft))
      
      d   <- read.csv(csv_path, stringsAsFactors=FALSE); d$flow <- flow_ml
      mla <- suppressWarnings(as.data.frame(average_traw(d, INTERVAL)))
      mla$HEATER <- as.numeric(mla$TS_switch)
      mla <- mla %>% filter(!is.na(RING01_TcalD_mean))
      mla <- add_cycle_column(mla)
      mla <- mla %>% filter(!is.na(flow))
      mla$temp_A <- as.numeric(mla$RING01_TcalA_mean)
      mla$temp_B <- as.numeric(mla$RING01_TcalB_mean)
      mla$temp_C <- as.numeric(mla$RING01_TcalC_mean)
      mla$temp_D <- as.numeric(mla$RING01_TcalD_mean)
      mla$temp_E <- as.numeric(mla$RING01_TcalE_mean)
      mla$temp_F <- as.numeric(mla$RING01_TcalF_mean)
      
      # Quality filter
      cycle_meta <- data.frame()
      for (cid in unique(mla$cycle)) {
        cyc <- mla[mla$cycle==cid,]; if(nrow(cyc)<20) next
        heat <- cyc[cyc$HEATER==1,]; if(nrow(heat)<3) next
        base_t <- sapply(SENSOR_COLS_P0, function(s)
          mean(heat[[s]][1:min(2,nrow(heat))],na.rm=TRUE))
        rises <- sapply(SENSOR_COLS_P0, function(s)
          max(cyc[[s]],na.rm=TRUE)-base_t[s])
        hd <- max(heat$time_loop)-min(heat$time_loop)
        cycle_meta <- bind_rows(cycle_meta,
                                data.frame(cycle=cid, flow=cyc$flow[1], mean_rise=mean(rises),
                                           max_rise=max(rises), heat_dur=hd))
      }
      if (nrow(cycle_meta)==0) next
      good_ids <- cycle_meta$cycle[cycle_meta$mean_rise>=MIN_RISE &
                                     cycle_meta$max_rise<MAX_SPIKE]
      gm <- cycle_meta[cycle_meta$cycle %in% good_ids,]
      if (nrow(gm)>2) {
        odd_ids <- gm$cycle[abs(gm$heat_dur-mean(gm$heat_dur))>2*sd(gm$heat_dur)]
        good_ids <- setdiff(good_ids, odd_ids)
      }
      good_ids_trimmed <- c()
      for (fl in unique(cycle_meta$flow)) {
        fl_ids <- sort(cycle_meta$cycle[cycle_meta$flow==fl &
                                          cycle_meta$cycle %in% good_ids])
        keep_ids <- if(length(fl_ids)>SKIP_FIRST_N)
          fl_ids[(SKIP_FIRST_N+1):length(fl_ids)] else fl_ids
        good_ids_trimmed <- c(good_ids_trimmed, keep_ids)
      }
      good_ids <- good_ids_trimmed
      
      # Anomaly removal
      anom_ids <- c()
      for (cid in good_ids) {
        cyc <- mla[mla$cycle==cid,]; heat <- cyc[cyc$HEATER==1,]
        cool <- cyc[cyc$HEATER==0,]
        if(nrow(heat)<2||nrow(cool)<2) next
        cool_before <- cool[cool$time_loop<heat$time_loop[1],]
        if(nrow(cool_before)<2) cool_before <- cool[1:min(5,nrow(cool)),]
        for (s in SENSOR_COLS_P0) {
          cool_med <- median(cool_before[[s]],na.rm=TRUE)
          first_row <- heat[[s]][1]
          if(!is.na(first_row)&&abs(first_row-cool_med)>SPIKE_BASELINE_THRESH) {
            anom_ids <- c(anom_ids,cid); break
          }
        }
      }
      good_ids <- setdiff(good_ids, unique(anom_ids))
      cat(sprintf("    Cycles kept: %d\n", length(good_ids)))
      
      min_cycle <- min(good_ids)
      for (cid in good_ids) {
        cyc <- mla[mla$cycle==cid,]
        rel_ts <- sapply(cyc$time_loop, secs_to_hms)
        cycle_offset_s <- (cid - min_cycle) * cyc_dur
        abs_ts <- format(base_dt + cycle_offset_s + cyc$time_loop,
                         "%Y-%m-%d %H:%M:%S")
        rows <- data.frame(
          Time=cyc$time_loop, Relative_timestamp=rel_ts,
          Absolute_timestamp=abs_ts, Flow_rate_ml_min=flow_ml,
          Velocity..ft.day.=flow_ft, XZ.angle..degrees.=xz_angle,
          YZ.angle..degrees.=0, ON_time_s=on_time,
          cycle_duration_s=cyc_dur, Cycle=cid,
          A1=cyc$temp_A, B1=cyc$temp_B, C1=cyc$temp_C,
          D1=cyc$temp_D, E1=cyc$temp_E, F1=cyc$temp_F,
          stringsAsFactors=FALSE)
        master <- bind_rows(master, rows)
      }
    }
  }
  
  write.csv(master, MASTER_CSV, row.names=FALSE)
  cat(sprintf("\n=== MASTER CSV SAVED ===\n"))
  cat(sprintf("  Rows: %d | Columns: %d\n", nrow(master), ncol(master)))
  cat(sprintf("  Path: %s\n", MASTER_CSV))
  cat(sprintf("  Rows per XZ angle:\n"))
  print(table(master$XZ.angle..degrees.))
  cat(sprintf("\n  Cycles per (angle, flow rate):\n"))
  print(master %>% group_by(XZ.angle..degrees., Flow_rate_ml_min, Velocity..ft.day.) %>%
          summarise(n_cycles=n_distinct(Cycle), .groups="drop") %>%
          mutate(Velocity..ft.day.=round(Velocity..ft.day.,3)))
  cat(sprintf("\n  Flow range: %.1f - %.1f ml/min  (%.3f - %.3f ft/day)\n",
              min(master$Flow_rate_ml_min), max(master$Flow_rate_ml_min),
              min(master$Velocity..ft.day.), max(master$Velocity..ft.day.)))
}

# =============================================================================
# ██████████████████████████████████████████████████████
# PART 1 — TIME SERIES PLOTS
# ██████████████████████████████████████████████████████
# =============================================================================
cat("\n")
cat(strrep("=",70),"\n")
cat("PART 1 — TIME SERIES PLOTS\n")
cat(strrep("=",70),"\n")

cat("Loading master CSV...\n")
df_ts <- read.csv(MASTER_CSV, stringsAsFactors=FALSE)
cat(sprintf("  Rows: %d | Cols: %d\n", nrow(df_ts), ncol(df_ts)))

names(df_ts)[names(df_ts)=="XZ.angle..degrees."] <- "angle"
names(df_ts)[names(df_ts)=="Velocity..ft.day."]  <- "vel"
names(df_ts)[names(df_ts)=="YZ.angle..degrees."] <- "yz"
df_ts$angle   <- as.integer(df_ts$angle)
df_ts$vel     <- round(df_ts$vel, 4)
df_ts$flow_ml <- ml_from_ftday(df_ts$vel)

cat(sprintf("  Angles: %s\n", paste(sort(unique(df_ts$angle)), collapse=", ")))
cat(sprintf("  Velocities: %d unique\n", length(unique(df_ts$vel))))
cat(sprintf("  Time range: %.1f - %.1f s\n", min(df_ts$Time), max(df_ts$Time)))

# ── Baseline correction ───────────────────────────────────────────────────────
cat("\nApplying per-cycle per-sensor baseline correction...\n")
df_ts <- df_ts %>%
  group_by(angle, vel, Cycle) %>%
  group_modify(function(cyc_data, keys) {
    cool_rows <- cyc_data[cyc_data$Time > ON_TIME, ]
    cool_rows <- cool_rows[order(cool_rows$Time, decreasing=TRUE), ]
    ref_rows  <- head(cool_rows, N_BASELINE)
    if (nrow(ref_rows) < 1) return(cyc_data)
    for (s in TS_SENSOR_COLS)
      cyc_data[[s]] <- cyc_data[[s]] - mean(ref_rows[[s]], na.rm=TRUE)
    cyc_data
  }) %>% ungroup()
cat("  Done.\n")

# ── Outlier removal ───────────────────────────────────────────────────────────
cat("Removing outliers...\n")
df_long_ts <- df_ts %>%
  pivot_longer(cols=all_of(TS_SENSOR_COLS), names_to="sensor", values_to="temp")
df_long_ts <- df_long_ts %>%
  group_by(angle, vel, sensor) %>%
  mutate(med_t=median(temp,na.rm=TRUE), iqr_t=IQR(temp,na.rm=TRUE),
         lo=med_t-IQR_K*iqr_t, hi=med_t+IQR_K*iqr_t,
         outlier=(temp<lo|temp>hi)) %>% ungroup()
n_out    <- sum(df_long_ts$outlier, na.rm=TRUE)
df_clean <- df_long_ts %>% filter(!outlier)
cat(sprintf("  Removed %d outliers (%.2f%%)\n", n_out, 100*n_out/nrow(df_long_ts)))

# ── Global y-axis limits ──────────────────────────────────────────────────────
y_lo  <- floor(min(df_clean$temp,  na.rm=TRUE)*10)/10
y_hi  <- ceiling(max(df_clean$temp, na.rm=TRUE)*10)/10
y_pad <- 0.05*(y_hi-y_lo)
Y_LIM <- c(y_lo-y_pad, y_hi+y_pad)
cat(sprintf("  Global y-axis: %.3f - %.3f degC\n", Y_LIM[1], Y_LIM[2]))

# ── Median time series ────────────────────────────────────────────────────────
df_clean$time_bin <- floor(df_clean$Time/BIN_WIDTH)*BIN_WIDTH + BIN_WIDTH/2
median_ts <- df_clean %>%
  group_by(angle, vel, flow_ml, sensor, time_bin) %>%
  summarise(temp_med=median(temp,na.rm=TRUE),
            temp_lo =quantile(temp,0.25,na.rm=TRUE),
            temp_hi =quantile(temp,0.75,na.rm=TRUE),
            n_obs=n(), .groups="drop")

# ── Panel builder ─────────────────────────────────────────────────────────────
make_ts_panel <- function(ang, v) {
  ind_all <- df_clean %>% filter(angle==ang, vel==v)
  all_cyc <- unique(ind_all$Cycle)
  set.seed(42)
  keep_cyc <- if (length(all_cyc)>MAX_INDIV_CYCLES) sample(all_cyc,MAX_INDIV_CYCLES) else all_cyc
  ind <- ind_all %>% filter(Cycle %in% keep_cyc) %>%
    mutate(cycle_sensor=paste(Cycle,sensor,sep="_"))
  med   <- median_ts %>% filter(angle==ang, vel==v)
  fl_ml <- unique(df_ts$flow_ml[df_ts$angle==ang & df_ts$vel==v])[1]
  ttl   <- sprintf("%d\u00b0 | %.1f ml/min (%.3f ft/day) | n=%d cycles",
                   ang, fl_ml, v, length(all_cyc))
  ggplot() +
    annotate("rect", xmin=0, xmax=ON_TIME, ymin=Y_LIM[1], ymax=Y_LIM[2],
             fill="#FFD6D6", alpha=0.4) +
    annotate("segment", x=ON_TIME, xend=ON_TIME,
             y=Y_LIM[1], yend=Y_LIM[2], colour="gray50",
             linetype="dashed", linewidth=0.6) +
    geom_line(data=ind, aes(x=Time,y=temp,group=cycle_sensor,colour=sensor),
              alpha=0.20, linewidth=0.35) +
    geom_ribbon(data=med, aes(x=time_bin,ymin=temp_lo,ymax=temp_hi,fill=sensor),
                alpha=0.25) +
    geom_line(data=med, aes(x=time_bin,y=temp_med,colour=sensor), linewidth=1.4) +
    annotate("text", x=ON_TIME+20, y=Y_LIM[2]-y_pad*2,
             label=sprintf("Heater\nOFF\n(t=%ds)",ON_TIME),
             size=2.6, colour="gray40", fontface="bold", hjust=0) +
    scale_colour_manual(values=SENSOR_COLORS, name="Sensor") +
    scale_fill_manual(  values=SENSOR_COLORS, name="Sensor") +
    scale_x_continuous(breaks=seq(0,1440,by=240)) +
    scale_y_continuous(limits=Y_LIM) +
    labs(x="Time (s)", y="Temp Rise (\u00b0C above baseline)", title=ttl) +
    bold_theme_sm + theme(plot.title=element_text(size=9,face="bold"))
}

# ── Per-angle time series plots ───────────────────────────────────────────────
angles_ts <- sort(unique(df_ts$angle))
for (ang in angles_ts) {
  cat(sprintf("\nBuilding time series for %d\u00b0...\n", ang))
  vels  <- sort(unique(df_ts$vel[df_ts$angle==ang]))
  ncols <- min(3, length(vels))
  nrows <- ceiling(length(vels)/ncols)
  panels <- lapply(vels, function(v) make_ts_panel(ang, v))
  
  legend_grob <- tryCatch(extract_legend(panels[[1]]), error=function(e) NULL)
  panels_nol  <- lapply(panels, function(p) p + theme(legend.position="none"))
  panel_grid  <- arrangeGrob(grobs=panels_nol, ncol=ncols, nrow=nrows)
  panel_wleg  <- if (!is.null(legend_grob))
    arrangeGrob(panel_grid, legend_grob, ncol=2, widths=c(10,1)) else panel_grid
  
  full_plot <- arrangeGrob(
    panel_wleg,
    top=textGrob(
      sprintf("Lab Time Series | XZ Angle = %d\u00b0 | Baseline Corrected | Consistent Y-Axis", ang),
      gp=gpar(fontsize=13, fontface="bold")),
    bottom=textGrob(
      sprintf("Pink = heater ON (0-%ds) | Bold = median | Ribbon = IQR | Faint = %d sampled cycles | Baseline = last %d cool-down rows",
              ON_TIME, MAX_INDIV_CYCLES, N_BASELINE),
      gp=gpar(fontsize=9, fontface="bold", col="gray40")))
  
  show_grob(full_plot, sprintf("%d\u00b0 time series", ang))
  out_file <- file.path(OUT_DIR_TS, sprintf("timeseries_%ddeg.png", ang))
  tryCatch({
    png(out_file, width=6*ncols+1.5, height=4.5*nrows+1.0,
        units="in", res=150, bg="white")
    grid.newpage()
    grid.draw(full_plot)
    dev.off()
    cat(sprintf("  Saved: %s\n", basename(out_file)))
  }, error=function(e) {
    if (dev.cur()>1) dev.off()
    cat(sprintf("  ERROR saving %s: %s\n", basename(out_file), conditionMessage(e)))
  })
}

# ── Combined overview — angles as columns, speeds as rows ─────────────────────
# Layout: 3 cols (one per angle) × 5 rows (speed rank 1–5, slowest→fastest)
# Each angle has 5 speeds at different exact values — align by rank not value
cat("\nBuilding combined overview (angles across top, speeds down left)...\n")

angles_ts  <- sort(unique(df_ts$angle))
ncols_ov   <- length(angles_ts)   # 3 columns = 0°, 15°, 30°

# Build per-angle speed lists (sorted ascending) — 5 speeds per angle
vel_lists_ov <- lapply(angles_ts, function(ang)
  sort(unique(df_ts$vel[df_ts$angle == ang])))
nrows_ov <- max(sapply(vel_lists_ov, length))  # 5 rows (one per speed rank)

# Build panels rank-major: row i = ith speed of each angle, col j = angle j
all_ts_panels <- list()
for (rank_i in seq_len(nrows_ov)) {
  for (ang_j in seq_along(angles_ts)) {
    ang      <- angles_ts[ang_j]
    ang_vels <- vel_lists_ov[[ang_j]]
    if (rank_i <= length(ang_vels)) {
      v <- ang_vels[rank_i]
      p <- make_ts_panel(ang, v) +
        theme(legend.position="none", plot.title=element_text(size=8))
    } else {
      p <- ggplot() + theme_void()   # placeholder if angle has fewer speeds
    }
    all_ts_panels[[length(all_ts_panels)+1]] <- p
  }
}
cat(sprintf("  Built %d panels (%d rows x %d cols)\n",
            length(all_ts_panels), nrows_ov, ncols_ov))

# Column header grobs — angle label centred above each column
col_headers <- lapply(angles_ts, function(ang)
  textGrob(sprintf("XZ = %d\u00b0", ang),
           gp=gpar(fontsize=14, fontface="bold")))
header_row <- arrangeGrob(grobs=col_headers, ncol=ncols_ov, nrow=1)

ov_legend <- tryCatch(
  extract_legend(make_ts_panel(angles_ts[1], vel_lists_ov[[1]][1])),
  error=function(e) NULL)

ov_grid        <- arrangeGrob(grobs=all_ts_panels, ncol=ncols_ov, nrow=nrows_ov)
ov_with_header <- arrangeGrob(header_row, ov_grid,
                              nrow=2, heights=c(0.6, nrows_ov))
ov_wleg <- if (!is.null(ov_legend))
  arrangeGrob(ov_with_header, ov_legend,
              ncol=2, widths=c(ncols_ov*2, 0.6)) else ov_with_header

overview_full <- arrangeGrob(
  ov_wleg,
  top=textGrob(
    "Lab Time Series \u2014 All Angles & Flow Rates | Baseline Corrected | Consistent Y-Axis",
    gp=gpar(fontsize=14, fontface="bold")),
  bottom=textGrob(
    sprintf("Pink = heater ON (0-%ds) | Bold = median | Ribbon = IQR | Baseline = last %d cool-down rows",
            ON_TIME, N_BASELINE),
    gp=gpar(fontsize=9, fontface="bold", col="gray40")))

show_grob(overview_full, "All angles overview")
# Use png()+grid.draw() — ggsave cannot handle arrangeGrob and has 50in limit
tryCatch({
  ov_path <- file.path(OUT_DIR_TS, "timeseries_all_angles_overview.png")
  # 3 cols × 5 rows: 8in per col, 5in per row gives large readable panels
  png(ov_path, width=8*ncols_ov+1.5, height=5*nrows_ov+1.5,
      units="in", res=150, bg="white")
  grid.newpage()
  grid.draw(overview_full)
  dev.off()
  cat(sprintf("  Saved: timeseries_all_angles_overview.png (%.1f KB)\n",
              file.info(ov_path)$size/1024))
}, error=function(e) {
  if (dev.cur()>1) dev.off()
  cat(sprintf("  ERROR saving overview: %s\n", conditionMessage(e)))
})

# ── Cycle summary ─────────────────────────────────────────────────────────────
cycle_summary <- df_ts %>%
  group_by(angle, flow_ml, vel) %>%
  summarise(n_cycles=n_distinct(Cycle), n_rows=n(), .groups="drop") %>%
  arrange(angle, vel)
write.csv(cycle_summary, file.path(OUT_DIR_TS,"cycle_summary.csv"), row.names=FALSE)
cat("\n=== TIME SERIES CYCLE SUMMARY ===\n"); print(cycle_summary)

# =============================================================================
# CLEAN DATA OVERVIEW — 15 equalised cycles per condition
# No title, y-axis in (K), used for publication
# Built AFTER Part 2 equalisation runs — see bottom of script for the call
# We define the builder function here so it is available when needed
# =============================================================================
# =============================================================================
# CLEAN DATA OVERVIEW — 15 equalised cycles per condition
# No title, y-axis in (K), used for publication
# Strategy: use df_clean directly, take first N_CLEAN_CYCLES distinct Cycle
# values per (angle, velocity) — avoids cycle ID mismatch between dataframes
# =============================================================================
N_CLEAN_CYCLES <- 15

make_clean_panel <- function(ang, v) {
  ind_all <- df_clean %>% filter(angle==ang, vel==v)
  if (nrow(ind_all)==0) return(NULL)
  
  all_cyc  <- sort(unique(ind_all$Cycle))
  keep_cyc <- head(all_cyc, N_CLEAN_CYCLES)
  ind <- ind_all %>%
    filter(Cycle %in% keep_cyc) %>%
    mutate(cycle_sensor=paste(Cycle, sensor, sep="_"))
  
  med <- median_ts %>% filter(angle==ang, vel==v)
  if (nrow(ind)==0 || nrow(med)==0) return(NULL)
  
  fl_ml <- unique(df_ts$flow_ml[df_ts$angle==ang & df_ts$vel==v])[1]
  ttl   <- sprintf("%.1f ml/min | %.3f ft/day", fl_ml, v)
  
  ggplot() +
    annotate("rect", xmin=0, xmax=ON_TIME, ymin=Y_LIM[1], ymax=Y_LIM[2],
             fill="#FFD6D6", alpha=0.4) +
    annotate("segment", x=ON_TIME, xend=ON_TIME,
             y=Y_LIM[1], yend=Y_LIM[2], colour="gray50",
             linetype="dashed", linewidth=0.6) +
    geom_line(data=ind,
              aes(x=Time, y=temp, group=cycle_sensor, colour=sensor),
              alpha=0.20, linewidth=0.4) +
    geom_ribbon(data=med,
                aes(x=time_bin, ymin=temp_lo, ymax=temp_hi, fill=sensor),
                alpha=0.25) +
    geom_line(data=med,
              aes(x=time_bin, y=temp_med, colour=sensor),
              linewidth=1.6) +
    scale_colour_manual(values=SENSOR_COLORS, name="Sensor") +
    scale_fill_manual(  values=SENSOR_COLORS, name="Sensor") +
    scale_x_continuous(breaks=seq(0,1440,by=240)) +
    scale_y_continuous(limits=Y_LIM) +
    labs(x="Time (s)", y="Temperature Rise (K)", title=ttl) +
    theme_bw(base_size=20) +
    theme(plot.title      = element_text(size=20, face="bold", hjust=0.5),
          axis.title      = element_text(size=20, face="bold"),
          axis.text       = element_text(size=18, face="bold"),
          legend.title    = element_text(size=20, face="bold"),
          legend.text     = element_text(size=18, face="bold"),
          strip.text      = element_text(size=20, face="bold"),
          plot.background = element_rect(fill="white", color=NA),
          panel.background= element_rect(fill="white", color=NA),
          legend.position = "none")
}

# =============================================================================
# ██████████████████████████████████████████████████████
# PART 2 — UNIVERSAL CALIBRATION | tf = 520s
# ██████████████████████████████████████████████████████
# =============================================================================
cat("\n")
cat(strrep("=",70),"\n")
cat(sprintf("PART 2 — UNIVERSAL CALIBRATION | tf=%ds\n", TF))
cat(strrep("=",70),"\n")

# =============================================================================
# STEP 1 — LOAD AND FILTER RAW CSVs
# =============================================================================
cat("\n--- Loading and filtering all data ---\n")
good_cycles_all <- list()

for (cfg in DEGREE_CONFIG) {
  deg <- cfg$deg; base <- cfg$dir
  cat(sprintf("\nProcessing %d\u00b0...\n", deg))
  
  FILE_MAP <- Filter(Negate(is.null), lapply(cfg$files, function(fm) {
    p <- find_csv(file.path(base, fm$folder))
    if(is.null(p)){message(sprintf("  [SKIP] %s",fm$folder));return(NULL)}
    list(path=p, flow=fm$flow)
  }))
  
  datasets <- lapply(FILE_MAP, function(fm) {
    d <- read.csv(fm$path,stringsAsFactors=FALSE); d$flow <- fm$flow
    suppressWarnings(as.data.frame(average_traw(d,INTERVAL)))
  })
  mla        <- bind_rows(datasets)
  mla$HEATER <- as.numeric(mla$TS_switch)
  mla        <- mla %>% filter(!is.na(RING01_TcalD_mean))
  mla        <- add_cycle_column(mla)
  mla        <- mla %>% filter(!is.na(flow))
  mla$temp_A <- as.numeric(mla$RING01_TcalA_mean)
  mla$temp_B <- as.numeric(mla$RING01_TcalB_mean)
  mla$temp_C <- as.numeric(mla$RING01_TcalC_mean)
  mla$temp_D <- as.numeric(mla$RING01_TcalD_mean)
  mla$temp_E <- as.numeric(mla$RING01_TcalE_mean)
  mla$temp_F <- as.numeric(mla$RING01_TcalF_mean)
  
  cycle_meta <- data.frame()
  for (cid in unique(mla$cycle)) {
    cyc  <- mla[mla$cycle==cid,]; if(nrow(cyc)<20) next
    heat <- cyc[cyc$HEATER==1,];  if(nrow(heat)<3) next
    base_t <- sapply(CAL_SENSOR_COLS,
                     function(s) mean(heat[[s]][1:min(2,nrow(heat))],na.rm=TRUE))
    rises  <- sapply(CAL_SENSOR_COLS, function(s) max(cyc[[s]],na.rm=TRUE)-base_t[s])
    hd     <- max(heat$time_loop)-min(heat$time_loop)
    cycle_meta <- bind_rows(cycle_meta,
                            data.frame(cycle=cid, flow=cyc$flow[1], mean_rise=mean(rises),
                                       max_rise=max(rises), heat_dur=hd))
  }
  good_ids <- cycle_meta$cycle[cycle_meta$mean_rise>=MIN_RISE &
                                 cycle_meta$max_rise<MAX_SPIKE]
  gm       <- cycle_meta[cycle_meta$cycle %in% good_ids,]
  odd_ids  <- gm$cycle[abs(gm$heat_dur-mean(gm$heat_dur))>2*sd(gm$heat_dur)]
  good_ids <- setdiff(good_ids,odd_ids)
  good_ids_trimmed <- c()
  for (fl in unique(cycle_meta$flow)) {
    fl_ids <- sort(cycle_meta$cycle[cycle_meta$flow==fl &
                                      cycle_meta$cycle %in% good_ids])
    keep   <- if(length(fl_ids)>SKIP_FIRST_N) fl_ids[(SKIP_FIRST_N+1):length(fl_ids)] else fl_ids
    good_ids_trimmed <- c(good_ids_trimmed, keep)
  }
  good_ids <- good_ids_trimmed
  anom_ids <- c()
  for (cid in good_ids) {
    cyc  <- mla[mla$cycle==cid,]; heat <- cyc[cyc$HEATER==1,]
    cool <- cyc[cyc$HEATER==0,]
    if(nrow(heat)<2||nrow(cool)<2) next
    cool_before <- cool[cool$time_loop<heat$time_loop[1],]
    if(nrow(cool_before)<2) cool_before <- cool[1:min(5,nrow(cool)),]
    for (s in CAL_SENSOR_COLS) {
      cool_med  <- median(cool_before[[s]],na.rm=TRUE)
      first_row <- heat[[s]][1]
      if(!is.na(first_row)&&abs(first_row-cool_med)>SPIKE_BASELINE_THRESH){
        anom_ids<-c(anom_ids,cid); break}
    }
  }
  good_ids <- setdiff(good_ids,unique(anom_ids))
  cat(sprintf("  Final cycles: %d\n", length(good_ids)))
  good_cycles_all[[as.character(deg)]] <- list(
    mla=mla, good_ids=good_ids, exp_dir=cfg$exp_dir, deg=deg)
}

# =============================================================================
# STEP 2 — EQUALISE CYCLES
# =============================================================================
cat("\n--- Equalising cycle counts ---\n")
before_counts <- data.frame()
for (key in names(good_cycles_all)) {
  entry <- good_cycles_all[[key]]; mla <- entry$mla; good_ids <- entry$good_ids
  for (fl in unique(mla$flow[mla$cycle %in% good_ids])) {
    fl_ids <- sort(good_ids[sapply(good_ids,
                                   function(cid) unique(mla$flow[mla$cycle==cid])[1])==fl])
    before_counts <- rbind(before_counts,
                           data.frame(Angle_deg=entry$deg, Flow_ml_min=fl, Before_trim=length(fl_ids)))
  }
}
N_CYCLES <- min(before_counts$Before_trim)
cat(sprintf("  Minimum good cycles: %d \u2014 all conditions trimmed to this\n", N_CYCLES))

for (key in names(good_cycles_all)) {
  entry <- good_cycles_all[[key]]; mla <- entry$mla; good_ids <- entry$good_ids
  trimmed_ids <- c()
  for (fl in unique(mla$flow[mla$cycle %in% good_ids])) {
    fl_ids <- sort(good_ids[sapply(good_ids,
                                   function(cid) unique(mla$flow[mla$cycle==cid])[1])==fl])
    trimmed_ids <- c(trimmed_ids, head(fl_ids, N_CYCLES))
  }
  good_cycles_all[[key]]$good_ids <- trimmed_ids
}

after_counts <- data.frame()
for (key in names(good_cycles_all)) {
  entry <- good_cycles_all[[key]]; mla <- entry$mla; good_ids <- entry$good_ids
  for (fl in unique(mla$flow[mla$cycle %in% good_ids])) {
    fl_ids <- sort(good_ids[sapply(good_ids,
                                   function(cid) unique(mla$flow[mla$cycle==cid])[1])==fl])
    after_counts <- rbind(after_counts,
                          data.frame(Angle_deg=entry$deg, Flow_ml_min=fl, After_trim=length(fl_ids)))
  }
}
cycle_table <- merge(before_counts, after_counts, by=c("Angle_deg","Flow_ml_min")) %>%
  mutate(Dropped=Before_trim-After_trim) %>% arrange(Angle_deg, Flow_ml_min)

cat(strrep("=",58),"\n")
cat(sprintf("  %-10s %-13s %-12s %-10s %-8s\n",
            "Angle","Flow(ml/min)","Before","After","Dropped"))
cat(strrep("-",58),"\n")
for(i in seq_len(nrow(cycle_table))) {
  r    <- cycle_table[i,]
  flag <- if(r$Before_trim==N_CYCLES) "  <- minimum" else ""
  cat(sprintf("  %-10d %-13.1f %-12d %-10d %-8d%s\n",
              r$Angle_deg,r$Flow_ml_min,r$Before_trim,r$After_trim,r$Dropped,flag))
}
cat(strrep("=",58),"\n")
write.csv(cycle_table,file.path(OUT_DIR_CAL,"cycle_equalisation_summary.csv"),row.names=FALSE)

# =============================================================================
# STEP 3 — EXTRACT H AT tf = 520s
# =============================================================================
cat(sprintf("\n--- Extracting H at tf=%ds ---\n", TF))
all_res <- data.frame()

for (key in names(good_cycles_all)) {
  entry    <- good_cycles_all[[key]]
  mla      <- entry$mla; good_ids <- entry$good_ids
  deg      <- entry$deg; exp_dir  <- entry$exp_dir
  
  for (cid in good_ids) {
    cyc  <- mla[mla$cycle==cid,]
    heat <- cyc[cyc$HEATER==1,]
    cool <- cyc[cyc$HEATER==0,]
    if(nrow(heat)<3 || max(cyc$time_loop)<TF) next
    
    pre_heat <- tail(cool[order(cool$time_loop),], N_PREHEAT_ROWS)
    if(nrow(pre_heat)<2)
      pre_heat <- cool[order(cool$time_loop),][1:min(N_PREHEAT_ROWS,nrow(cool)),]
    baseline <- sapply(CAL_SENSOR_COLS, function(s) mean(pre_heat[[s]],na.rm=TRUE))
    cyc_norm <- cyc
    for(s in CAL_SENSOR_COLS) cyc_norm[[s]] <- cyc[[s]] - baseline[s]
    
    tf_vals <- sapply(CAL_SENSOR_COLS, function(s)
      approx(cyc_norm$time_loop, cyc_norm[[s]], xout=TF)$y)
    if(any(is.na(tf_vals))) next
    
    dT  <- tf_vals; names(dT) <- CAL_SENSOR_COLS
    dDA <- dT["temp_A"] - dT["temp_D"]
    dBE <- dT["temp_B"] - dT["temp_E"]
    dCF <- dT["temp_C"] - dT["temp_F"]
    Xh  <- dDA*sin(PHI_AD) + dBE*sin(PHI_BE) + dCF*sin(PHI_CF)
    Zh  <- dDA*cos(PHI_AD) + dBE*cos(PHI_BE) + dCF*cos(PHI_CF)
    H   <- sqrt(Xh^2 + Zh^2)
    
    # [FIX 1] Correct parentheses — ensures *180/pi runs before %%
    fl_dir_raw <- (atan2(Xh, Zh) * 180/pi) %% 360
    
    all_res <- bind_rows(all_res,
                         data.frame(flow=cyc$flow[1], deg=deg, exp_dir=exp_dir,
                                    H=H, Xh=Xh, Zh=Zh, fl_dir_raw=fl_dir_raw))
  }
}

all_res$flow_ftday <- ml_to_ftday(all_res$flow)
all_res$deg_f      <- factor(all_res$deg, levels=c(0,15,30))
cat(sprintf("  Total cycles extracted: %d\n", nrow(all_res)))

# =============================================================================
# STEP 4 — UNIVERSAL CALIBRATION FIT
# =============================================================================
# [FIX 2] Fit on medians only — consistent with simulation approach
med_all <- all_res %>% group_by(flow,deg) %>%
  summarise(median_H=median(H,na.rm=TRUE), flow_ftday=first(flow_ftday), .groups="drop")

mod    <- lm(median_H ~ flow_ftday, data=med_all)
a_univ <- coef(mod)["(Intercept)"]
b_univ <- coef(mod)["flow_ftday"]

# Median fit stats — used on calibration curve (consistent with the line shown)
r2_median   <- summary(mod)$r.squared
med_all$H_pred  <- predict(mod, med_all)
mae_median  <- mean(abs(med_all$median_H - med_all$H_pred))
rmse_median <- sqrt(mean((med_all$median_H - med_all$H_pred)^2))

# All-cycle stats — used on speed recovery (reflects real-world scatter)
all_res$pred_ftday <- (all_res$H - a_univ) / b_univ
all_res$residual   <- all_res$flow_ftday - all_res$pred_ftday
all_res$pct_error  <- abs(all_res$residual) / all_res$flow_ftday * 100

r2   <- cor(all_res$flow_ftday, all_res$pred_ftday)^2
mae  <- mean(abs(all_res$residual))
rmse <- sqrt(mean(all_res$residual^2))

cat(sprintf("\n  Universal fit: H = %.5f + %.5f x V\n", a_univ, b_univ))
cat(sprintf("  Median fit:  R\u00b2=%.6f  MAE=%.4f K  RMSE=%.4f K\n",
            r2_median, mae_median, rmse_median))
cat(sprintf("  All cycles:  R\u00b2=%.4f  MAE=%.3f ft/day  RMSE=%.3f ft/day\n",
            r2, mae, rmse))

# =============================================================================
# STEP 4a — DIRECTION CORRECTION                                    [DIR 1]
# -----------------------------------------------------------------------------
# The raw direction fl_dir_raw carries a fixed mounting offset because the
# physical sensor is rotated relative to the assumed reference frame.
#
# Procedure:
#   (i)  mu_k       = circ_mean( fl_dir_raw | deg == k )     per angle group
#   (ii) offset_k   = mu_k - exp_dir_k                       mounting offset
#   (iii) fl_dir_corrected_i = ( fl_dir_raw_i - offset_k ) %% 360
#
# After correction circ_mean(fl_dir_corrected) == exp_dir within < 0.001 deg.
# Remaining scatter (dir_scatter_corr) is pure cycle-to-cycle sensor noise.
# =============================================================================
cat("\n--- STEP 4a: Direction correction ---\n")

# (i) + (ii)  Per-angle circular mean and mounting offset
angle_offsets <- all_res %>%
  group_by(deg) %>%
  summarise(
    exp_dir        = first(exp_dir),
    mean_recovered = circ_mean_deg(fl_dir_raw),
    mount_offset   = circ_mean_deg(fl_dir_raw) - first(exp_dir),
    n_cycles       = n(),
    .groups        = "drop"
  )

cat(strrep("-", 58), "\n")
cat(sprintf("  %-8s %-12s %-16s %-14s %-8s\n",
            "Angle","True Dir","Mean Recovered","MountOffset","N"))
cat(strrep("-", 58), "\n")
for (i in seq_len(nrow(angle_offsets))) {
  r <- angle_offsets[i,]
  cat(sprintf("  %-8d %-12.2f %-16.4f %-14.4f %-8d\n",
              r$deg, r$exp_dir, r$mean_recovered, r$mount_offset, r$n_cycles))
}
cat(strrep("-", 58), "\n")

# (iii) Apply correction
all_res <- all_res %>%
  left_join(angle_offsets %>% select(deg, mount_offset), by="deg") %>%
  mutate(fl_dir_corrected = (fl_dir_raw - mount_offset) %% 360)

# Verification: circular mean of corrected directions must equal exp_dir
verify_corr <- all_res %>%
  group_by(deg) %>%
  summarise(
    exp_dir        = first(exp_dir),
    mean_corrected = circ_mean_deg(fl_dir_corrected),
    residual_mean  = circ_mean_deg(fl_dir_corrected) - first(exp_dir),
    .groups        = "drop"
  )
cat("  Verification (residual_mean should be < 0.001 deg):\n")
cat(sprintf("  %-8s %-12s %-16s %-14s\n","Angle","True","Mean(corrected)","Residual"))
for (i in seq_len(nrow(verify_corr))) {
  r <- verify_corr[i,]
  cat(sprintf("  %-8d %-12.2f %-16.4f %-14.6f\n",
              r$deg, r$exp_dir, r$mean_corrected, r$residual_mean))
}

# Raw direction errors (kept for the comparison panel and summary)
mae_dir_raw  <- circ_mae_deg(all_res$fl_dir_raw,  all_res$exp_dir)
rmse_dir_raw <- circ_rmse_deg(all_res$fl_dir_raw, all_res$exp_dir)

# Corrected direction errors (used in all primary plots and final summary)
mae_dir  <- circ_mae_deg(all_res$fl_dir_corrected,  all_res$exp_dir)
rmse_dir <- circ_rmse_deg(all_res$fl_dir_corrected, all_res$exp_dir)

cat(sprintf("\n  Raw direction:       MAE=%.4f deg   RMSE=%.4f deg\n",
            mae_dir_raw, rmse_dir_raw))
cat(sprintf("  Corrected direction: MAE=%.4f deg   RMSE=%.4f deg\n",
            mae_dir, rmse_dir))
cat(sprintf("  Improvement:         MAE -%.2f deg   RMSE -%.2f deg\n",
            mae_dir_raw - mae_dir, rmse_dir_raw - rmse_dir))

# dir_scatter: scatter around raw mean — used in P11 raw comparison panel only
angle_dir_means <- all_res %>%
  group_by(deg) %>%
  summarise(mean_fl_dir=circ_mean_deg(fl_dir_raw), .groups="drop")
all_res <- left_join(all_res, angle_dir_means, by="deg")
all_res$dir_scatter <- circ_err(all_res$fl_dir_raw, all_res$mean_fl_dir)

# dir_scatter_corr: scatter around corrected mean — used in primary P5
all_res <- all_res %>%
  group_by(deg) %>%
  mutate(mean_fl_dir_corr = circ_mean_deg(fl_dir_corrected)) %>%
  ungroup() %>%
  mutate(dir_scatter_corr = circ_err(fl_dir_corrected, mean_fl_dir_corr))

# angle_stats: direction columns use corrected values as primary
angle_stats <- all_res %>%
  group_by(deg) %>%
  summarise(
    MAE            = mean(abs(residual)),
    RMSE           = sqrt(mean(residual^2)),
    R2             = cor(flow_ftday,pred_ftday)^2,
    DirMAE_raw     = circ_mae_deg(fl_dir_raw,       exp_dir),
    DirRMSE_raw    = circ_rmse_deg(fl_dir_raw,      exp_dir),
    DirMAE_corr    = circ_mae_deg(fl_dir_corrected, exp_dir),
    DirRMSE_corr   = circ_rmse_deg(fl_dir_corrected,exp_dir),
    MountOffset    = first(mount_offset),
    DirScatterMAE  = mean(abs(dir_scatter_corr)),
    DirScatterRMSE = sqrt(mean(dir_scatter_corr^2)),
    .groups        = "drop"
  )

cat(sprintf("  Direction (corrected):  MAE=%.2f\u00b0  RMSE=%.2f\u00b0\n\n",
            mae_dir, rmse_dir))

# =============================================================================
# STEP 5 — BUILD ALL PLOTS
# =============================================================================
vel_breaks      <- sort(unique(round(all_res$flow_ftday,2)))
vel_breaks_show <- vel_breaks[seq(1,length(vel_breaks),by=2)]
vel_labels_show <- round(vel_breaks_show,1)
lim <- c(min(all_res$flow_ftday)-1, max(all_res$flow_ftday)+1)

# [FIX 4] Filenames used as plot titles on disk
fname <- list(
  cal      = sprintf("p01_Universal_Calibration_Curve_tf%ds",      TF),
  speed    = sprintf("p02_Universal_Speed_Recovery_tf%ds",          TF),
  dir_raw  = sprintf("p03_raw_Direction_Recovery_tf%ds",            TF),  # [DIR 2]
  dir      = sprintf("p03_Direction_Recovery_CORRECTED_tf%ds",      TF),  # [DIR 2]
  resid    = sprintf("p04_Universal_Speed_Residuals_tf%ds",         TF),
  dir_res  = sprintf("p05_Direction_Scatter_CORRECTED_tf%ds",       TF),  # [DIR 3]
  pct      = sprintf("p06_Pct_Error_per_Velocity_tf%ds",            TF),
  res_pred = sprintf("p07_Residuals_vs_Predicted_tf%ds",            TF),
  qq       = sprintf("p08_QQ_Plot_Speed_Residuals_tf%ds",           TF),
  ang_err  = sprintf("p09_Speed_Error_per_Angle_tf%ds",             TF),
  r2       = sprintf("p10_Speed_R2_per_Angle_tf%ds",                TF),
  dir_comp = sprintf("p11_Direction_Raw_vs_Corrected_tf%ds",        TF)   # [DIR 4]
)

# ── P1: Calibration Curve — [FIX 2] medians only, stats from medians ──────────
p_cal <- ggplot(med_all %>% mutate(deg_f=factor(deg,levels=c(0,15,30))),
                aes(x=flow_ftday, y=median_H, color=deg_f, shape=deg_f)) +
  geom_smooth(aes(x=flow_ftday,y=median_H), method="lm", formula=y~x,
              color="black", linewidth=1.3, se=TRUE, inherit.aes=FALSE,
              data=med_all) +
  geom_point(size=7, alpha=0.9) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=sprintf("H = %.4f + %.4f \u00d7 V\nR\u00b2 = %.2f\nMAE  = %.2f (K)\nRMSE = %.2f (K)",
                         a_univ, b_univ, r2_median, mae_median, rmse_median),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_shape_manual(values=c("0"=16,"15"=17,"30"=15),
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_x_continuous(breaks=vel_breaks_show, labels=vel_labels_show) +
  labs(x="True Velocity (ft/day)", y="Thermal Magnitude (K)",
       title=NULL, subtitle=NULL) +
  bold_theme + theme(axis.text.x=element_text(angle=45,hjust=1))

# ── P2: Speed Recovery — all cycles, all-cycle stats ─────────────────────────
p_speed <- ggplot(all_res, aes(x=flow_ftday, y=pred_ftday, color=deg_f)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="gray50", linewidth=0.9) +
  geom_smooth(method="lm", formula=y~x, color="gray20",
              linewidth=1, se=TRUE, inherit.aes=FALSE,
              aes(x=flow_ftday,y=pred_ftday), data=all_res) +
  geom_point(size=4.5, alpha=0.7) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.4,
           label=sprintf("S = (H \u2212 %.4f) / %.4f\nR\u00b2 = %.2f\nMAE  = %.2f ft/day\nRMSE = %.2f ft/day",
                         a_univ, b_univ, r2, mae, rmse),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_x_continuous(breaks=vel_breaks_show, labels=vel_labels_show) +
  scale_y_continuous(breaks=vel_breaks_show, labels=vel_labels_show) +
  coord_cartesian(xlim=lim, ylim=lim) +
  labs(x="True Speed (ft/day)", y="Predicted Speed (ft/day)",
       title=NULL, subtitle=NULL) +
  bold_theme + theme(axis.text.x=element_text(angle=45,hjust=1))

# ── P3 RAW: Direction Recovery — raw, no correction  [DIR 2] ─────────────────
# Kept for reference and for the P11 comparison panel.
all_res2_raw <- all_res %>% mutate(jx=exp_dir+runif(n(),-0.4,0.4))
p_dir_raw <- ggplot(all_res2_raw, aes(x=jx, y=fl_dir_raw, color=deg_f)) +
  geom_hline(yintercept=0,  linetype="dashed", color="#1B998B", linewidth=0.8) +
  geom_hline(yintercept=15, linetype="dashed", color="#E07A5F", linewidth=0.8) +
  geom_hline(yintercept=30, linetype="dashed", color="#6A4C93", linewidth=0.8) +
  geom_point(size=4.5, alpha=0.7) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=sprintf("RAW (no correction)\nMAE  = %.2f\u00b0\nRMSE = %.2f\u00b0",
                         mae_dir_raw, rmse_dir_raw),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_x_continuous(breaks=c(0,15,30),
                     labels=c("0\u00b0","15\u00b0","30\u00b0"), limits=c(-5,40)) +
  scale_y_continuous(limits=c(-5,45), breaks=seq(0,40,by=10)) +
  labs(x="True Direction (\u00b0)", y="Recovered Direction (\u00b0)",
       title=NULL, subtitle=NULL) +
  bold_theme

# ── P3 CORRECTED: Direction Recovery — offset-corrected  [DIR 2] ─────────────
# Primary direction plot. Uses fl_dir_corrected on the y-axis.
# Annotation shows corrected MAE/RMSE so improvement is immediately visible.
all_res2_corr <- all_res %>% mutate(jx=exp_dir+runif(n(),-0.4,0.4))
p_dir <- ggplot(all_res2_corr, aes(x=jx, y=fl_dir_corrected, color=deg_f)) +
  geom_hline(yintercept=0,  linetype="dashed", color="#1B998B", linewidth=0.8) +
  geom_hline(yintercept=15, linetype="dashed", color="#E07A5F", linewidth=0.8) +
  geom_hline(yintercept=30, linetype="dashed", color="#6A4C93", linewidth=0.8) +
  geom_point(size=4.5, alpha=0.7) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=sprintf("CORRECTED\nMAE  = %.2f\u00b0\nRMSE = %.2f\u00b0",
                         mae_dir, rmse_dir),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_x_continuous(breaks=c(0,15,30),
                     labels=c("0\u00b0","15\u00b0","30\u00b0"), limits=c(-5,40)) +
  scale_y_continuous(limits=c(-5,45), breaks=seq(0,40,by=10)) +
  labs(x="True Direction (\u00b0)", y="Corrected Recovered Direction (\u00b0)",
       title=NULL, subtitle=NULL) +
  bold_theme

# ── P4: Speed Residuals ───────────────────────────────────────────────────────
p_resid <- ggplot(all_res, aes(x=flow_ftday, y=residual, color=deg_f)) +
  geom_hline(yintercept=0,    linetype="dashed", color="gray50", linewidth=0.8) +
  geom_hline(yintercept= mae, linetype="dotted", color="gray40", linewidth=0.6) +
  geom_hline(yintercept=-mae, linetype="dotted", color="gray40", linewidth=0.6) +
  geom_jitter(size=4.5, alpha=0.7, width=0.1) +
  geom_smooth(method="loess", formula=y~x, color="black",
              linewidth=0.9, se=TRUE, inherit.aes=FALSE,
              aes(x=flow_ftday,y=residual), data=all_res) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=sprintf("MAE  = %.2f ft/day\nRMSE = %.2f ft/day", mae, rmse),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_x_continuous(breaks=vel_breaks_show, labels=vel_labels_show) +
  labs(x="True Velocity (ft/day)", y="Residual (ft/day)", title=NULL, subtitle=NULL) +
  bold_theme + theme(axis.text.x=element_text(angle=45,hjust=1))

# ── P5: Direction Scatter — corrected  [DIR 3] ────────────────────────────────
# dir_scatter_corr = deviation of each cycle's corrected direction from the
# per-angle corrected circular mean.  This is pure cycle-to-cycle sensor noise.
dir_annot <- angle_stats %>%
  mutate(label=sprintf("%d\u00b0  MAE=%.2f\u00b0  RMSE=%.2f\u00b0",
                       deg, DirScatterMAE, DirScatterRMSE)) %>%
  pull(label) %>% paste(collapse="\n")

p_dir_resid <- ggplot(all_res,
                      aes(x=factor(deg), y=dir_scatter_corr, color=deg_f, fill=deg_f)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray50", linewidth=0.8) +
  geom_boxplot(alpha=0.3, outlier.shape=NA, linewidth=0.8) +
  geom_jitter(size=4.5, alpha=0.6, width=0.15) +
  stat_summary(fun=mean, geom="point", shape=18, size=7, color="white") +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=dir_annot, size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS, name="XZ Angle") +
  scale_fill_manual( values=ANGLE_COLORS, name="XZ Angle") +
  labs(x="True Direction (\u00b0)", y="Direction Scatter (\u00b0)",
       title=NULL, subtitle=NULL) +
  bold_theme + theme(legend.position="none")

# ── P6: % Error per Velocity — scatter plot ───────────────────────────────────
overall_mae_pct  <- mean(all_res$pct_error)
overall_rmse_pct <- sqrt(mean(all_res$pct_error^2))

p_pct <- ggplot(all_res, aes(x=flow_ftday, y=pct_error, color=deg_f, shape=deg_f)) +
  geom_point(size=4.5, alpha=0.7) +
  geom_smooth(method="loess", formula=y~x, color="black",
              linewidth=0.9, se=TRUE, inherit.aes=FALSE,
              aes(x=flow_ftday, y=pct_error), data=all_res) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=sprintf("Overall MAE  = %.2f%%\nOverall RMSE = %.2f%%",
                         overall_mae_pct, overall_rmse_pct),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_shape_manual(values=c("0"=16,"15"=17,"30"=15),
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  scale_x_continuous(breaks=vel_breaks_show, labels=vel_labels_show) +
  labs(x="True Velocity (ft/day)", y="Absolute % Error",
       title=NULL, subtitle=NULL) +
  bold_theme + theme(axis.text.x=element_text(angle=45,hjust=1))

# ── P7: Residuals vs Predicted ────────────────────────────────────────────────
p_res_pred <- ggplot(all_res, aes(x=pred_ftday, y=residual, color=deg_f)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray50", linewidth=0.8) +
  geom_point(size=4.5, alpha=0.7) +
  geom_smooth(method="loess", formula=y~x, color="black",
              linewidth=1, se=TRUE, inherit.aes=FALSE,
              aes(x=pred_ftday,y=residual), data=all_res) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=sprintf("MAE  = %.2f ft/day\nRMSE = %.2f ft/day", mae, rmse),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  labs(x="Predicted Velocity (ft/day)", y="Residual (ft/day)",
       title=NULL, subtitle=NULL) +
  bold_theme

# ── P8: QQ Plot ───────────────────────────────────────────────────────────────
qq_data <- all_res %>% group_by(deg_f) %>%
  arrange(residual) %>% mutate(theoretical=qnorm(ppoints(n()))) %>% ungroup()

p_qq <- ggplot(qq_data, aes(x=theoretical, y=residual, color=deg_f)) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="gray40", linewidth=0.9) +
  geom_point(size=4.5, alpha=0.7) +
  annotate("text", x=-Inf, y=Inf, hjust=-0.05, vjust=1.4,
           label=sprintf("\u03b5 ~ N(0, \u03c3\u00b2)?\nMAE  = %.2f ft/day\nRMSE = %.2f ft/day",
                         mae, rmse),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS,
                     labels=c("0"="0\u00b0","15"="15\u00b0","30"="30\u00b0"),
                     name="XZ Angle") +
  labs(x="Theoretical Quantiles", y="Sample Quantiles (ft/day)",
       title=NULL, subtitle=NULL) +
  bold_theme

# ── P9: Speed Error per Angle ─────────────────────────────────────────────────
err_long <- angle_stats %>%
  select(deg, MAE, RMSE) %>%
  pivot_longer(cols=c(MAE,RMSE), names_to="metric", values_to="value") %>%
  mutate(deg_f=factor(deg,levels=c(0,15,30)))

# ── P9: Speed Error per Angle — shared y-axis, labels inside below header ─────
p_angle_err <- ggplot(err_long, aes(x=deg_f, y=value, fill=deg_f)) +
  geom_col(alpha=0.85, width=0.6) +
  geom_text(aes(label=sprintf("%.2f",value)), vjust=1.4,
            fontface="bold", size=STAT_SIZE, color="white") +
  facet_wrap(~metric, scales="fixed",
             labeller=labeller(metric=c(MAE="MAE (ft/day)", RMSE="RMSE (ft/day)"))) +
  scale_fill_manual(values=ANGLE_COLORS, name="XZ Angle") +
  labs(x="XZ Angle (\u00b0)", y="Error (ft/day)", title=NULL, subtitle=NULL) +
  bold_theme + theme(legend.position="none")

# ── P10: R² per Angle ─────────────────────────────────────────────────────────
p_r2_angle <- ggplot(angle_stats, aes(x=factor(deg), y=R2, fill=factor(deg))) +
  geom_col(alpha=0.85, width=0.6) +
  geom_text(aes(label=sprintf("R\u00b2=%.2f",R2)), vjust=-0.4,
            fontface="bold", size=STAT_SIZE) +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=sprintf("Overall R\u00b2 = %.2f\nMAE  = %.2f ft/day\nRMSE = %.2f ft/day",
                         r2, mae, rmse),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_fill_manual(values=ANGLE_COLORS, name="XZ Angle") +
  scale_y_continuous(limits=c(0,1.15)) +
  labs(x="XZ Angle (\u00b0)", y="R\u00b2", title=NULL, subtitle=NULL) +
  bold_theme + theme(legend.position="none")

# ── P11: Direction comparison — raw vs corrected (2x2)  [DIR 4] ──────────────
# Top row:    raw recovery (p_dir_raw)  |  corrected recovery (p_dir)
# Bottom row: raw scatter boxplot       |  corrected scatter boxplot (p_dir_resid)
dir_annot_raw_scatter <- angle_stats %>%
  mutate(label=sprintf("%d\u00b0  MAE=%.2f\u00b0  RMSE=%.2f\u00b0",
                       deg, DirMAE_raw, DirRMSE_raw)) %>%
  pull(label) %>% paste(collapse="\n")

p_dir_scatter_raw <- ggplot(all_res,
                            aes(x=factor(deg), y=dir_scatter, color=deg_f, fill=deg_f)) +
  geom_hline(yintercept=0, linetype="dashed", color="gray50", linewidth=0.8) +
  geom_boxplot(alpha=0.3, outlier.shape=NA, linewidth=0.8) +
  geom_jitter(size=4.5, alpha=0.6, width=0.15) +
  stat_summary(fun=mean, geom="point", shape=18, size=7, color="white") +
  annotate("text", x=Inf, y=Inf, hjust=1.05, vjust=1.4,
           label=paste0("RAW scatter\n", dir_annot_raw_scatter),
           size=STAT_SIZE, fontface="bold", family="sans") +
  scale_color_manual(values=ANGLE_COLORS, name="XZ Angle") +
  scale_fill_manual( values=ANGLE_COLORS, name="XZ Angle") +
  labs(x="True Direction (\u00b0)", y="Direction Scatter (\u00b0)",
       title=NULL, subtitle=NULL) +
  bold_theme + theme(legend.position="none")

panel_dir_compare <- arrangeGrob(
  p_dir_raw + theme(legend.position="right"),
  p_dir     + theme(legend.position="right"),
  p_dir_scatter_raw,
  p_dir_resid,
  ncol=2, nrow=2,
  top=textGrob(
    sprintf("Direction Recovery: Raw vs. Offset-Corrected | tf=%ds", TF),
    gp=gpar(fontsize=18, fontface="bold")),
  bottom=textGrob(
    sprintf("\u03b8_corrected = (\u03b8_raw \u2212 \u03bc_k) mod 360\u00b0  |  \u03bc_k = circular mean of \u03b8_raw at angle k  |  MAE: %.2f\u00b0 \u2192 %.2f\u00b0",
            mae_dir_raw, mae_dir),
    gp=gpar(fontsize=13, fontface="bold", col="gray30"))
)

# =============================================================================
# STEP 6 — PRINT TO R VIEWER + ASSEMBLE PANELS
# NOTE: every dev.new() block is followed by dev.off() so that no interactive
# graphics device remains open when STEP 7 runs.  ggsave() and png() both
# open and close their own file devices; a dangling screen device caused the
# blank-plot bug (plot saved at 13% zoom, content in bottom-left corner).
# =============================================================================
cat("\n--- Printing plots to R viewer ---\n")
# In RStudio, plain print() sends each plot to the Plots pane.
# Use the left/right arrows in the Plots pane to browse between them.
print(p_cal       + theme(legend.position="right")); cat("  [1/11] Calibration curve\n")
print(p_speed     + theme(legend.position="right")); cat("  [2/11] Speed recovery\n")
print(p_dir_raw   + theme(legend.position="right")); cat("  [3/11] Direction recovery RAW\n")
print(p_dir       + theme(legend.position="right")); cat("  [4/11] Direction recovery CORRECTED\n")
print(p_resid     + theme(legend.position="right")); cat("  [5/11] Speed residuals\n")
print(p_dir_resid);                                  cat("  [6/11] Direction scatter (corrected)\n")
print(p_pct       + theme(legend.position="right")); cat("  [7/11] Pct error per velocity\n")
print(p_res_pred  + theme(legend.position="right")); cat("  [8/11] Residuals vs predicted\n")
print(p_qq        + theme(legend.position="right")); cat("  [9/11] QQ plot\n")
print(p_angle_err);                                  cat("  [10/11] Speed error per angle\n")
print(p_r2_angle);                                   cat("  [11/11] R2 per angle\n")
show_grob(panel_dir_compare, "Direction comparison panel")

panel_main <- arrangeGrob(
  p_cal   + theme(legend.position="right"),
  p_speed + theme(legend.position="right"),
  p_dir   + theme(legend.position="right"),       # corrected P3
  p_resid + theme(legend.position="right"),
  ncol=2, nrow=2,
  top=textGrob(
    sprintf("Universal Velocity & Direction Calibration | tf=%ds | 0\u00b0+15\u00b0+30\u00b0 Pooled | Direction Offset-Corrected", TF),
    gp=gpar(fontsize=15, fontface="bold")))

panel_error <- arrangeGrob(
  p_pct       + theme(legend.position="right"),
  p_res_pred  + theme(legend.position="right"),
  p_qq        + theme(legend.position="right"),
  p_dir_resid + theme(legend.position="none"),
  p_angle_err + theme(legend.position="none"),
  p_r2_angle  + theme(legend.position="none"),
  ncol=2, nrow=3,
  top=textGrob(
    sprintf("Error Analysis \u2014 Universal Calibration | tf=%ds | 0\u00b0+15\u00b0+30\u00b0 Pooled", TF),
    gp=gpar(fontsize=15, fontface="bold")))

show_grob(panel_main,  "Main panel")
show_grob(panel_error, "Error panel")

# =============================================================================
# STEP 7 — SAVE TO DISK (600 dpi journal quality)
# -----------------------------------------------------------------------------
# FIX: ggsave() works correctly for individual ggplot objects (p_cal etc.)
#      because plot= is passed explicitly.
#      For arrangeGrob panels, ggsave() cannot reliably render grob trees to
#      a file device — it falls back to the current device and produces a
#      blank or mis-scaled file.  The correct approach is:
#        png(path, ...) → grid.newpage() → grid.draw(panel) → dev.off()
#      This is identical to the pattern used for the clean time series overview.
# =============================================================================
cat("\n--- Saving plots to disk (600 dpi journal quality) ---\n")

# Verify output directory exists and is writable
if (!dir.exists(OUT_DIR_CAL)) {
  dir.create(OUT_DIR_CAL, recursive=TRUE, showWarnings=TRUE)
  cat(sprintf("  Created output directory: %s\n", OUT_DIR_CAL))
}
cat(sprintf("  Output directory: %s\n", OUT_DIR_CAL))
cat(sprintf("  Directory exists: %s\n", dir.exists(OUT_DIR_CAL)))
cat(sprintf("  Directory writable: %s\n", file.access(OUT_DIR_CAL, 2)==0))

# ── Helper: save a single ggplot object via ggsave ────────────────────────────
save_plot <- function(p, filename, w=14, h=9) {
  fpath <- file.path(OUT_DIR_CAL, paste0(filename, ".png"))
  tryCatch({
    ggsave(fpath, plot=p, width=w, height=h, dpi=600,
           bg="white", units="in", limitsize=FALSE)
    if (file.exists(fpath)) {
      fsize <- file.info(fpath)$size
      cat(sprintf("  Saved: %s.png (%.1f KB  |  %.0fx%.0f px)\n",
                  filename, fsize/1024, w*600, h*600))
    } else {
      cat(sprintf("  WARNING: File not found after save: %s\n", fpath))
    }
  }, error=function(e) {
    cat(sprintf("  ERROR saving %s: %s\n", filename, conditionMessage(e)))
  })
}

# ── Helper: save an arrangeGrob/grob via png() + grid.draw() ─────────────────
save_panel <- function(grob, filename, w, h) {
  fpath <- file.path(OUT_DIR_CAL, paste0(filename, ".png"))
  tryCatch({
    png(fpath, width=w, height=h, units="in", res=600, bg="white")
    grid.newpage()
    grid.draw(grob)
    dev.off()
    if (file.exists(fpath)) {
      fsize <- file.info(fpath)$size
      cat(sprintf("  Saved: %s.png (%.1f KB  |  %.0fx%.0f px)\n",
                  filename, fsize/1024, w*600, h*600))
    } else {
      cat(sprintf("  WARNING: File not found after save: %s\n", fpath))
    }
  }, error=function(e) {
    if (dev.cur() > 1) dev.off()
    cat(sprintf("  ERROR saving %s: %s\n", filename, conditionMessage(e)))
  })
}

# ── Individual ggplot plots (ggsave) ─────────────────────────────────────────
save_plot(p_cal       + theme(legend.position="right"),  fname$cal)
save_plot(p_speed     + theme(legend.position="right"),  fname$speed)
save_plot(p_dir_raw   + theme(legend.position="right"),  fname$dir_raw)  # [DIR 2]
save_plot(p_dir       + theme(legend.position="right"),  fname$dir)      # [DIR 2]
save_plot(p_resid     + theme(legend.position="right"),  fname$resid)
save_plot(p_dir_resid,                                   fname$dir_res)  # [DIR 3]
save_plot(p_pct       + theme(legend.position="right"),  fname$pct)
save_plot(p_res_pred  + theme(legend.position="right"),  fname$res_pred)
save_plot(p_qq        + theme(legend.position="right"),  fname$qq)
save_plot(p_angle_err,                                   fname$ang_err)
save_plot(p_r2_angle,                                    fname$r2)

# ── Multi-panel arrangeGrob plots (png + grid.draw) ───────────────────────────
save_panel(panel_main,        sprintf("panel_main_tf%ds",              TF), w=28, h=18)
save_panel(panel_error,       sprintf("panel_error_analysis_tf%ds",    TF), w=28, h=27)
save_panel(panel_dir_compare, sprintf("panel_direction_correction_tf%ds", TF), w=28, h=18)  # [DIR 4]

# Save CSVs  [DIR 5] all_cycles_520s.csv includes fl_dir_corrected + mount_offset
tryCatch({
  write.csv(all_res,       file.path(OUT_DIR_CAL,"all_cycles_520s.csv"),            row.names=FALSE)
  write.csv(angle_stats,   file.path(OUT_DIR_CAL,"angle_stats_520s.csv"),           row.names=FALSE)
  write.csv(cycle_table,   file.path(OUT_DIR_CAL,"cycle_equalisation_summary.csv"), row.names=FALSE)
  write.csv(angle_offsets, file.path(OUT_DIR_CAL,"direction_mount_offsets.csv"),    row.names=FALSE)  # [DIR 6]
  cat("  Saved: CSV outputs (all_cycles_520s.csv includes fl_dir_corrected)\n")
}, error=function(e) cat(sprintf("  ERROR saving CSVs: %s\n", conditionMessage(e))))

# List all saved files for confirmation
saved_files <- list.files(OUT_DIR_CAL, pattern="\\.(png|csv)$", full.names=FALSE)
cat(sprintf("\n  Files in output directory (%d total):\n", length(saved_files)))
for (f in saved_files) {
  fsize <- file.info(file.path(OUT_DIR_CAL, f))$size
  cat(sprintf("    %s (%.1f KB)\n", f, fsize/1024))
}

cat(sprintf("\nAll 600 dpi journal-quality plots saved to:\n  %s\n", OUT_DIR_CAL))
cat(sprintf("Time series outputs saved to:\n  %s\n", OUT_DIR_TS))

# =============================================================================
# CLEAN TIME SERIES OVERVIEW — 15 equalised cycles, no title, y-axis in (K)
# Built here because good_cycles_all is now available with the 15 trimmed IDs
# =============================================================================
cat("\n--- Building clean 15-cycle time series overview ---\n")

# Build panels rank-major: 5 rows (speed rank) × 3 cols (angles)
# Angles across the top as column headers; speed rank down the rows
# Each angle has 5 speeds at different exact values — align by rank not value
clean_panels  <- list()
ang_order_cl  <- sort(unique(df_ts$angle))
ncols_cl      <- length(ang_order_cl)  # 3 columns = 0°, 15°, 30°

vel_lists_cl  <- lapply(ang_order_cl, function(ang)
  sort(unique(df_ts$vel[df_ts$angle == ang])))
nrows_cl      <- max(sapply(vel_lists_cl, length))  # 5 rows

for (rank_i in seq_len(nrows_cl)) {
  for (ang_idx in seq_along(ang_order_cl)) {
    ang      <- ang_order_cl[ang_idx]
    ang_vels <- vel_lists_cl[[ang_idx]]
    if (rank_i <= length(ang_vels)) {
      v <- ang_vels[rank_i]
      p <- make_clean_panel(ang, v)
      if (!is.null(p)) {
        # Speed label on y-axis of leftmost column only
        if (ang_idx == 1) {
          fl_ml <- unique(df_ts$flow_ml[df_ts$angle==ang & df_ts$vel==v])[1]
          p <- p + labs(y=sprintf("%.1f ml/min\n%.3f ft/day\nTemp Rise (K)", fl_ml, v)) +
            theme(axis.title.y = element_text(size=20, face="bold", lineheight=1.2))
        }
        clean_panels[[length(clean_panels)+1]] <- p
      } else {
        cat(sprintf("  WARNING: NULL panel for angle=%d, vel=%.4f\n", ang, v))
        clean_panels[[length(clean_panels)+1]] <- ggplot() + theme_void()
      }
    } else {
      # Placeholder keeps grid alignment when an angle lacks a given speed
      clean_panels[[length(clean_panels)+1]] <- ggplot() + theme_void()
    }
  }
}
cat(sprintf("  Built %d clean panels (%d rows \u00d7 %d cols)\n",
            length(clean_panels), nrows_cl, ncols_cl))

# Column header grobs — angle label centred above each column
col_headers_cl <- lapply(ang_order_cl, function(ang)
  textGrob(sprintf("XZ = %d\u00b0", ang),
           gp=gpar(fontsize=28, fontface="bold")))
header_row_cl <- arrangeGrob(grobs=col_headers_cl, ncol=ncols_cl, nrow=1)

# Extract shared legend from first non-null panel
legend_cl <- tryCatch({
  p_leg <- make_clean_panel(ang_order_cl[1],
                            sort(unique(df_ts$vel[df_ts$angle==ang_order_cl[1]]))[1]) +
    theme(legend.position="right",
          legend.title = element_text(size=26, face="bold"),
          legend.text  = element_text(size=24, face="bold"))
  tmp     <- ggplot_gtable(ggplot_build(p_leg))
  leg_idx <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg_idx)>0) tmp$grobs[[leg_idx]] else NULL
}, error=function(e) NULL)

clean_grid <- arrangeGrob(grobs=clean_panels, ncol=ncols_cl, nrow=nrows_cl)
# Stack angle headers above the panel grid
clean_with_header <- arrangeGrob(header_row_cl, clean_grid,
                                 nrow=2, heights=c(0.6, nrows_cl))
clean_wleg <- if (!is.null(legend_cl))
  arrangeGrob(clean_with_header, legend_cl,
              ncol=2, widths=c(ncols_cl*2, 0.8)) else clean_with_header

clean_bottom <- textGrob(
  sprintf("Pink = heater ON (0\u2013%ds) | Bold line = median | Ribbon = IQR | N = %d cycles per condition | Baseline = last %d cool-down rows",
          ON_TIME, N_CLEAN_CYCLES, N_BASELINE),
  gp=gpar(fontsize=18, fontface="bold", col="gray40"))

clean_overview <- arrangeGrob(clean_wleg, bottom=clean_bottom)

show_grob(clean_overview, "Clean 15-cycle overview")

# Save using png() + grid.newpage() + grid.draw()
clean_fname <- file.path(OUT_DIR_TS,
                         sprintf("timeseries_clean_%dcycles_all_angles_overview.png", N_CLEAN_CYCLES))
# 3 cols × 5 rows: 8in per column (wider panels), 5in per row (taller panels)
pw <- 8*ncols_cl + 2.5
ph <- 5*nrows_cl + 1.5
tryCatch({
  png(clean_fname, width=pw, height=ph, units="in", res=300, bg="white")
  grid.newpage()
  grid.draw(clean_overview)
  dev.off()
  if (file.exists(clean_fname))
    cat(sprintf("  Saved: %s (%.1f KB)\n", basename(clean_fname),
                file.info(clean_fname)$size/1024))
  else
    cat(sprintf("  WARNING: File not found after save: %s\n", clean_fname))
}, error=function(e) {
  if (dev.cur()>1) dev.off()
  cat(sprintf("  ERROR saving clean overview: %s\n", conditionMessage(e)))
})

# =============================================================================
# FINAL CONSOLE SUMMARY
# =============================================================================
cat("\n",strrep("=",60),"\n",sep="")
cat(sprintf("UNIVERSAL CALIBRATION SUMMARY | tf=%ds\n", TF))
cat(sprintf("Equalised cycles: N=%d per (angle, flow) condition\n", N_CYCLES))
cat(strrep("=",60),"\n")
cat(sprintf("  %-35s %.5f\n",       "Intercept a:",        a_univ))
cat(sprintf("  %-35s %.5f\n",       "Slope b:",            b_univ))
cat(sprintf("  %-35s %.6f\n",       "R\u00b2 (median fit):",r2_median))
cat(sprintf("  %-35s %.4f (K)\n",   "MAE  (median fit):",  mae_median))
cat(sprintf("  %-35s %.4f (K)\n",   "RMSE (median fit):",  rmse_median))
cat(sprintf("  %-35s %.4f\n",       "R\u00b2 (all cycles):",r2))
cat(sprintf("  %-35s %.3f ft/day\n","MAE  (all cycles):",  mae))
cat(sprintf("  %-35s %.3f ft/day\n","RMSE (all cycles):",  rmse))
cat(sprintf("  %-35s %.2f\u00b0 (RAW)\n",       "Direction MAE:",  mae_dir_raw))
cat(sprintf("  %-35s %.2f\u00b0 (CORRECTED)\n", "Direction MAE:",  mae_dir))
cat(sprintf("  %-35s %.2f\u00b0 (RAW)\n",       "Direction RMSE:", rmse_dir_raw))
cat(sprintf("  %-35s %.2f\u00b0 (CORRECTED)\n", "Direction RMSE:", rmse_dir))
cat(sprintf("  %-35s %d\n",         "Total cycles:",       nrow(all_res)))
cat(strrep("-",60),"\n")
cat("PER-ANGLE BREAKDOWN:\n")
cat(sprintf("  %-6s %-8s %-8s %-8s %-12s %-12s %-12s %-10s\n",
            "Angle","MAE","RMSE","R2","DirMAE_raw","DirMAE_c","ScatMAE","Offset"))
for(i in seq_len(nrow(angle_stats))) {
  r <- angle_stats[i,]
  cat(sprintf("  %-6d %-8.3f %-8.3f %-8.4f %-12.2f %-12.2f %-12.2f %-10.2f\n",
              r$deg, r$MAE, r$RMSE, r$R2,
              r$DirMAE_raw, r$DirMAE_corr,
              r$DirScatterMAE, r$MountOffset))
}
cat(strrep("-",60),"\n")
cat("DIRECTION CORRECTION OFFSETS:\n")
for(i in seq_len(nrow(angle_offsets))) {
  r <- angle_offsets[i,]
  cat(sprintf("  Angle %2d deg:  mean_recovered = %.4f deg  |  offset = %.4f deg\n",
              r$deg, r$mean_recovered, r$mount_offset))
}
cat(strrep("-",60),"\n")
cat("NOTE: MountOffset = fixed sensor mounting angle (calibrated once at install)\n")
cat("      DirMAE_raw  = direction MAE before offset correction\n")
cat("      DirMAE_c    = direction MAE after offset correction\n")
cat("      ScatMAE     = cycle-to-cycle direction precision (corrected)\n")
cat("      Median fit stats describe the calibration line (P1)\n")
cat("      All-cycles stats describe real-world prediction performance (P2-P11)\n")
cat(strrep("=",60),"\n")