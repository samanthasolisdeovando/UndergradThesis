# Aquatic Floating Chamber Flux ----
# ANOVA and Linear Models

# July 22, 2025 (Last Update: July 22, 2025)
# Kelsey McGuire, McKenzie Kuhn
# UBC Boreal-Arctic Biogeochemistry Lab (bab-lab.github.io)

# LOAD LOCAL PACKAGES ----
setwd('~/Desktop/masters/data/msc-aquatic-ch4/')
source('~/Desktop/masters/data/msc-aquatic-ch4/functions/Data.R')

# data summaries ----
### summary values ----
summary <- clean_data %>%
  group_by(veg_class) %>%
  summarise(
    n = sum(!is.na(cch4_flux_mgm2d1)),
    mean_cch4 = mean(cch4_flux_mgm2d1, na.rm = TRUE),
    sd_cch4   = sd(cch4_flux_mgm2d1, na.rm = TRUE),
    se_cch4 = sd_cch4 / sqrt(n)
  )

pp_summary <- perfect_pairs %>%
  group_by(veg_class) %>%
  summarise(
    n = sum(!is.na(cch4_flux_mgm2d1)),
    mean_cch4 = mean(cch4_flux_mgm2d1, na.rm = TRUE),
    sd_cch4   = sd(cch4_flux_mgm2d1, na.rm = TRUE),
    se_cch4 = sd_cch4 / sqrt(n)
  )

pp_summary_zone <- perfect_pairs %>%
  group_by(zone) %>%
  summarise(
    n = sum(!is.na(cch4_flux_mgm2d1)),
    mean_cch4 = mean(cch4_flux_mgm2d1, na.rm = TRUE),
    sd_cch4   = sd(cch4_flux_mgm2d1, na.rm = TRUE),
    se_cch4 = sd_cch4 / sqrt(n)
  )

pp_summary_vegzone <- perfect_pairs %>%
  group_by(zone, veg_class) %>%
  summarise(
    n = sum(!is.na(cch4_flux_mgm2d1)),
    mean_cch4 = mean(cch4_flux_mgm2d1, na.rm = TRUE),
    sd_cch4   = sd(cch4_flux_mgm2d1, na.rm = TRUE),
    se_cch4 = sd_cch4 / sqrt(n)
  )

# HISTOS ----
## entire ch4 flux ---
clean_data %>%
  ggplot() +
  geom_histogram(aes(x = cch4_flux_mgm2d1), binwidth = 5,
                 fill = "steelblue", colour = "steelblue4") + # see a very right skewed distribution
  xlab(expression(CH[4]~Flux~"["*mg~CH[4]~m^{-2}~day^{-1}*"]")) +
  ylab("Count") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

## veg ch4 flux ----
veg_ch4 %>%
  ggplot() +
  geom_histogram(aes(x = cch4_flux_mgm2d1), binwidth = 5,
                 fill = "olivedrab3", colour = "olivedrab") + # see a very right skewed distribution
  xlab(expression(CH[4]~Flux~"["*mg~CH[4]~m^{-2}~day^{-1}*"]")) +
  ylab("Count") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

## open ch4 flux ----
open_ch4 %>%
  ggplot() +
  geom_histogram(aes(x = cch4_flux_mgm2d1), binwidth = 5,
                 fill = "dodgerblue2", colour = "dodgerblue3") + # see a very right skewed distribution
  xlab(expression(CH[4]~Flux~"["*mg~CH[4]~m^{-2}~day^{-1}*"]")) +
  ylab("Count") +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

# WILCOXON-RANK ----
## DIFFUSIVE ----
### *** VEG ----
DCH4VegWRPaired <- wilcox.test(x = perf_veg_ch4$cch4_flux_mgm2d1, y = perf_open_ch4$cch4_flux_mgm2d1,
                               paired = T)
DCH4VegWRPaired # V = 1516, p-value = 4.394e-08

### get some stats on means and relationships ----
c(mean(perf_open_ch4$cch4_flux_mgm2d1), mean(perf_veg_ch4$cch4_flux_mgm2d1)) # 41.05382 (open); 99.55257 (veg)
mean(perf_veg_ch4$cch4_flux_mgm2d1) / mean(perf_open_ch4$cch4_flux_mgm2d1) # veg is 2.5 greater than open (on average!)

### boxplot of veg ----
DCH4VegWR_plot <- perfect_pairs %>%
  ggplot(aes(x = veg_class, y = cch4_flux_mgm2d1)) +
  geom_jitter(aes(colour = veg_class), alpha = 0.5,
              width = 0.35) +
  geom_boxplot(aes(fill = veg_class), alpha = 0.7,
               outlier.shape = NA) +
  geom_signif(comparisons = list(c("Open", "Vegetated")),
              map_signif_level=TRUE) +
  scale_colour_manual(values = c("Open" = "dodgerblue3",
                                 "Vegetated" = "olivedrab3")) +
  scale_fill_manual(values = c("Open" = "dodgerblue3",
                               "Vegetated" = "olivedrab3")) +
  theme_minimal()+
  xlab("Vegetation Presence") +
  ylab(expression(CH[4]~Flux~"("*mg~C~m^{-2}~day^{-1}*")")) + # changed from mg CH4 to mg C 
  theme(legend.position = "None",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))
DCH4VegWR_plot

### *** paired method of running stat ----
DCH4VegZoneWRPaired <- wilcox.test(x = perf_veg_za$cch4_flux_mgm2d1_mean, 
                                   y = perf_open_za$cch4_flux_mgm2d1_mean,
                                   paired = T)
DCH4VegZoneWRPaired # V = 221, p-value = 4.101e-05

# ANOVAS ----
## *** sites ----
DCH4SiteAOV <- aov(cch4_flux_mgm2d1 ~ site, data = clean_data)
summary(DCH4SiteAOV) # F value: 8.463 Pr(>F): 3.1e-13
TukeyHSD(DCH4SiteAOV)

### boxplot of veg by sites ----
clean_data %>%
  ggplot(aes(x = site, y = cch4_flux_mgm2d1)) +
  geom_jitter(aes(colour = veg_class),
              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8),
              alpha = 0.7) +
  geom_boxplot(aes(fill = veg_class), alpha = 0.6,
               outlier.shape = NA,  position = position_dodge(width = 0.8)) +
  scale_colour_manual(name = "Vegetation Class",
                      values = c("Open" = "dodgerblue3",
                                 "Vegetated" = "olivedrab3")) +
  scale_fill_manual(name = "Vegetation Class",
                    values = c("Open" = "dodgerblue3",
                               "Vegetated" = "olivedrab3")) +
  theme_minimal()+
  xlab("Site") +
  ylab(expression(Diffusive~CH[4]~Flux~"["*mg~C~m^{-2}~day^{-1}*"]")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

## ** zone ----
DCH4ZoneAOV <- aov(cch4_flux_mgm2d1 ~ zone * veg_class, data = clean_data)
summary(DCH4ZoneAOV) # p-value: 0.054 
TukeyHSD(DCH4ZoneAOV) # only zones 1 and 2 are significantly different (0.048)

## ** zone (perf. pairs) ----
DCH4ZoneAOVpp <- aov(cch4_flux_mgm2d1 ~ zone, data = perfect_pairs)
summary(DCH4ZoneAOVpp) # p-value: 0.00236 
TukeyHSD(DCH4ZoneAOVpp) # only zones 1 and 2 are significantly different (0.0015159)

### boxplot of veg by zone ----
perfect_pairs %>%
  ggplot(aes(x = zone, y = cch4_flux_mgm2d1)) +
  geom_jitter(aes(colour = veg_class),
              position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.8),
              alpha = 0.7) +
  geom_boxplot(aes(fill = veg_class), alpha = 0.6,
               outlier.shape = NA,  position = position_dodge(width = 0.8)) +
  # geom_signif(comparisons = list(c("1", "2"),
  #                               c("1", "3"),
  #                               c("2", "3")),
  #            map_signif_level=TRUE,
  #            step_increase = 0.1) +
  scale_colour_manual(name = "Vegetation Class",
                      values = c("Open" = "dodgerblue3",
                                 "Vegetated" = "olivedrab3")) +
  scale_fill_manual(name = "Vegetation Class",
                    values = c("Open" = "dodgerblue3",
                               "Vegetated" = "olivedrab3")) +
  theme_minimal()+
  xlab("Zone") +
  ylab(expression(Diffusive~CH[4]~Flux~"["*mg~C~m^{-2}~day^{-1}*"]")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

## zone and veg class ----
DCH4VegAOV <- aov(cch4_flux_mgm2d1 ~ zone * veg_class, data = clean_data)
summary(DCH4VegAOV)
TukeyHSD(DCH4VegAOV)

### boxplot of veg by zone ----
clean_data %>%
  ggplot(aes(x = zone, y = cch4_flux_mgm2d1)) +
  geom_jitter(aes(colour = veg_class),
              position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.8),
              alpha = 0.7) +
  geom_boxplot(aes(fill = veg_class), alpha = 0.6,
               outlier.shape = NA,  position = position_dodge(width = 0.8)) +
  # geom_signif(comparisons = list(c("1", "2"),
  #                               c("1", "3"),
  #                               c("2", "3")),
  #            map_signif_level=TRUE,
  #            step_increase = 0.1) +
  scale_colour_manual(name = "Vegetation Class",
                      values = c("Open" = "dodgerblue3",
                                 "Vegetated" = "olivedrab3")) +
  scale_fill_manual(name = "Vegetation Class",
                    values = c("Open" = "dodgerblue3",
                               "Vegetated" = "olivedrab3")) +
  theme_minimal()+
  xlab("Zone") +
  ylab(expression(CH[4]~Flux~"["*mg~CH[4]~m^{-2}~day^{-1}*"]")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

## temps ----
### daily surface v. vegclass ----
SurfTempVegWR <- wilcox.test(x = perf_open_ch4$temp_surface_daily, 
                             y = perf_veg_ch4$temp_surface_daily,
                             paired = TRUE)
SurfTempVegWR # V = 870, p-value = 0.7323; not significant
c(mean(perf_open_ch4$temp_surface_daily), mean(perf_veg_ch4$temp_surface_daily)) # surface waters warmer in veg

### daily sediment v. vegclass ----
SedTempVegWR <- wilcox.test(x = perf_open_ch4$temp_sed_daily, y = perf_veg_ch4$temp_sed_daily, 
                        paired = TRUE)
SedTempVegWR # V = 1647, p-value = 6.882e-11; mean diff -> 0.32
c(mean(perf_open_ch4$temp_sed_daily), mean(perf_veg_ch4$temp_sed_daily)) # sediments cooler in veg

### 3day surface v. vegclass ----
Surf3TempVegWR <- wilcox.test(x = perf_open_ch4$temp_surface_3day, y = perf_veg_ch4$temp_surface_3day,
                        paired = TRUE)
Surf3TempVegWR # V = 968, p-value = 0.6988; not significant
c(mean(perf_open_ch4$temp_surface_3day), mean(perf_veg_ch4$temp_surface_3day)) # surface waters warmer in veg

### 3day sediment v. vegclass ----
Sed3TempVegWR <- wilcox.test(x = perf_open_ch4$temp_sed_3day, y = perf_veg_ch4$temp_sed_3day, 
                       paired = TRUE)
Sed3TempVegWR # V = 1800, p-value = 7.077e-11; mean diff -> 0.66
c(mean(perf_open_ch4$temp_sed_3day), mean(perf_veg_ch4$temp_sed_3day)) # sediments cooler in veg

### 7day surface v. vegclass ----
Surf7TempVegWR <- wilcox.test(x = perf_open_ch4$temp_surface_7day, y = perf_veg_ch4$temp_surface_7day, # t-test needs them as their own columns - clean_data not set up for that
                         paired = TRUE)
Surf7TempVegWR # V = 1455, p-value = 7e-05; mean diff -> 0.23
c(mean(perf_open_ch4$temp_surface_7day), mean(perf_veg_ch4$temp_surface_7day)) # surface waters now cooler in veg

### 7day sediment v. vegclass ----
Sed7TempVegWR <- wilcox.test(x = perf_open_ch4$temp_sed_7day, y = perf_veg_ch4$temp_sed_7day, # t-test needs them as their own columns - clean_data not set up for that
                        paired = TRUE)
Sed7TempVegWR # V = 1830, p-value = 1.582e-11; mean diff -> 0.99391
c(mean(perf_open_ch4$temp_sed_7day), mean(perf_veg_ch4$temp_sed_7day)) # sediments cooler in veg

## other environmental vars ----
### DO v. vegclass ----
DOVegWR <- wilcox.test(x = perf_open_ch4$DO, y = perf_veg_ch4$DO, # t-test needs them as their own columns - clean_data not set up for that
                        paired = TRUE)
DOVegWR # V = 1346.5, p-value = 0.0005017; mean diff -> 1.45
c(mean(perf_open_ch4$DO), mean(perf_veg_ch4$DO)) # DO lower in veg

### ph v. vegclass ----
pHVegWR <- wilcox.test(x = perf_open_ch4$ph, y = perf_veg_ch4$ph, # t-test needs them as their own columns - clean_data not set up for that
                  paired = TRUE)
pHVegWR # V = 397, p-value = 0.3603; not significant

### conductivity v. vegclass ----
CondVegWR <- wilcox.test(x = perf_open_ch4$cond_mScm, y = perf_veg_ch4$cond_mScm, # t-test needs them as their own columns - clean_data not set up for that
                  paired = TRUE)
CondVegWR # V = 847, p-value = 0.2442; not significant

### TDS v. vegclass ----
TDSVegWR <- wilcox.test(x = perf_open_ch4$tds_mgL, y = perf_veg_ch4$tds_mgL, # t-test needs them as their own columns - clean_data not set up for that
                    paired = TRUE)
TDSVegWR # V = 590, p-value = 0.41; not significant

# REGRESSIONS ------------------------------------------------------------------
## diffusive + plant-med lms ----
diff_lms <- runlm(clean_data, cch4_flux_mgm2d1, c("depth", "ph", "cond_mScm", "tds_mgL", "wat_temp_C", "air_temp_C", "press_kPa", "DO", 
                                      "HOURLY_PRECIPITATION", "HOURLY_TEMPERATURE", "HOURLY_RELATIVE_HUMIDITY", "HOURLY_WIND_SPEED", "HOURLY_WIND_DIRECTION", "PRECIP_RGT",
                                      "light_lux_mean_BOT", "light_lux_mean_SUR", 
                                      "temp_air_daily", "temp_bottom_daily", "temp_surface_daily", "temp_sed_daily", 
                                      "temp_air_3day", "temp_bottom_3day", "temp_surface_3day", "temp_sed_3day", 
                                      "temp_air_7day", "temp_bottom_7day", "temp_surface_7day", "temp_sed_7day"))

### zone avg
diff_za_lms <- runlm(zone_avg, cch4_flux_mgm2d1_mean, c("depth_mean", "ph_mean", "cond_mScm_mean", "tds_mgL_mean", "wat_temp_C_mean", "air_temp_C_mean",
                                                     "press_kPa_mean", "DO_mean", 
                                                     "HOURLY_PRECIPITATION_mean", "HOURLY_TEMPERATURE_mean", "HOURLY_RELATIVE_HUMIDITY_mean", 
                                                     "HOURLY_WIND_SPEED_mean", "HOURLY_WIND_DIRECTION_mean", "PRECIP_RGT_mean",
                                                  "light_lux_mean_BOT_mean", "light_lux_mean_SUR_mean", 
                                                  "temp_air_daily_mean", "temp_bottom_daily_mean", "temp_surface_daily_mean", "temp_sed_daily_mean", 
                                                  "temp_air_3day_mean", "temp_bottom_3day_mean", "temp_surface_3day_mean", "temp_sed_3day_mean", 
                                                  "temp_air_7day_mean", "temp_bottom_7day_mean", "temp_surface_7day_mean", "temp_sed_7day_mean"))

### pull out sig models ----
diff_siglms <- diff_lms$sig_models.summary # depth

#### *** DEPTH ----
## model fit plots
mf_lmdepth <- modelfit(diff_lms$all_models.model$depth, 
                       xvar = diff_lms$all_models.model$depth$model[[2]], 
                       main_label = "Depth (SLR)")

## pull out information
diff_depth_p <- diff_siglms$depth$coefficients[2,4] # p: 0.02
diff_depth_r2 <- diff_siglms$depth$r.squared # r: 0.2417137

##### DEPTH PLOT ----
diff_depth_plot <- ggplot(clean_data,
                       aes(x = depth, y = cch4_flux_mgm2d1)) +
  geom_point(shape = 21, fill = "steelblue3", colour = "black") +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue4", fill = "steelblue4", alpha = 0.2) +
  xlab(expression("Depth [cm]")) +
  ylab(expression(Diffusive~CH[4]~Flux~"["*mg~C~m^{-2}~day^{-1}*"]")) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
print(diff_depth_plot)

#### *** DEPTH (NO OUT) ----
depth_modelling <- clean_data %>%
  filter(depth > 10)

depth_lm_noout <- lm(cch4_flux_mgm2d1 ~ depth, data = depth_modelling)
summary(depth_lm_noout) # SIGNIFICANT; 
# Multiple R-squared:  0.1077,	Adjusted R-squared:  0.09531
# Pr(>|t|): 0.00431 ** (for depth)

##### no outlier depth ----
diff_depth_noout_plot <- ggplot(depth_modelling,
                                aes(x = depth, y = cch4_flux_mgm2d1)) +
  geom_point(shape = 21, fill = "steelblue3", colour = "black") +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue4", fill = "steelblue4", alpha = 0.2) +
  xlab(expression("Depth [cm]")) +
  ylab(expression(Diffusive~CH[4]~Flux~"["*mg~C~m^{-2}~day^{-1}*"]")) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
print(diff_depth_noout_plot)

#### * DAILY BOTTOM TEMP ----
diff_BotTempDailylm <- diff_siglms$temp_bottom_daily
diff_BotTempDailylm # 0.0115 *; Multiple R-squared:  0.03275,	Adjusted R-squared:  0.02456 

## model fit
mf_BotTempDailylm <- modelfit( diff_lms$all_models.model$temp_bottom_daily,
                               xvar = diff_lms$all_models.model$temp_bottom_daily$model[[2]],
                               main_label = "Bottom Temp daily (SLR)")

##### daily bottom temp plot ----
diff_dailybottomlm_plot <- ggplot(clean_data,
                           aes(x = temp_bottom_daily, y = cch4_flux_mgm2d1)) +
  geom_point(shape = 21, fill = "orangered2", colour = "black") +
  geom_smooth(method = "lm", se = TRUE, colour = "orangered2", fill = "orangered2", alpha = 0.2) +
  xlab(expression("Averaged Bottom Temperature [°C Daily Average]")) +
  ylab(expression(CH[4]~Flux~"["*mg~CH[4]~m^{-2}~day^{-1}*"]")) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))
print(diff_dailybottomlm_plot)

### simple regression results - zone avg ----
diff_za_siglms <- diff_za_lms$sig_models.summary # depth

diff_za_lms$all_models.summary$depth_mean

##### *** DEPTH (za) ----
## model fit
mf_lmdepthza <- modelfit(diff_za_lms$all_models.model$depth, xvar = diff_za_lms$all_models.model$depth$model[[2]], main_label = "Depth (SLR - ZA)")

## pull out info
diff_za_depth_p <- diff_za_siglms$depth$coefficients[2,4] # p: 0.000293317
diff_za_depth_r2 <- diff_za_siglms$depth$r.squared # r: 0.4272516

#### depth za plot ----
diff_za_depth_plot <- ggplot(zone_avg,
                         aes(x = depth_mean, y = cch4_flux_mgm2d1_mean)) +
  geom_point(shape = 21, fill = "steelblue3", colour = "black") +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue4", fill = "steelblue4", alpha = 0.2) +
  xlab(expression("Depth [cm]")) +
  ylab(expression(Diffusive~CH[4]~Flux~"["*mg~C~m^{-2}~day^{-1}*"]")) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
print(diff_za_depth_plot)

## open lms ----
### entire dataset
open_lms <- runlm(open_ch4, cch4_flux_mgm2d1, c("depth", "ph", "cond_mScm", "tds_mgL", "wat_temp_C", "air_temp_C", "press_kPa", "DO", 
                                                  "HOURLY_PRECIPITATION", "HOURLY_TEMPERATURE", "HOURLY_RELATIVE_HUMIDITY", "HOURLY_WIND_SPEED", "HOURLY_WIND_DIRECTION", "PRECIP_RGT",
                                                  "light_lux_mean_BOT", "light_lux_mean_SUR", 
                                                  "temp_air_daily", "temp_bottom_daily", "temp_surface_daily", "temp_sed_daily", 
                                                  "temp_air_3day", "temp_bottom_3day", "temp_surface_3day", "temp_sed_3day", 
                                                  "temp_air_7day", "temp_bottom_7day", "temp_surface_7day", "temp_sed_7day"))

### zone avg
open_za_lms <- runlm(open_za, cch4_flux_mgm2d1_mean, c("depth_mean", "ph_mean", "cond_mScm_mean", "tds_mgL_mean", "wat_temp_C_mean", "air_temp_C_mean",
                                                        "press_kPa_mean", "DO_mean", 
                                                        "HOURLY_PRECIPITATION_mean", "HOURLY_TEMPERATURE_mean", "HOURLY_RELATIVE_HUMIDITY_mean", 
                                                        "HOURLY_WIND_SPEED_mean", "HOURLY_WIND_DIRECTION_mean", "PRECIP_RGT_mean",
                                                        "light_lux_mean_BOT_mean", "light_lux_mean_SUR_mean", 
                                                        "temp_air_daily_mean", "temp_bottom_daily_mean", "temp_surface_daily_mean", "temp_sed_daily_mean", 
                                                        "temp_air_3day_mean", "temp_bottom_3day_mean", "temp_surface_3day_mean", "temp_sed_3day_mean", 
                                                        "temp_air_7day_mean", "temp_bottom_7day_mean", "temp_surface_7day_mean", "temp_sed_7day_mean"))


### pull out sig models ----
### simple regression results ----
open_siglms <- open_lms$sig_models.summary # nada
open_za_siglms <- open_za_lms$sig_models.summary # air temp (hanna)

## MLMs ----
### entire dataset ----
diff_mlms <- runmlm(clean_data, cch4_flux_mgm2d1, c("depth", "ph", "cond_mScm", "tds_mgL", "wat_temp_C", "air_temp_C", "press_kPa", "DO", 
                                                  "HOURLY_PRECIPITATION", "HOURLY_TEMPERATURE", "HOURLY_RELATIVE_HUMIDITY", "HOURLY_WIND_SPEED", "HOURLY_WIND_DIRECTION", "PRECIP_RGT",
                                                  "light_lux_mean_BOT", "light_lux_mean_SUR", 
                                                  "temp_air_daily", "temp_bottom_daily", "temp_surface_daily", "temp_sed_daily", 
                                                  "temp_air_3day", "temp_bottom_3day", "temp_surface_3day", "temp_sed_3day", 
                                                  "temp_air_7day", "temp_bottom_7day", "temp_surface_7day", "temp_sed_7day"),
                    random_effect = "zone")

### zone avg
diff_za_mlms <- runmlm(zone_avg, cch4_flux_mgm2d1_mean, c("depth_mean", "ph_mean", "cond_mScm_mean", "tds_mgL_mean", "wat_temp_C_mean", "air_temp_C_mean",
                                                        "press_kPa_mean", "DO_mean", 
                                                        "HOURLY_PRECIPITATION_mean", "HOURLY_TEMPERATURE_mean", "HOURLY_RELATIVE_HUMIDITY_mean", 
                                                        "HOURLY_WIND_SPEED_mean", "HOURLY_WIND_DIRECTION_mean", "PRECIP_RGT_mean",
                                                        "light_lux_mean_BOT_mean", "light_lux_mean_SUR_mean", 
                                                        "temp_air_daily_mean", "temp_bottom_daily_mean", "temp_surface_daily_mean", "temp_sed_daily_mean", 
                                                        "temp_air_3day_mean", "temp_bottom_3day_mean", "temp_surface_3day_mean", "temp_sed_3day_mean", 
                                                        "temp_air_7day_mean", "temp_bottom_7day_mean", "temp_surface_7day_mean", "temp_sed_7day_mean"),
                       random_effect = "zone")

#### pull out sig models ----
#### mixed linear regression results ----
diff_sigmlms <- diff_mlms$sig_models$summary # depth

#### mixed linear regression results (zone avg) ----
diff_za_sigmlms <- diff_za_mlms$sig_models$summary # depth
diff_za_sigmlms$
#### *** DEPTH ----
diff_depth_mlm <- lmerTest::lmer(cch4_flux_mgm2d1 ~ depth + (1|zone/subsample), # fit just random slope - performs better
                                 data = clean_data) # issues with singularity - overfitting

diff_depth_mlm2 <- lmerTest::lmer(cch4_flux_mgm2d1 ~ depth + (1 + zone|subsample), # fit random intercept and slope
                                  data = clean_data)

diff_depth_mlm3 <- lmerTest::lmer(cch4_flux_mgm2d1 ~ depth + (1|zone), # fit just random slope - performs best
                                  data = clean_data) # no singularity issues.

anova(diff_depth_mlm, diff_depth_mlm2, diff_depth_mlm3)

#### *** DEPTH (ZA) ----
# model
diff_depth_za_mlm <- diff_za_mlms$all_models$model$depth_mean

# summary
diff_depth_za_mlm_sum <- diff_za_mlms$all_models$summary$depth_mean

# coefficients
diff_depth_za_mlm_sum$coefficients # p- 2.556665e-03
performance::r2(diff_depth_za_mlm) # Conditional R2: 0.340; Marginal R2: 0.316

### open ----
open_mlms <- runmlm(open_ch4, cch4_flux_mgm2d1, c("depth", "ph", "cond_mScm", "tds_mgL", "wat_temp_C", "air_temp_C", "press_kPa", "DO", 
                                                    "HOURLY_PRECIPITATION", "HOURLY_TEMPERATURE", "HOURLY_RELATIVE_HUMIDITY", "HOURLY_WIND_SPEED", "HOURLY_WIND_DIRECTION", "PRECIP_RGT",
                                                    "light_lux_mean_BOT", "light_lux_mean_SUR", 
                                                    "temp_air_daily", "temp_bottom_daily", "temp_surface_daily", "temp_sed_daily", 
                                                    "temp_air_3day", "temp_bottom_3day", "temp_surface_3day", "temp_sed_3day", 
                                                    "temp_air_7day", "temp_bottom_7day", "temp_surface_7day", "temp_sed_7day"),
                    random_effect = "zone") # fix singularity?

### zone avg
open_za_mlms <- runmlm(open_za, cch4_flux_mgm2d1_mean, c("depth_mean", "ph_mean", "cond_mScm_mean", "tds_mgL_mean", "wat_temp_C_mean", "air_temp_C_mean",
                                                          "press_kPa_mean", "DO_mean", 
                                                          "HOURLY_PRECIPITATION_mean", "HOURLY_TEMPERATURE_mean", "HOURLY_RELATIVE_HUMIDITY_mean", 
                                                          "HOURLY_WIND_SPEED_mean", "HOURLY_WIND_DIRECTION_mean", "PRECIP_RGT_mean",
                                                          "light_lux_mean_BOT_mean", "light_lux_mean_SUR_mean", 
                                                          "temp_air_daily_mean", "temp_bottom_daily_mean", "temp_surface_daily_mean", "temp_sed_daily_mean", 
                                                          "temp_air_3day_mean", "temp_bottom_3day_mean", "temp_surface_3day_mean", "temp_sed_3day_mean", 
                                                          "temp_air_7day_mean", "temp_bottom_7day_mean", "temp_surface_7day_mean", "temp_sed_7day_mean"),
                       random_effect = "zone")

### pull out sig models ----
### mixed linear regression results ----
open_sigmlms <- open_mlms$sig_models$summary # nada
open_za_sigmlms <- open_za_mlms$sig_models$summary # air temp

#### . Air Temp ----
### simple regression results - zone avg ----
open_za_siglms <- open_za_lms$sig_models.summary # air temp

open_za_lms$all_models.summary$air_temp_C_mean

#### *** DEPTH (za) ----
## model fit
mf_lm_airtemp_za <- modelfit(open_za_lms$all_models.model$air_temp_C_mean, 
                             xvar = open_za_lms$all_models.model$air_temp_C_mean$model[[2]], 
                             main_label = "Air Temperature (HANNA) (SLR - ZA)")

## pull out info
open_za_airtemp_p <- open_za_siglms$air_temp_C_mean$coefficients[2,4] # p: 0.00938
open_za_airtemp_r2 <- open_za_siglms$air_temp_C_mean$r.squared # r: 0.17

#### LM PLOT ----
open_za_airtemp_plot <- ggplot(open_za,
                               aes(x = air_temp_C_mean, y = cch4_flux_mgm2d1_mean)) +
  geom_point(shape = 21, fill = "steelblue3", colour = "black") +
  geom_smooth(method = "lm", se = TRUE, colour = "steelblue4", fill = "steelblue4", alpha = 0.2) +
  xlab(expression("Depth [cm]")) +
  ylab(expression(Diffusive~CH[4]~Flux~"["*mg~C~m^{-2}~day^{-1}*"]")) +
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5))
print(open_za_airtemp_plot)

### MULTIPLE FIXED EFFECTS ----
diff_mp_mlm <- lmer(cch4_flux_mgm2d1_mean ~ temp_sed_7day_mean + depth_mean + (1|zone),
                    data = zone_avg)
summary(diff_mp_mlm)
# temp_sed_7day_mean - p: 0.030100 *  
# depth_mean - p: 0.000417 ***
