library(ggplot2)
library(tidyverse)
library(readr)
require(brms)
require(modelr)
require(tidybayes)
require(readxl)
require(lubridate)

workspace <- "D:/data/BoxUP/shadow paper/data/d_change"
setwd(workspace)

gradients <- read_excel("alt/glacier_gradients.xlsx") %>%
  as_tibble() %>%
  rename(Glacier = ...1)

# Read data

Sperry_gesamt <- read.table("final/data_Sperry_strtm_bereinigt.csv", 
                            dec = ",", sep = ";",
                            header = T) %>%
  as_tibble() %>%
  transmute(Year = as.numeric(.$Year),
            dh = d_Change*(tan(Elevation*pi/180) - 
                                       tan(gradients %>% 
                                             filter(Glacier == "Sperry") %>% 
                                             pull(gradient))),
           
            prev_mb = Ba_USGS,
            Glacier = "Sperry") 

Sperry_gesamt$dh_y0 <- Sperry_gesamt$dh - 
                       median(Sperry_gesamt$dh[Sperry_gesamt$Year == 2000], 
                              na.rm = T)


SouthCascade_gesamt <- read.table("final/data_SouthCascade_srtm.csv", 
                                  dec = ",", header = T) %>%
  as_tibble() %>%
  transmute(Year = .$Year,
            dh = d_Change_corrected*(tan(Elevation*pi/180) - 
                                       tan(gradients %>% 
                                             filter(Glacier == "South Cascade") %>% 
                                             pull(gradient))),
            prev_mb = Ba_USGS_sum,
            Glacier = "South Cascade") 


SouthCascade_gesamt$dh_y0 <- SouthCascade_gesamt$dh - 
  median(SouthCascade_gesamt$dh[SouthCascade_gesamt$Year == 2000], 
         na.rm = T)

Gulkana_West <- read.table("final/data_GulkanaShadowIcefallPeak_bereinigt.csv", 
                           dec = ",", sep = ";", header = T) %>%
  as_tibble() %>%
  transmute(Year = as.numeric(str_sub(Year, 1,4)),
            dh = d_Change_mit_Vorzeichen*(tan(Elevation*pi/180) - 
                             tan(gradients %>% 
                                   filter(Glacier == "Gulkana West") %>% 
                                   pull(gradient))),
            prev_mb = NA,
            Glacier = "Gulkana West") 

# Gulkana doesn't have an observation in 1999, so we normalise the values
# to 1999.

Gulkana_West$dh_y0 <- Gulkana_West$dh - 
  median(Gulkana_West$dh[Gulkana_West$Year == 1999], na.rm = T)

Gulkana_East <- read.table("final/data_GulkanaShadowOgiveMountain_bereinigt.csv", 
                           dec = ",", sep = ";", header = T) %>%
  as_tibble() %>%
  transmute(Year = as.numeric(str_sub(Year, 1,4)), 
            dh = d_Change_mit_Vorzeichen*(tan(Elevation*pi/180) - 
                             tan(gradients %>% 
                                   filter(Glacier == "Gulkana East") %>% 
                                   pull(gradient))),
            prev_mb = NA,
            Glacier = "Gulkana East") 

Gulkana_East$dh_y0 <- Gulkana_East$dh - 
  median(Gulkana_East$dh[Gulkana_East$Year == 1999], na.rm = T)


Baltoro_srtm_view_gesamt <- read.table("final/data_Baltoro_srtm_vfp_bereinigt.csv", 
                                       sep = ";", dec = ",", header = T) %>%
  as_tibble() %>%
  transmute(Year = as.numeric(Year),
            dh = d_Change*(tan(Elevation*pi/180) - 
                             tan(gradients %>% 
                                   filter(Glacier == "Baltoro") %>% 
                                   pull(gradient))),
            prev_mb = Ba_GangLi_sum,
            Glacier = "Baltoro") 

Baltoro_srtm_view_gesamt$dh_y0 <- Baltoro_srtm_view_gesamt$dh - 
  median(Baltoro_srtm_view_gesamt$dh[Baltoro_srtm_view_gesamt$Year == 2000], 
         na.rm = T)


Aletsch_gesamt <- read.table("final/data_Aletsch_swisstopo_bereinigt.csv", 
                             sep = ";", dec = ",", header = T) %>%
  as_tibble() %>%
  transmute(Year = as.numeric(Year),
            dh = d_change*(tan(Elevation*pi/180) - 
                             tan(gradients %>% 
                                   filter(Glacier == "Aletsch") %>% 
                                   pull(gradient))),
            prev_mb = Ba_WGMS_sum,
            Glacier = "Great Aletsch") 

Aletsch_gesamt$dh_y0 <- Aletsch_gesamt$dh - 
  median(Aletsch_gesamt$dh[Aletsch_gesamt$Year == 2000], 
         na.rm = T)


scale_this <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

all_glaciers <- bind_rows(Sperry_gesamt, 
                          SouthCascade_gesamt,
                          Gulkana_West,
                          Gulkana_East,
                          Baltoro_srtm_view_gesamt,
                          Aletsch_gesamt) 


ggplot(data = all_glaciers, 
       mapping = aes(x = Year,
                     dh_y0, 
                     group = Year)) + 
  geom_boxplot() +
  facet_wrap(~Glacier, 
             scales = "free", 
             ncol = 1) +
  theme_bw()

all_glaciers %>% 
  group_by(Year, Glacier) %>% 
  summarise(n = n()) %>% 
  filter(n >12) %>% 
  ungroup() %>% 
  summarise(quantile(n, c(0.025, 0.5, 0.975)))

all_glaciers %>% 
  group_by(Year, Glacier) %>% 
  summarise(l025 = quantile(dh_y0, 0.025, na.rm = T),
            median = quantile(dh_y0, 0.5, na.rm = T),
            u975 = quantile(dh_y0, 0.975, na.rm = T)) %>%
  na.omit() %>%
  View()

all_glaciers %>% 
  group_by(Glacier) %>% 
  summarise(l025   = round(quantile(dh_y0, 0.025, na.rm = T)),
            median = round(quantile(dh_y0, 0.5,   na.rm = T)),
            u975   = round(quantile(dh_y0, 0.975, na.rm = T))) %>%
  na.omit() %>%
  View()

#----------------------------------
# add linear regression with posterior 95% highest density interval
#----------------------------------

glacier.dat <- all_glaciers %>%
  filter(!is.na(Year), 
         !is.na(dh_y0)) %>%
  group_by(Glacier) %>%
  filter(dh_y0 >= quantile(dh_y0, 0.01),
         dh_y0 <= quantile(dh_y0, 0.99)) %>%
  ungroup() %>%
  mutate(year_scale = scale_this(Year),
         dh_y0_scale = scale_this(dh_y0),
         prev_mb_scale = scale_this(prev_mb)) %>% 
  mutate(id = 1: n())

bprior <- prior(normal(0, 2.5), class = "Intercept") +
  prior(normal(0, 2.5), class = "b") +
  prior(normal(0, 2.5), class = "sd") +
  prior(normal(0, 2.5), class = "sigma") +
  prior(constant(3), class = "nu")

brm.dh_y0  <- brm(dh_y0_scale ~ year_scale + ( year_scale | Glacier),
                  family  = student(),
                  data    = glacier.dat,
                  prior   = bprior,
                  cores = 3,
                  chains = 3,
                  warmup = 2000,
                  iter = 6000,
                  control = list(adapt_delta = 0.99,
                                 max_treedepth = 15),
                  backend = "cmdstanr",
                  threads = threading(3))

saveRDS(brm.dh_y0, "elev_ch_geodetic_lines_brms.RDS")

# Original parameters

sd_y       <- sd(glacier.dat$dh_y0)
mean_y     <- mean(glacier.dat$dh_y0) 
sd_x       <- sd(glacier.dat$Year)
mean_x     <- mean(glacier.dat$Year)
mod.levels <- unique(glacier.dat$Glacier)

# Define the range, for which new predictions will be made.
# ... on original scale

seq.x.orig <- seq(from = min(glacier.dat$Year)-2,
                  to =   max(glacier.dat$Year)+2,
                  length.out = 132)

# ... and on scaled range

seq.x.scaled <- (seq.x.orig - mean_x) / sd_x

# Obtain the standardized posterior predictive distribution for new observations
# and convert the predictions to original scale.

pred.tibble <- tibble(year_scale = rep(seq.x.scaled, each = length(mod.levels)),
                      Year       = rep(seq.x.orig,   each = length(mod.levels)),
                      Glacier    = rep(unique(mod.levels), times = length(seq.x.scaled)))

post_epred  <- posterior_epred(brm.dh_y0,
                               # dpar = "mu",
                               ndraws = 1000,
                               re_formula = NULL,
                               newdata = pred.tibble) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%  
  mutate(Glacier = pred.tibble$Glacier,
         Year = pred.tibble$Year ) %>%
  pivot_longer( cols = starts_with("..."), 
                values_to = "prediction") %>%
  mutate(prediction = (prediction*sd_y) + mean_y) %>%
  dplyr::select(-name) 

# Extract posterior trends

slopes.cond <- tidy_draws(brm.dh_y0) %>% 
  pivot_longer(starts_with("r_Glacier"),
               names_to = "param",
               values_to = "post") %>% 
  filter(str_detect(param, 'year_scale')) %>%
  transmute(b_year_scale, param, post) %>%
  mutate(param = str_replace_all(param, "[.]", " "),
         Glacier  = str_extract_all(param,"(?<=\\[).+(?=,)") %>% unlist(),
         post_orig = b_year_scale + post,
         post_orig = post_orig  * sd_y / sd_x) 


trend.dh_y0.hdi <- slopes.cond %>% 
  group_by(Glacier) %>% 
  summarise(med = median(post_orig), 
            q_2_5 = quantile(post_orig, 0.025), 
            q_97_5 = quantile(post_orig, 0.975)) %>% 
  mutate(lb = paste0("-", round(med - q_2_5, digits = 2)), 
         ub = paste0("+", round(q_97_5 - med, digits = 2)),
         med = round(med, digits = 2),
         hdi = paste0(med, "(", ub, "/", lb, ")")) 

min_y <- glacier.dat %>%
  group_by(Glacier) %>%
  summarise(min_y = quantile(dh_y0, 0.025))

trend.dh_y0.hdi <- left_join(trend.dh_y0.hdi, min_y, by = "Glacier")

elev.ch <- post_epred %>%
  ggplot() +
  geom_boxplot(data = glacier.dat , 
               aes(x = Year,
                   y = dh_y0,
                   group = Year),
               fill = "lightblue",
               outlier.shape = NA,
               lwd = 0.25) +
  facet_wrap(~Glacier, 
             scales = "free",
             ncol = 3) +
  stat_lineribbon(aes(x = Year, 
                      y = prediction),
                  .width = c(.95), 
                  alpha = 0.6,
                  size = 0.8,
                  point_interval = mean_qi) + 
  scale_fill_manual("",
                    labels = "95% posterior HDI",
                    values = "coral4") +
  labs(x = "Year" ,
       y = "Elevation change [m]") +
  theme_bw()   +
  geom_text(data  = trend.dh_y0.hdi,
            aes(x = 1985, y = min_y, label = hdi),
            size = 2.2,
            colour = "gray10",
            inherit.aes = FALSE,
            parse = FALSE,
            hjust = "left") +
  theme( axis.text   = element_text(size = 7),
         axis.text.x = element_text(size = 7),
         axis.title  = element_text(size = 7),
         strip.text  = element_text(size = 7),
         legend.position = "none") +
  theme(strip.background = element_blank())

ggsave(
  filename = "regression.pdf",
  plot = elev.ch,
  width = 180,
  height = 90,
  units = "mm",
  dpi = 300
)


### Previous Elevation change

dat.valid <- read.csv2(file = "D:/data/BoxUP/shadow paper/dem_validation/All_DEMs_validation.txt",
                       header = T,
                       sep = "\t",
                       dec = ",") %>% 
  as_tibble()%>%
  mutate(year = as.numeric(year),
         elevation_change = as.numeric(elevation_change),
         dh_y0 = as.numeric(dh_y0),
         year_scale = scale_this(year),
         dh_y0_scale = scale_this(dh_y0)) 

bprior <- prior(normal(0, 2.5), class = "Intercept") +
  prior(normal(0, 2.5), class = "b") +
  prior(normal(0, 2.5), class = "sd") +
  prior(normal(0, 2.5), class = "sigma") +
  prior(constant(3), class = "nu")

brm.valid  <- brm(dh_y0_scale ~ year_scale + ( year_scale | Glacier),
                  family  = student(),
                  data    = dat.valid,
                  prior   = bprior,
                  cores = 3,
                  chains = 3,
                  warmup = 2000,
                  iter = 6000,
                  control = list(adapt_delta = 0.99,
                                 max_treedepth = 15),
                  backend = "cmdstanr",
                  threads = threading(3))

saveRDS(brm.valid, "elev_validation_brms.RDS")

# Original parameters

sd_y       <- sd(dat.valid$dh_y0)
mean_y     <- mean(dat.valid$dh_y0) 
sd_x       <- sd(dat.valid$year)
mean_x     <- mean(dat.valid$year)
mod.levels <- unique(dat.valid$Glacier)

# Define the range, for which new predictions will be made.
# ... on original scale

seq.x.orig <- seq(from = min(dat.valid$year)-2,
                  to =   max(dat.valid$year)+2,
                  length.out = 132)

# ... and on scaled range

seq.x.scaled <- (seq.x.orig - mean_x) / sd_x

# Obtain the standardized posterior predictive distribution for new observations
# and convert the predictions to original scale.

pred.tibble <- tibble(year_scale = rep(seq.x.scaled, each = length(mod.levels)),
                      Year       = rep(seq.x.orig,   each = length(mod.levels)),
                      Glacier    = rep(unique(mod.levels), times = length(seq.x.scaled)))

post_epred  <- posterior_epred(brm.valid,
                               # dpar = "mu",
                               ndraws = 1000,
                               re_formula = NULL,
                               newdata = pred.tibble) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%  
  mutate(Glacier = pred.tibble$Glacier,
         Year = pred.tibble$Year ) %>%
  pivot_longer( cols = starts_with("..."), 
                values_to = "prediction") %>%
  mutate(prediction = (prediction*sd_y) + mean_y) %>%
  dplyr::select(-name) 

# Extract posterior trends

slopes.cond <- tidy_draws(brm.valid) %>% 
  pivot_longer(starts_with("r_Glacier"),
               names_to = "param",
               values_to = "post") %>% 
  filter(str_detect(param, 'year_scale')) %>%
  transmute(b_year_scale, param, post) %>%
  mutate(param = str_replace_all(param, "[.]", " "),
         Glacier  = str_extract_all(param,"(?<=\\[).+(?=,)") %>% unlist(),
         post_orig = b_year_scale + post,
         post_orig = post_orig  * sd_y / sd_x) 


trend.valid.hdi <- slopes.cond %>% 
  group_by(Glacier) %>% 
  summarise(med = median(post_orig), 
            q_2_5 = quantile(post_orig, 0.025), 
            q_97_5 = quantile(post_orig, 0.975)) %>% 
  mutate(lb = paste0("-", round(med - q_2_5, digits = 2)), 
         ub = paste0("+", round(q_97_5 - med, digits = 2)),
         med = round(med, digits = 2),
         hdi = paste0(med, "(", ub, "/", lb, ")")) 

min_y <- dat.valid %>%
  group_by(Glacier) %>%
  summarise(min_y = quantile(dh_y0, 0.025))

trend.valid.hdi <- left_join(trend.valid.hdi, min_y, by = "Glacier")

# elev.valid <- post_epred %>%
#   ggplot() +
#   facet_wrap(~Glacier, 
#              scales = "free",
#              ncol = 5) +
#   stat_lineribbon(aes(x = Year, 
#                       y = prediction),
#                   .width = c(.95), 
#                   alpha = 0.6,
#                   size = 0.8,
#                   point_interval = mean_qi) + 
#   scale_fill_manual("",
#                     labels = "95% posterior HDI",
#                     values = "darkorange4") +
#   geom_point(data = dat.valid , 
#              aes(x = year,
#                  y = dh_y0),
#              shape = 21,
#              fill = "lightblue") +
#   labs(x = "Year" ,
#        y = "Elevation change [m]") +
#   theme_bw()   +
#   geom_text(data  = trend.valid.hdi,
#             aes(x = 1960, y = min_y, label = hdi),
#             size = 2.2,
#             colour = "gray10",
#             inherit.aes = FALSE,
#             parse = FALSE,
#             hjust = "left") +
#   theme( axis.text   = element_text(size = 7),
#          axis.text.x = element_text(size = 7),
#          axis.title  = element_text(size = 7),
#          strip.text  = element_text(size = 7),
#          legend.position = "none")
# 
# ggsave(
#   filename = "regression_valid.pdf",
#   plot = elev.valid,
#   width = 180,
#   height = 50,
#   units = "mm"
# )

################################################################################

min.year <- all_glaciers %>%
  group_by(Glacier) %>%
  summarise(min_year = min(Year, na.rm = T))

dat.valid.1985 <- dat.valid %>%
  filter(! (Glacier == "Great Aletsch" & year < min.year$min_year[min.year$Glacier == "Great Aletsch"]),
         ! (Glacier == "Gulkana West" & year < min.year$min_year[min.year$Glacier == "Gulkana West"]),
         ! (Glacier == "Gulkana East" & year < min.year$min_year[min.year$Glacier == "Gulkana East"]),
         ! (Glacier == "South Cascade" & year < min.year$min_year[min.year$Glacier == "South Cascade"]),
         ! (Glacier == "Sperry" & year < min.year$min_year[min.year$Glacier == "Sperry"])) %>%
  mutate(year_scale = scale_this(year),
         dh_y0_scale = scale_this(dh_y0)) 

bprior <- prior(normal(0, 2.5), class = "Intercept") +
  prior(normal(0, 2.5), class = "b") +
  prior(normal(0, 2.5), class = "sd") +
  prior(normal(0, 2.5), class = "sigma") +
  prior(constant(3), class = "nu")

brm.valid.1985 <- brm(dh_y0_scale ~ year_scale + ( year_scale | Glacier),
                      family  = student(),
                      data    = dat.valid.1985,
                      prior   = bprior,
                      cores = 3,
                      chains = 3,
                      warmup = 2000,
                      iter = 6000,
                      control = list(adapt_delta = 0.99,
                                     max_treedepth = 15),
                      backend = "cmdstanr",
                      threads = threading(3))

saveRDS(brm.valid.1985, "elev_validation_1985_brms.RDS")

# Original parameters

sd_y       <- sd(dat.valid.1985$dh_y0)
mean_y     <- mean(dat.valid.1985$dh_y0) 
sd_x       <- sd(dat.valid.1985$year)
mean_x     <- mean(dat.valid.1985$year)
mod.levels <- unique(dat.valid.1985$Glacier)

# Define the range, for which new predictions will be made.
# ... on original scale

min.max.year <- dat.valid.1985 %>% 
  group_by(Glacier) %>% 
  summarise(min_year = min(year),
            max_year = max(year))

seq.x.orig <- mapply(function (x,y) {
  
  seq.x.orig <- seq(from = x,
                    to =   y,
                    length.out = 132)}, 
  x = min.max.year$min_year, y = min.max.year$max_year) %>% 
  c()

# ... and on scaled range

seq.x.scaled <- (seq.x.orig - mean_x) / sd_x

# Obtain the standardized posterior predictive distribution for new observations
# and convert the predictions to original scale.

pred.tibble <- tibble(year_scale = seq.x.scaled,
                      Year       = seq.x.orig,
                      Glacier    = rep(unique(mod.levels), each = 132))

post_epred.1985 <- posterior_epred(brm.valid.1985,
                                    # dpar = "mu",
                                    ndraws = 1000,
                                    re_formula = NULL,
                                    newdata = pred.tibble) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%  
  mutate(Glacier = pred.tibble$Glacier,
         Year = pred.tibble$Year ) %>%
  pivot_longer( cols = starts_with("..."), 
                values_to = "prediction") %>%
  mutate(prediction = (prediction*sd_y) + mean_y) %>%
  dplyr::select(-name) 

# Extract posterior trends

slopes.cond <- tidy_draws(brm.valid.1985) %>% 
  pivot_longer(starts_with("r_Glacier"),
               names_to = "param",
               values_to = "post") %>% 
  filter(str_detect(param, 'year_scale')) %>%
  transmute(b_year_scale, param, post) %>%
  mutate(param = str_replace_all(param, "[.]", " "),
         Glacier  = str_extract_all(param,"(?<=\\[).+(?=,)") %>% unlist(),
         post_orig = b_year_scale + post,
         post_orig = post_orig  * sd_y / sd_x) 


trend.valid.1985.hdi <- slopes.cond %>% 
  group_by(Glacier) %>% 
  summarise(med    = median(post_orig), 
            q_2_5  = quantile(post_orig, 0.025), 
            q_97_5 = quantile(post_orig, 0.975)) %>% 
  mutate(lb  = paste0("-", round(med - q_2_5, digits = 2)), 
         ub  = paste0("+", round(q_97_5 - med, digits = 2)),
         med = round(med, digits = 2),
         hdi = paste0(med, "(", ub, "/", lb, ")")) 

min_y <- dat.valid.1985 %>%
  group_by(Glacier) %>%
  summarise(min_y = quantile(dh_y0, 0.025))

trend.valid.1985.hdi <- left_join(trend.valid.1985.hdi, min_y, by = "Glacier")

elev.valid.1985 <- rbind(post_epred.1985 %>% mutate(period = "gt1985"),
                         post_epred %>% mutate(period = "all")) %>%
  mutate(period = as_factor(period)) %>%
  arrange(desc(period)) %>%
  ggplot() +
  facet_wrap(~Glacier, 
             scales = "free",
             ncol = 5) +
  stat_lineribbon(aes(x = Year, 
                      y = prediction, 
                      color = ordered(period),
                      fill = ordered(period)), 
                  .width = c(.95), 
                  alpha = 0.5) + 
  scale_fill_manual("Posterior\ninterval", 
                    values =  c("navy", "darkorange")) +
  scale_color_manual("Posterior\ninterval", 
                     values = c("navy", "darkorange"))+
  geom_point(data = dat.valid , 
             aes(x = year,
                 y = dh_y0),
             shape = 21,
             size = 1.5,
             fill = "grey90") +
  geom_point(data = dat.valid.1985 , 
             aes(x = year,
                 y = dh_y0),
             shape = 21,
             size = 1.5,
             fill = "black") +
  labs(x = "Year" ,
       y = "Elevation change [m]") +
  theme_bw()   +
  geom_text(data  = trend.valid.hdi,
            aes(x = 1950, y = min_y-15, label = hdi),
            size = 2.2,
            colour = "darkorange",
            inherit.aes = FALSE,
            parse = FALSE,
            hjust = "left") +
  geom_text(data  = trend.valid.1985.hdi,
            aes(x = 1950, y = min_y, label = hdi),
            size = 2.2,
            colour = "navy",
            inherit.aes = FALSE,
            parse = FALSE,
            hjust = "left") +
  theme( axis.text   = element_text(size = 7),
         axis.text.x = element_text(size = 7),
         axis.title  = element_text(size = 7),
         strip.text  = element_text(size = 7),
         legend.position = "none")  +
  theme(strip.background = element_blank())

ggsave(
  filename = "regression_valid_1985.pdf",
  plot = elev.valid.1985,
  width = 180,
  height = 55,
  units = "mm"
)

ggsave(
  filename = "regression_valid_1985.png",
  plot = elev.valid.1985,
  width = 180,
  height = 60,
  units = "mm"
)


################################################################################

# COMPARISON WITH HUGONNET

################################################################################

setwd("D:/data/BoxUP/shadow paper/data/Romain/pfau_shadows_v2")

require(tidyverse)
require(ggdist)

raw.0102.old <- read_csv("D:/data/BoxUP/shadow paper/data/Romain/pfau_shadows/dh_01_02_rgi60_int.csv")
raw.0102.old <- raw.0102.old %>%
  mutate(rgiid = str_replace_all(rgiid, "Satellitenschatten_2009_sued_Schatten_Gulkana", "Gulkana West"),
         rgiid = str_replace_all(rgiid, "Satellitenschatten_2009_wirklich_Gulkana", "Gulkana East"))

raw.0102.old <- raw.0102.old[raw.0102.old$rgiid == "Gulkana West" | raw.0102.old$rgiid == "Gulkana East", ]

raw.0102.new <- read_csv("dh_01_02_rgi60_int.csv")
raw.0102.new <- raw.0102.new %>%
  filter(rgiid != "RGI60-01.00570")

raw.11     <- read_csv("dh_11_rgi60_int.csv")
raw.131415 <- read_csv("dh_13_14_15_rgi60_int.csv")

raw.all <- bind_rows(raw.0102.old, raw.0102.new, raw.11, raw.131415) %>%
  mutate(lower = dh - err_dh,
         upper = dh + err_dh) %>%
  mutate(rgiid = str_replace_all(rgiid, 
                                 "RGI60-02.17023", 
                                 "Sperry"),
         rgiid = str_replace_all(rgiid, 
                                 "RGI60-11.01450", 
                                 "Great Aletsch"),
         rgiid = str_replace_all(rgiid, 
                                 "RGI60-02.18778", 
                                 "South Cascade"),
         rgiid = str_replace_all(rgiid, 
                                 "RGI60-14.06794", 
                                 "Baltoro")) %>%
  rename(Glacier = rgiid)

ggplot(data = raw.all, 
       mapping = aes(x = time,
                     y = dh)) +
  geom_lineribbon(aes(ymin = lower, ymax = upper), fill = "lightblue") +
  geom_line(size = 0.2) +
  facet_wrap(~Glacier) + 
  theme_bw() + 
  xlab("Year") +
  ylab("Cumuluative elevation change (m)\n(Mean and 1 sigma measurement error")


glacier.2000 <- all_glaciers %>%
  filter(!is.na(Year), 
         !is.na(dh_y0),
         Year >= 1999 & Year <= 2019) %>% 
  group_by(Glacier) %>%
  filter(dh_y0 >= quantile(dh_y0, 0.01),
         dh_y0 <= quantile(dh_y0, 0.99)) %>%
  ungroup() %>%
  mutate(year_scale = scale_this(Year),
         dh_y0_scale = scale_this(dh_y0),
         prev_mb_scale = scale_this(prev_mb)) %>% 
  mutate(id = 1: n()) %>% 
  mutate(year_date = as.Date(paste0(Year, "-07-01"), format = "%Y-%m-%d"))

bprior <- prior(normal(0, 2.5), class = "Intercept") +
  prior(normal(0, 2.5), class = "b") +
  prior(normal(0, 2.5), class = "sd") +
  prior(normal(0, 2.5), class = "sigma") +
  prior(constant(3), class = "nu")

brm.dh_y0.since2000  <- brm(dh_y0_scale ~ year_scale + ( year_scale | Glacier),
                  family  = student(),
                  data    = glacier.2000,
                  prior   = bprior,
                  cores = 3,
                  chains = 3,
                  warmup = 2000,
                  iter = 6000,
                  control = list(adapt_delta = 0.99,
                                 max_treedepth = 15),
                  backend = "cmdstanr",
                  threads = threading(3))

saveRDS(brm.dh_y0.since2000, "geodetic_lines_since_2000_brms.RDS")

# Original parameters

sd_y       <- sd(glacier.2000$dh_y0)
mean_y     <- mean(glacier.2000$dh_y0) 
sd_x       <- sd(glacier.2000$Year)
mean_x     <- mean(glacier.2000$Year)
mod.levels <- unique(glacier.2000$Glacier)

# Define the range, for which new predictions will be made.
# ... on original scale

seq.x.orig <- seq(from = min(glacier.2000$Year)-2,
                  to =   max(glacier.2000$Year)+2,
                  length.out = 132)

# ... and on scaled range

seq.x.scaled <- (seq.x.orig - mean_x) / sd_x

# Obtain the standardized posterior predictive distribution for new observations
# and convert the predictions to original scale.

pred.tibble <- tibble(year_scale = rep(seq.x.scaled, each = length(mod.levels)),
                      Year       = rep(seq.x.orig,   each = length(mod.levels)),
                      Glacier    = rep(unique(mod.levels), times = length(seq.x.scaled)))

post_epred  <- posterior_epred(brm.dh_y0.since2000,
                               # dpar = "mu",
                               ndraws = 1000,
                               re_formula = NULL,
                               newdata = pred.tibble) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%  
  mutate(Glacier = pred.tibble$Glacier,
         Year = pred.tibble$Year ) %>%
  pivot_longer( cols = starts_with("..."), 
                values_to = "prediction") %>%
  mutate(prediction = (prediction*sd_y) + mean_y) %>%
  dplyr::select(-name) %>% 
  mutate(year_date = as.Date(paste0(Year, "-07-01"), format = "%Y-%m-%d"))

# Extract posterior trends

slopes.cond <- tidy_draws(brm.dh_y0.since2000) %>% 
  pivot_longer(starts_with("r_Glacier"),
               names_to = "param",
               values_to = "post") %>% 
  filter(str_detect(param, 'year_scale')) %>%
  transmute(b_year_scale, param, post) %>%
  mutate(param = str_replace_all(param, "[.]", " "),
         Glacier  = str_extract_all(param,"(?<=\\[).+(?=,)") %>% unlist(),
         post_orig = b_year_scale + post,
         post_orig = post_orig  * sd_y / sd_x) 


trend.dh_y0.hdi.2000 <- slopes.cond %>% 
  group_by(Glacier) %>% 
  summarise(med = median(post_orig), 
            q_2_5 = quantile(post_orig, 0.025), 
            q_97_5 = quantile(post_orig, 0.975)) %>% 
  mutate(lb = paste0("-", round(med - q_2_5, digits = 2)), 
         ub = paste0("+", round(q_97_5 - med, digits = 2)),
         med = round(med, digits = 2),
         hdi = paste0(med, "(", ub, "/", lb, ")")) 

write_csv(trend.dh_y0.hdi.2000, "shadow_trends_of_our_method.csv")

min_y <- glacier.2000 %>%
  group_by(Glacier) %>%
  summarise(min_y = quantile(dh_y0, 0.025))

trend.dh_y0.hdi <- left_join(trend.dh_y0.hdi.2000, min_y, by = "Glacier")

elev.ch.2000 <- post_epred %>%
  ggplot() +
  facet_wrap(~Glacier, 
             scales = "free",
             ncol = 3) +
  geom_boxplot(data = glacier.2000 ,
               aes(x = year_date,
                   y = dh_y0,
                   group = year_date),
               fill = "khaki2",
               outlier.shape = NA,
               lwd = 0.25) +
  stat_lineribbon(aes(x = year_date, 
                      y = prediction),
                  .width = c(.95),
                  fill = "khaki1",
                  color = "khaki4",
                  alpha = 0.6,
                  size = 0.8,
                  point_interval = mean_qi) + 
  labs(x = "Year" ,
       y = "Elevation change [m]") +
  theme_bw()   +
  geom_lineribbon(data = raw.all,
    aes(x = time,
        ymin = lower, ymax = upper), 
    fill = "lightblue",
    alpha = 0.75) +
  geom_line(data = raw.all,
            aes(x = time,
                y = dh), 
            color = "darkblue",
            size = 0.5,
            alpha = 0.75) +
  theme( axis.text   = element_text(size = 7),
         axis.text.x = element_text(size = 7),
         axis.title  = element_text(size = 7),
         strip.text  = element_text(size = 7),
         legend.position = "none")  +
  geom_text(data  = trend.dh_y0.hdi,
            aes(x = as.Date("1997-01-01", format = "%Y-%m-%d"), 
                y = min_y, 
                label = hdi),
            size = 2.2,
            colour = "khaki4",
            inherit.aes = FALSE,
            parse = FALSE,
            hjust = "left") +
  theme(strip.background = element_blank()) 


ggsave(
  filename = "our_data_vs_hugonnet.pdf",
  plot = elev.ch.2000,
  width = 180,
  height = 90,
  units = "mm",
  dpi = 300
)

ggsave(
  filename = "our_data_vs_hugonnet.png",
  plot = elev.ch.2000,
  width = 180,
  height = 90,
  units = "mm",
  dpi = 300
)

# Rates Hugonnet

r.13 <- read_csv("dh_13_14_15_rgi60_int_rates.csv") %>% 
  filter(period == "2000-01-01_2020-01-01") %>% 
  transmute(rgiid, period, dhdt, err_dhdt)

r.11 <- read_csv("dh_11_rgi60_int_rates.csv") %>% 
  filter(period == "2000-01-01_2020-01-01") %>% 
  transmute(rgiid, period, dhdt, err_dhdt)


r.0102 <- read_csv("dh_01_02_rgi60_int_rates.csv") %>% 
  filter(period == "2000-01-01_2020-01-01") %>% 
  transmute(rgiid, period, dhdt, err_dhdt)


r.0102 <- read_csv("D:/data/BoxUP/shadow paper/data/Romain/pfau_shadows/dh_01_02_rgi60_int_rates.csv") %>% 
  filter(period == "2000-01-01_2020-01-01") %>% 
  transmute(rgiid, period, dhdt, err_dhdt)


