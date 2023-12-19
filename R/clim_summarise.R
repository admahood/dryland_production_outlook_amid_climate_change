library(tidyverse)
library(terra)
library(sf)
library(mgcv)
library(tidygam)

unzipit <- FALSE

if(unzipit){
  zipfiles <- list.files("data/prism", pattern = "all_bil.zip", full.names = TRUE)
  lapply(zipfiles, function(x)unzip(x, exdir = "data/prism/unzipped/"))
}
noaa_regions <- st_read("data/climdivs.gpkg")
bil_files <- list.files("data/prism/unzipped", pattern = "bil.bil$", full.names = TRUE)
ppt <- terra::rast(bil_files)

noaa_regions_w_precip <-
  noaa_regions %>%
  dplyr::select(noaa_region = NAME) %>%
  dplyr::mutate(terra::extract(ppt, vect(.), fun = "mean", ID=F, raw=TRUE)) %>%
  st_set_geometry(NULL) %>%
  pivot_longer(cols = names(.)[2:ncol(.)], values_to = "ppt") %>%
  # hardcoding alert
  mutate(temporal_aggregation = ifelse(nchar(name) == 33, "month", "year"),
         month = str_sub(name,28,29) %>% as.numeric,
         year = str_sub(name,24,27) %>% as.numeric); noaa_regions_w_precip

monthly <- noaa_regions_w_precip %>%
  filter(temporal_aggregation == "month") %>%
  mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-")),
         dateN = as.numeric(date),
         nr = as.factor(noaa_region))

noaa_regions_w_precip %>%
  filter(temporal_aggregation == "year") %>%
  ggplot(aes(x=year,y=ppt, color = noaa_region)) +
  geom_line() +
  theme_classic()

mf <- monthly %>%
  ggplot(aes(x=date,y=ppt, color = noaa_region)) +
  geom_line() +
  geom_smooth(method = "lm", se = F)+
  theme_classic() +
  facet_wrap(~month)
ggsave(filename = "out/mf.png", plot =mf)

mod_ar0 <- gam(ppt ~ s(as.numeric(date), k = 20, by = nr) + s(month, bs = "cc", k=12), 
                data = monthly)
summary(mod_ar0)


m3 <- gamm(ppt ~ s(month, bs = "cc", k = 12) + s(dateN, k = 20) + noaa_region,
          data = monthly, correlation = corARMA(form = ~ 1|year, p = 3))
m0 <- gam(ppt ~ s(month, bs = "cc", k = 12) + s(dateN, k = 20, by=nr),
           data = monthly)
preds <- tidygam::predict_gam(m0)
ggplot(preds, aes(x=dateN, y=ppt, color = month)) +
  geom_point() +
  facet_wrap(~noaa_region)


anova(m$lme, m2$lme, m3$lme, m4$lme, m5$lme)

gam.check(m3$gam)

forecast::ggAcf(residuals(m3$lme))
forecast::ggAcf(residuals(m3$lme), type="partial")

summary(m3$gam)

layout(matrix(1:2, ncol = 2))
plot(m3$gam, residuals = TRUE)
layout(1)



library(mblm)
ts <- mblm(ppt ~  dateN, data= monthly %>% mutate(dateN = as.numeric(date)))
summary(ts)

mods <- lapply(1:12, function(x) broom::tidy(mblm(ppt ~  dateN, data= monthly %>% 
                                       filter(month == x) %>% 
                                       mutate(dateN = as.numeric(date)))) %>%
                 mutate(month = x)) %>%
  bind_rows() %>%
  filter(term != "(Intercept)") %>%
  write_csv("out/ts_monthly.csv") 

