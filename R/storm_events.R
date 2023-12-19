# extremne stormes

library(tidyverse)
library(sf)
library(janitor)
storm_events <- 
  list.files("data/storm_events",
             pattern = "csv.gz",
             full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  clean_names() %>%
  mutate(cdn = str_sub(damage_crops, 1, nchar(damage_crops)-1)%>% as.numeric(),
         cdm = str_sub(damage_crops, nchar(damage_crops), nchar(damage_crops)),
         cdx = ifelse(cdm == "K", 1000, 1e6),
         crop_damage = cdn * cdx)
hp <- st_read("/home/a/data/background/ecoregions/cec/us_eco_l3.shp") %>% 
  filter(NA_L3CODE == "9.4.1") %>%
  mutate(region = "Semi-Arid High Plains") %>%
  st_transform(4326)

big_events <- 
  storm_events %>%
  filter(crop_damage > 1000, !is.na(begin_lat)) %>%
  st_as_sf(coords = c("begin_lon", "begin_lat"), crs=4326) %>%
  st_intersection(hp) %>%
  filter(!is.na(L3_KEY)) %>%
  mutate(date = as.Date(paste0(begin_yearmonth, begin_day), "%Y%m%d"))

plot(hp[0])  
plot(big_events[0], add=T)

ggplot(big_events, aes(x=date, y=crop_damage)) +
  geom_point()

p1 <- big_events %>%
  group_by(year, event_type) %>%
  summarise(sum_damage = sum(crop_damage)) %>%
  ungroup() %>%
  ggplot(aes(x=as.factor(year), y = sum_damage, fill=event_type)) +
  geom_bar(stat = "identity", color = "black") +
  theme_classic();p1

p1n <- big_events %>%
  group_by(year, event_type) %>%
  summarise(n_events = n()) %>%
  ungroup() %>%
  ggplot(aes(x=as.factor(year), y = n_events, fill=event_type)) +
  geom_bar(stat = "identity", color = "black") +
  theme_classic() ;p1n

p2 <- storm_events %>%
  filter(!is.na(begin_lat),!is.na(begin_lon)) %>%
  st_as_sf(coords = c("begin_lon", "begin_lat"), crs=4326) %>%
  st_intersection(hp) %>%
  filter(!is.na(L3_KEY)) %>%
  group_by(year, event_type) %>%
  summarise(sum_damage = sum(crop_damage)) %>%
  ungroup() %>%
  ggplot(aes(x=year, y = sum_damage, fill=event_type)) +
  geom_bar(stat = "identity", color = "black") +
  theme_classic()

library(ggpubr)
ggsave(plot=ggarrange(p1, p1n,nrow = 2, ncol=1, common.legend = TRUE),
       height=10, width=10, filename = "out/big_events.png", bg="white")

# aoi <- storm_events  %>%
#   filter(!is.na(begin_lat), !is.na(begin_lon)) %>%
#   st_as_sf(coords = c("begin_lon", "begin_lat"), crs=4326) %>%
#   st_intersection(hp)

# aoi <- aoi %>%
#   filter(!is.na(L3_KEY),
#          state != "NEW MEXICO",
#          state != "SOUTH DAKOTA")
aoi <- st_read("data/high_plains_events.gpkg") |>
  st_set_geometry(NULL) |>
  filter(state != "NEW MEXICO", state != "WYOMING") |>
  mutate(state = str_to_title(state))
# st_write(aoi, "data/high_plains_events.gpkg")
  # %>%
  #filter(!is.na(damage_crops), damage_crops != "0.00K", #!is.na(BEGIN_LON),
         # state %in% c("COLORADO", "KANSAS", "TEXAS",
         #              "NEBRASKA", "OKLAHOMA",
         #              "WYOMING"))
pal2 <- RColorBrewer::brewer.pal(name = "Paired",n = 10)
pal1 <- pal2[c(2,3,5,8,9,10)]

pp1<- aoi %>%
  group_by(year, event_type, state) %>%
  summarise(sum_damage = sum(crop_damage)) %>%
  ungroup() %>%
  filter(sum_damage > 0) %>%
  ggplot(aes(x=year, y = sum_damage, fill=event_type)) +
  geom_bar(stat = "identity", color = "black", lwd=0.2) +
  theme_bw() +
  xlab("Year") +
  ylab("Total Damage Reported") +
  scale_fill_manual(values = pal1)+
  facet_wrap(~state, nrow=1) +
  scale_y_log10() +
  scale_color_brewer();pp1

pp2 <- aoi %>%
  group_by(year, event_type, state) %>%
  summarise(n_events = n()) %>%
  ungroup() %>%
  ggplot(aes(x=year, y = n_events, fill=event_type)) +
  geom_bar(stat = "identity", color = "black", lwd=0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  ylab("Number of Events Reported") +
  scale_fill_manual(values = pal2)+
  facet_wrap(~state, nrow=1) +
  scale_y_log10();pp2

multipanel <- ggpubr::ggarrange(pp2, pp1, nrow=2, ncol=1, 
                                common.legend = TRUE, legend = "bottom")

ggsave("out/multipanel.png",multipanel, bg = "white", width = 10, height = 5.5)
# 
# yearly_sums <-
#   aoi %>%
#   filter(event_type %in% c("Drought", "Hail", "Flood", "Thunderstorm Wind")) %>%
#   group_by(event_type, year) %>%
#   summarise(sum_crop_damage = sum(crop_damage, na.rm = TRUE),
#             n_damaging_events = n()) %>%
#   ungroup()
# 
# ggplot(yearly_sums, aes(x = year, y=log(sum_crop_damage))) +
#   geom_point() +
#   facet_wrap(~event_type, scales = "free")
# ggplot(yearly_sums, aes(x = year, y=n_damaging_events)) +
#   geom_point() +
#   facet_wrap(~event_type, scales = "free")
# aoi %>%
#   mutate(date = ym(begin_yearmonth)) %>%
#   filter(event_type %in% c("Drought", "Hail", "Flood", "Thunderstorm Wind")) %>%
#   ggplot(aes(x=date, y= log(crop_damage))) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~event_type)
# 
# plot(spatial[0])

# cumulative crop damage by state ~ time

