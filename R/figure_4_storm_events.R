# extremne stormes

library(tidyverse)
library(sf)
library(janitor)
storm_events <- 
  list.files("data/storm_events/",
             pattern = "csv.gz",
             full.names = T) %>%
  lapply(function(x)read_csv(x, guess_max = 1000000)) %>%
  dplyr::bind_rows() %>%
  janitor::clean_names() %>%
  dplyr::mutate(cdn = str_sub(damage_crops, 1, nchar(damage_crops)-1)%>% as.numeric(),
         cdm = str_sub(damage_crops, nchar(damage_crops), nchar(damage_crops)),
         cdx = ifelse(cdm == "K", 1000, 1e6),
         crop_damage = cdn * cdx)

stts <- c("COLORADO", "KANSAS", "NEBRASKA", "OKLAHOMA", "SOUTH DAKOTA", "TEXAS")
event_types <- c("Dust Devil", "Flash Flood", "Flood", "Funnel Cloud", "Hail", 
            "Heavy Rain", "High Wind", "Lightning", "Thunderstorm Wind",
            "Tornado")

hp <- st_read("data/us_eco_l3/us_eco_l3.shp") %>% 
  filter(NA_L3CODE == "9.4.1") %>%
  mutate(region = "Semi-Arid High Plains") %>%
  st_transform(4326)

aoi <- storm_events %>%
    filter(!is.na(begin_lat), !is.na(begin_lon)) %>% 
  filter(state %in% stts, event_type %in% event_types) %>%
  mutate(state = str_to_title(state)) %>%
    st_as_sf(coords = c("begin_lon", "begin_lat"), crs=4326) %>%
    st_intersection(hp)
aoi <- st_set_geometry(aoi, NULL)
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
  scale_x_continuous(breaks = c(2000,2010,2020)) +
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
  scale_x_continuous(breaks = c(2000,2010,2020)) +
  scale_fill_manual(values = pal2) +
  facet_wrap(~state, nrow=1) +
  scale_y_log10();pp2

multipanel <- ggpubr::ggarrange(pp2, pp1, nrow=2, ncol=1, 
                                common.legend = TRUE, legend = "bottom")

ggsave("out/figure_4_multipanel.png",multipanel, bg = "white", width = 10, height = 5.5)


# aoi <- storm_events  %>%
#   filter(!is.na(begin_lat), !is.na(begin_lon))

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

