invisible(sapply(c("tidyverse", "sf"), 
                 library, 
                 character.only=T))


#map figure ============

gp <- st_read("/home/a/data/background/ecoregions/cec/us_eco_l3.shp") %>% 
  filter(str_sub(L2_KEY, 1,3) %in% c("9.4", "9.3")) %>%
  mutate(region = "Semi-Arid Great Plains")

l1 <- st_read("/home/a/data/background/ecoregions/cec/us_eco_l3.shp") %>%
  group_by(L1_KEY) %>%
  summarise()

hp <- st_read("/home/a/data/background/ecoregions/cec/us_eco_l3.shp") %>% 
  filter(NA_L3CODE == "9.4.1") %>%
  mutate(region = "Semi-Arid High Plains")


states <- sf::st_read("/home/a/data/background/CUS/CUS.shp") %>%
  dplyr::filter(STUSPS %in% c("MT", "WY", "CO", "UT", "NM", "AZ",
                              "TX", "OK", "KS", "NE", "SD", "ND")) %>%
  dplyr::mutate(region = ifelse(STUSPS %in% c("MT", "WY", "CO", "UT", "NM", "AZ"),
                                "Mountain Region", "Central Region"))

p <- ggplot(states, aes(fill = region)) +
  geom_sf(color = "black") +
  geom_sf(data = gp, 
          color = "transparent", alpha = 0.75) +
  geom_sf(data = hp, alpha = 0.75) +
  scale_fill_manual(values = c("grey", "steelblue", "red", "gold")) +
  theme_void() +
  theme(legend.title = element_blank());p
ggsave(plot = p, filename = "out/ecos.png", bg = "white", width=5, height=5)
