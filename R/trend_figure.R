invisible(sapply(c("tidyverse", "terra", "sf", "mblm"), 
                 library, 
                 character.only=T))


hp <- st_read("/home/a/data/background/ecoregions/cec/us_eco_l3.shp") %>% 
  filter(NA_L3CODE == "9.4.1") %>%
  mutate(region = "Semi-Arid High Plains")


states <- sf::st_read("/home/a/data/background/CUS/CUS.shp") %>%
  dplyr::filter(STUSPS %in% c("MT", "WY", "CO", "UT", "NM", "AZ",
                              "TX", "OK", "KS", "NE", "SD", "ND")) %>%
  dplyr::mutate(region = ifelse(STUSPS %in% c("MT", "WY", "CO", "UT", "NM", "AZ"),
                                "Mountain Region", "Central Region"))

# ggplot(states) + geom_sf() + geom_sf(data = gp, aes(fill = L3_KEY), alpha = 0.5)

# prism climate analysis ============================================================

if(!file.exists("data/time_series.rda")){
  ppt_files <- list.files("data/prism/ppt/PRISM_ppt_stable_4kmM3_198101_202303_bil/", 
                          pattern = "bil.bil$", full.names = TRUE)
  vpd_files <- list.files("data/prism/vpdmin/PRISM_vpdmin_stable_4kmM3_198101_202302_bil/",
                          pattern = "bil.bil$", full.names = TRUE)
  vpdmx_files <- list.files("data/prism/vpdmax/PRISM_vpdmax_stable_4kmM3_198101_202303_bil/",
                            pattern = "bil.bil$", full.names = TRUE)
  
  ppt <- terra::rast(ppt_files)
  vpd <- terra::rast(vpd_files)
  vpdmx <- terra::rast(vpdmx_files)
  
  hp_w_precip <-
    hp %>%
    sf::st_transform(crs = st_crs(ppt)) %>%
    dplyr::select(region) %>%
    dplyr::summarise(region=region) %>%
    dplyr::mutate(terra::extract(ppt, vect(.), fun = "mean", ID=F, raw=TRUE)) %>%
    st_set_geometry(NULL) %>%
    pivot_longer(cols = names(.)[2:ncol(.)], values_to = "ppt") %>%
    # hardcoding alert
    mutate(temporal_aggregation = ifelse(nchar(name) == 33, "month", "year"),
           month = str_sub(name,28,29) %>% as.numeric,
           year = str_sub(name,24,27) %>% as.numeric); hp_w_precip
  
  hp_w_vpd <-
    hp %>%
    sf::st_transform(crs = st_crs(vpd)) %>%
    dplyr::select(region) %>%
    dplyr::summarise(region=region) %>%
    dplyr::mutate(terra::extract(vpd, vect(.), fun = "mean", ID=F, raw=TRUE)) %>%
    st_set_geometry(NULL) %>%
    pivot_longer(cols = names(.)[2:ncol(.)], values_to = "vpdmin") %>%
    tidyr::separate(name, into =c("product", "variable", "stable", "resolution", 
                                  "yearmonth", "format"), sep = "_") %>%
    mutate(month = str_sub(yearmonth,5,6) %>% as.numeric,
           year = str_sub(yearmonth,1,4) %>% as.numeric)%>%
    mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-")),
           dateN = as.numeric(date)); hp_w_vpd
  
  hp_w_vpdmx <-
    hp %>%
    sf::st_transform(crs = st_crs(vpdmx)) %>%
    dplyr::select(region) %>%
    dplyr::summarise(region=region) %>%
    dplyr::mutate(terra::extract(vpdmx, vect(.), fun = "mean", ID=F, raw=TRUE)) %>%
    st_set_geometry(NULL) %>%
    pivot_longer(cols = names(.)[2:ncol(.)], values_to = "vpdmax") %>%
    tidyr::separate(name, into =c("product", "variable", "stable", "resolution", 
                                  "yearmonth", "format"), sep = "_") %>%
    mutate(month = str_sub(yearmonth,5,6) %>% as.numeric,
           year = str_sub(yearmonth,1,4) %>% as.numeric)%>%
    mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-")),
           dateN = as.numeric(date)); hp_w_vpdmx
  monthly <- hp_w_precip %>%
    filter(temporal_aggregation == "month") %>%
    mutate(date = lubridate::ymd(paste(year, month, "01", sep = "-")),
           dateN = as.numeric(date))
  
  save(monthly, hp_w_vpd, hp_w_vpdmx, file = "data/time_series.rda")}else{
    load("data/time_series.rda")}

mods <- lapply(1:12, function(x) broom::tidy(mblm(ppt ~  dateN, data= monthly %>% 
                                       filter(month == x) %>% 
                                       mutate(dateN = as.numeric(date)))) %>%
                 mutate(month = x)) %>%
  bind_rows() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable = "ppt") %>%
  write_csv("out/ts_monthly.csv") 

modsvpd <- lapply(1:12, function(x) broom::tidy(mblm(vpdmin ~  dateN, data= hp_w_vpd %>% 
                                                    filter(month == x) %>% 
                                                    mutate(dateN = as.numeric(date)))) %>%
                 mutate(month = x)) %>%
  bind_rows() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable = "vpdmin") %>%
  write_csv("out/ts_monthly_vpd.csv") 

modsvpdmx <- lapply(1:12, function(x) broom::tidy(mblm(vpdmax ~  dateN, data= hp_w_vpdmx %>% 
                                                       filter(month == x) %>% 
                                                       mutate(dateN = as.numeric(date)))) %>%
                    mutate(month = x)) %>%
  bind_rows() %>%
  filter(term != "(Intercept)") %>%
  mutate(variable = "vpdmax") %>%
  write_csv("out/ts_monthly_vpdmx.csv") 

p<- bind_rows(mods, modsvpd, modsvpdmx) %>%
  mutate(month = lubridate::month(month, label=TRUE, abbr=F) %>%
           fct_rev(),
         p_symbol = case_when(p.value < 0.05 & p.value >= 0.01 ~ "*",
                              p.value < 0.01 & p.value >= 0.001 ~ "**",
                              p.value < 0.001  ~ "***",
                              p.value >= 0.05 ~ ""),
         sign = ifelse(estimate > 0, "+", "-"),
         variable = case_match(variable, "ppt" ~ "Precipitation",
                               "vpdmax" ~ "Max VPD",
                               "vpdmin" ~ "Min VPD")) %>% 
  dplyr::select(-std.error, -statistic,-term) %>%
  ggplot(aes(x=variable, y = month, fill = sign, alpha = p_symbol)) +
  geom_tile(color = "black") +
  theme_classic() +
  geom_text(aes(label = p_symbol)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_alpha_manual(values = c(0.25, 1,1, 1)) +
  scale_fill_manual(values = c("turquoise", "firebrick"), name = "Trend Direction") +
  theme(panel.border = element_rect(color = "black", fill=NA),
        axis.title = element_blank()) +
  guides(alpha = "none")
ggsave(plot = p, filename =  "out/pretty_figure.png", width = 4.5, height = 5)
