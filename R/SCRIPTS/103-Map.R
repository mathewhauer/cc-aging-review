###------FIGURE Map and Timelines-----
## @knitr map

## Importing working sample of reviewed articles
documents_reviewed <- read.xlsx("../R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A") %>%
  mutate(ISO3 = Area)


countries <- pull(documents_reviewed, ISO3) 

countries <- data.frame(ISO3 = str_trim(unlist(strsplit(countries,",")))) %>%
  group_by(ISO3) %>%
  dplyr::summarise(count = n())

wrld_simpl <- read_sf("../R/DATA-RAW/wrld_simpl/TM_WORLD_BORDERS_SIMPL-0.2.shp") 

shape_df <- fortify(wrld_simpl) %>%
  left_join(countries, by = "ISO3")

ggplot() +
  geom_sf(data = shape_df, aes(fill = count), color = 'black') +
  # geom_polygon(data = shape_df , aes(x = long, y = lat, group = group, fill = count), color = 'black') +
  scale_fill_distiller(palette = "YlOrBr", na.value = "white",
                       direction = 1) +
  # scale_fill_continuous(low="yellow", high="darkred", 
  #                       guide="colorbar",na.value="white") +
  # scale_fill_brewer(palette = "YlOrRd", na.value = "white") +
  theme_void() +
  # coord_sf("mollweide", lims_method = "geometry_bbox") +
  # coord_sf(crs = st_crs("World"), default_crs = NULL) +
  # coord_sf(crs = st_crs(7051)) +
  theme(legend.position = "bottom")
