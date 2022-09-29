###------FIGURE Map and Timelines-----
## @knitr mapandtimelines

## Importing working sample of reviewed articles
documents_reviewed <- read.xlsx("../R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")


countries <- pull(documents_reviewed, Area)

countries <- data.frame(Area = str_trim(unlist(strsplit(countries,",")))) %>%
  group_by(Area) %>%
  dplyr::summarise(count = n())

data(wrld_simpl)

shape_df <- fortify(wrld_simpl, region = 'ISO3') %>%
  left_join(countries, by = c("id" = "Area"))

ggplot() +
  geom_polygon(data = shape_df , aes(x = long, y = lat, group = group, fill = count), color = 'black') +
  scale_fill_distiller(palette = "YlOrBr", na.value = "white",
                       direction = 1) +
  # scale_fill_continuous(low="yellow", high="darkred", 
  #                       guide="colorbar",na.value="white") +
  # scale_fill_brewer(palette = "YlOrRd", na.value = "white") +
  theme_void() +
  coord_map("mollweide") +
  theme(legend.position = "bottom")
