###------FIGURE Map and Timelines-----
## @knitr mapandtimelines

## Importing working sample of reviewed articles
documents_reviewed <- read.xlsx("./R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")

py <- documents_reviewed %>%
  group_by(PY) %>%
  dplyr::summarise(count= n())

TC <- documents_reviewed %>%
  group_by(PY) %>%
  dplyr::summarise(cites= sum(TC))

citesyear <- documents_reviewed %>%
  group_by(PY) %>%
  dplyr::summarise(cites= mean(`citations/year`, na.rm=T))

a <- ggplot(data=py, aes(x = PY, y = count)) +
  geom_point() +
  geom_smooth() + 
  theme_bw() +
  NULL

b <- ggplot(data=TC, aes(x = PY, y = cites)) +
  geom_point() +
  geom_smooth() + 
  theme_bw() +
  NULL

c <- ggplot(data=citesyear, aes(x = PY, y = cites)) +
  geom_point() +
  geom_smooth() + 
  theme_bw() +
  NULL

plot_grid(a,b,c, ncol=1)

