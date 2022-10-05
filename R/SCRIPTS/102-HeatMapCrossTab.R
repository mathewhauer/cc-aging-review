###------FIGURE Heat Map Crosstab-----
## @knitr heatmapcrosstab

## Importing Data
documents_split <- read.xlsx("../R/DATA-RAW/categorized effects impacts.xlsx")
documents_split <- documents_split[,c(30,31)]

## Create df for crosstab
heatmap_df <- as.data.frame(table(documents_split$Collapsed.Effect, documents_split$Collapsed.Impact))
colnames(heatmap_df) <- c("Climate Effect", "Climate Impact", "Freq")
heatmap_df <- heatmap_df %>%
  filter(`Climate Effect` != "None") %>% # CONSIDER RECODING NONES TO BROAD "CLIMATE CHANGE" CATEGORY
  mutate(textlabel = paste0(Freq, " (",
    percent(Freq/sum(Freq), accuracy = 0.1),
    ")"))

##
yin <- heatmap_df %>% 
  group_by(`Climate Impact`) %>% 
  summarise(value = sum(Freq)) %>% 
  mutate(value = value / sum(value))

xin <- heatmap_df %>% 
  group_by(`Climate Effect`) %>% 
  summarise(value = sum(Freq)) %>% 
  mutate(value = value / sum(value))

## Creating HeatMap Crosstab
ph <- ggplot(heatmap_df,aes(x=`Climate Effect`,y=`Climate Impact`)) +
  geom_tile(aes(fill=Freq)) +
  scale_fill_gradient(low = "grey", high = "steelblue", limits = c(1,max(heatmap_df$Freq)), na.value = "white") +
  geom_text(aes(label=textlabel), size =2) +
  theme(axis.text.x = element_text(angle = 35,  hjust=1),
        legend.position = "bottom") +
  labs(fill = NULL)

# Marginal plots
py <- ggplot(yin, aes(value, `Climate Impact`)) +
  geom_col(width = .75) +
  geom_text(aes(label = scales::percent(value, accuracy = 0.1)), hjust = -.1, size = 6 / .pt) +
  scale_x_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void()

px <- ggplot(xin, aes(`Climate Effect`, value)) +
  geom_col(width = .75) +
  geom_text(aes(label = scales::percent(value, accuracy = 0.1)), vjust = -.5, size = 6 / .pt) +
  scale_y_continuous(expand = expansion(mult = c(.0, .25))) +
  theme_void()

px + plot_spacer() + ph + py + plot_layout(ncol = 2, widths = c(2, 1), heights = c(1, 2))
