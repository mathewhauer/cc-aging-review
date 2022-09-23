###------FIGURE Flow Diagram-----
## @knitr heatmapcrosstab

## Documents review
documents_reviewed <- read.xlsx("../R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")

## Splitting strings
effects_impacts <- documents_reviewed[,c(17,20)]
documents_split <- documents_reviewed[,c(17,20)] %>% 
  separate_rows(Climate.Effect, sep = ",") %>% 
  separate_rows(Climate.Impact, sep = ",")

### Cleaning up strings                    ## I'll revisit this, I only want to remove the first character if it's a " " 
#documents_split <- documents_split %>%                     
#  mutate(Climate.Effect = str_replace_all(Climate.Effect, " ", ""),
#         Climate.Impact = str_replace_all(Climate.Impact, " ", "")
#  )

## Create df for crosstab
heatmap_df <- as.data.frame(table(documents_split$Climate.Effect, documents_split$Climate.Impact))
colnames(heatmap_df) <- c("Climate Effect", "Climate Impact", "Freq")

## Creating HeatMap Crosstab
ggplot(heatmap_df, aes(`Climate Effect`, `Climate Impact`)) +
  geom_tile(aes(fill = Freq), color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 35,  hjust=1))

### Scrap, delete later
#col_a <- c("a", "b", "c", "a,b" , "b,c")
#col_b <- c("c", "b","a","a,a", "a,c")
#df <- data.frame(col_a, col_b)
#df <- df %>% 
#  separate_rows(col_a) %>%
#  separate_rows(col_b)
#df_table <- as.data.frame(table(df$col_a,df$col_b))
