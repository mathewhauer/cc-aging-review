rm(list=ls())
### TS=(aging OR aged or elderly) AND TS=("climate change")

recs2020 <- read_xlsx("./R/DATA-RAW/CopyofCoded2.xlsx", sheet='Sheet3') %>%
  filter(`citations/year` >= 4)

### Searched on 09/07/2022 at SSCI Web of Science the following term:
### TS=(aging OR aged or elderly) AND TS=("climate change")
###
### We then limited our results to any article recieving at least 4 citations per year,
### and contained the words "aging", "aged", or "elderly" in the abstract.

recs_search <- vroom(list.files("./R/DATA-RAW/Clarvariate", full.names = T)) %>%
  mutate(YsincePub = 2023-PY,
         CitesPerYear = TC/YsincePub)

### Filtering for >=4 cites per year AND with a DOI.
documents_4peryear <- recs_search %>%
  filter(CitesPerYear >=4,
         !is.na(DI))
documents_4pyearandAgingterms <- documents_4peryear %>%
    filter(grepl('\\baging\\b|\\baged\\b|elderly', AB))
  #!grepl('forest|forests|soil|soils', AB),


recs_search_4peryear <- documents_4pyearandAgingterms[which(documents_4pyearandAgingterms$CitesPerYear >=4),] %>%
  left_join(., recs2020, by = c('DI' = 'DOI')) %>%
  dplyr::select(AU, TI, DI, PY, TC, UT, `DOI Base`:`...25`)
  

recs_search_4peryear <- cbind(recs_search_4peryear,  sample(c("Matt", "Kyle"), nrow(recs_search_4peryear), replace=TRUE, prob=c(0.5,0.5)))

write_excel_csv(recs_search_4peryear, "./R/DATA-RAW/papers.xls")

### Next step is reviewing the papers in recs_search_4peryear and putting notes in the excel file.






# 
# recs_search_4peryear2 <- recs_search_4peryear %>%
#   filter(!Summary %in% c("N/A"))
# 
# 
# commonpapers <- intersect(recs2020$DOI, recs_search$DI)
# 
# dat <- recs_search_4peryear[which(recs_search_4peryear$DI %in% commonpapers),] %>%
#   unique()
# 
# rus_stopwords = data.frame(word = stopwords("eng"))
# 
# dat2 <- dat %>% unnest_tokens(word, AB) %>% anti_join(rus_stopwords)
# frequency_dataframe = dat2 %>% count(word) %>% arrange(desc(n))
# ggplot(frequency_dataframe[1:20,], aes(x = word, y = n, fill = word)) + geom_col() 