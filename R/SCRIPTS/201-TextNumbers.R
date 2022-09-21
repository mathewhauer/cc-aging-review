###------Numbers -----
## @knitr textnumbers


## All records from WOS
recs_search <- vroom(list.files("../R/DATA-RAW/Clarvariate", full.names = T)) %>%
  mutate(YsincePub = 2023-PY,
         CitesPerYear = TC/YsincePub)

## At least 4 citations per year and contains a DOI
documents_4peryear <- recs_search %>%
  filter(CitesPerYear >=4)

## Abstract contains certain key words
documents_4pyearandAgingterms <- documents_4peryear %>%
  filter(grepl('\\baging\\b|\\baged\\b|elderly', AB))

## Documents review
documents_reviewed <- read.xlsx("../R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")



# Total number of articles
num_articles_all <- nrow(recs_search)

# Total number of articles with 4 cites per year AND a DOI
num_articles_4cites <- nrow(documents_4peryear)

# Total number of articles with aging terms in their abstract
num_articles_terms <- nrow(documents_4pyearandAgingterms)

# Total number of articles in our review
num_articles_reviewed <- nrow(documents_reviewed)

