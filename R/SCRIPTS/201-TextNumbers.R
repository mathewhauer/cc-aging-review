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

## Categorized & Separated climate effects & impacts
effect_impact <- read.xlsx("../R/DATA-RAW/categorized effects impacts.xlsx")


# Total number of articles
num_articles_all <- nrow(recs_search)

# Total number of articles with 4 cites per year
num_articles_4cites <- nrow(documents_4peryear)

# Total number of articles with aging terms in their abstract
num_articles_terms <- nrow(documents_4pyearandAgingterms)

# Total number of articles in our review
num_articles_reviewed <- nrow(documents_reviewed)

# Number of articles about temperature and mortality
num_temp_mort <- nrow(effect_impact[effect_impact$Collapsed.Effect == "Temperature" & effect_impact$Collapsed.Impact == "Mortality",])

# Number of articles about temperature and morbidity
num_temp_morb <- nrow(effect_impact[effect_impact$Collapsed.Effect == "Temperature" & effect_impact$Collapsed.Impact == "Morbidity",])

# Number of articles about US
num_US <- nrow(documents_reviewed[c(grep("US", documents_reviewed$Area), grep("United States", documents_reviewed$Area)),])

# Number of articles about China
num_china <- nrow(documents_reviewed[grep("China", documents_reviewed$Area),])

# Number of articles about globe
num_global <- nrow(documents_reviewed[grep("Global", documents_reviewed$Area),])

# Number of articles about Australia
num_aus <- nrow(documents_reviewed[grep("Australia", documents_reviewed$Area),])

# Number of articles about mortality
num_mort <- nrow(effect_impact[effect_impact$Collapsed.Impact == "Mortality",])

# Number of articles about temperature
num_temp <- nrow(effect_impact[effect_impact$Collapsed.Effect == "Temperature",])

# Number of articles about morbidity
num_morb <- nrow(effect_impact[effect_impact$Collapsed.Impact == "Morbidity",])

# Number of articles about pollution
num_pollution <- nrow(effect_impact[effect_impact$Collapsed.Effect == "Pollution",])

# Number of articles about Drought, SLR, or Extreme Weather
num_D_EW_SLR <- nrow(effect_impact[effect_impact$Collapsed.Effect == "SLR & Flooding" | effect_impact$Collapsed.Effect == "Extreme Weather" | effect_impact$Collapsed.Effect == "Drought",])

# Number of articles about human behavior
num_behave <- nrow(effect_impact[effect_impact$Collapsed.Impact == "Migration" | effect_impact$Collapsed.Impact == "Economy" | effect_impact$Collapsed.Impact == "Climate Behaviors & Policy",])
