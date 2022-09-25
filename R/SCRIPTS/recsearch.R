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

# write_excel_csv(recs_search_4peryear, "./R/DATA-RAW/papers.xls")

### Next step is reviewing the papers in recs_search_4peryear and putting notes in the excel file.

### Creating a df with collapsed climate effect and climate impact categories 
### NOTE: The product has more observations than articles in our sample because articles with > 1 climate effect or impact were counted more than once.

## Creating vectors of climate effect categories
pollution <- c("pollution","particulate matter","Ozone","fine dust","air quality","emissions","ozone","CO2 emissions")
temperature <- c("temperature","heat","seasonal variability","heat waves","daily range","ambient heat","ambient temperature","heatwaves","hot and cold","temperature variability","heat wave","cold","heat islands","variability","cold spells","temperature change","heat, heat waves","summer variability")
extreme_weather <- c("wildfires","extreme weather events","wildires","hurricanes")
SLR_flooding <- c("Flooding","SLR","flooding","flood")
flag_review <- c("mitigation")
drought <- c("Drought","drought")
no_effect <- c("None")

## Create a list of these vectors
effects_list <- list(pollution, temperature, extreme_weather, SLR_flooding, flag_review, drought, no_effect)
names(effects_list) <- c("pollution", "temperature", "extreme_weather", "SLR_flooding", "flag_review", "drought", "None")

## Creating function to collapse Climate Effect variables
effect_collapse <- function(x,y,z){
  for(i in 1:length(effects_list))
    z <- ifelse(x %in% effects_list[[i]] | y %in% effects_list[[i]],
                ifelse(z == "", paste(names(effects_list)[i]), paste(z, names(effects_list)[i], sep = ",")),
                z)
  paste(z)
}

## Collapsing Climate Effect Variables
documents_reviewed <- documents_reviewed %>% 
  mutate(Collapsed.Effect = "",
         Collapsed.Effect = effect_collapse(Climate.Effect, Climate.Effect.2, Collapsed.Effect))

## Creating Climate Impact category vectors
mortality <- c("mortality","long-term survival")
Flag_Review <- c("vulnerability","Exposure","multiple","exposure","Multiple")
hospital_ambulance <- c("hospital admissions","Ambulance attendance")
economy <- c("economic", "leisure activities")
migration <- c("migration")
morbidity <- c("morbidity","cardiac health")
mental_health <- c("emotional wellbeing","mental disorder hospital admissions")
climate_behaviors_policy <- c("Energy Use","urban climate change policies","home protection","emissions")
sleep <- c("Sleep","sleep")
no_impact <- c("No")

##Creating list of those vectors
impacts_list <- list(mortality, Flag_Review, hospital_ambulance, economy, migration, morbidity, mental_health, climate_behaviors_policy, sleep, no_impact)
names(impacts_list) <- c("mortality", "Flag_Review", "hospital_ambulance", "economy", "migration", "morbidity", "mental_health", "climate_behaviors_policy", "sleep", "none")

## Creating function to collapse climate impact
impact_collapse <- function(x,y){
  for(i in 1:length(impacts_list))
    y <- ifelse(x %in% impacts_list[[i]],
                ifelse(y == "", paste(names(impacts_list)[i]), paste(y, names(impacts_list)[i], sep = ",")),
                y)
  paste(y)
}

## Splitting strings
documents_split <- documents_reviewed %>% 
  separate_rows(Collapsed.Effect, sep = ",") %>% 
  separate_rows(Climate.Impact, sep = ", ")

## Collapsing Climate Impact Variable
documents_split <- documents_split %>% 
  mutate(Collapsed.Impact = "",
         Collapsed.Impact = impact_collapse(Climate.Impact, Collapsed.Impact))

## Cleaning up Strings
documents_split <- documents_split %>%
  mutate(Collapsed.Effect = str_to_title(str_replace_all(Collapsed.Effect, "_", " ")),
         Collapsed.Effect = ifelse(Collapsed.Effect == "Flag Review", "Flag For Review", Collapsed.Effect),
         Collapsed.Effect = ifelse(Collapsed.Effect == "Slr Flooding", "SLR & Flooding", Collapsed.Effect),
         Collapsed.Impact = str_to_title(str_replace_all(Collapsed.Impact, "_", " ")),
         Collapsed.Impact = ifelse(Collapsed.Impact == "Flag Review", "Flag For Review", Collapsed.Impact),
         Collapsed.Impact = ifelse(Collapsed.Impact == "", "Empty", Collapsed.Impact),
         Collapsed.Impact = ifelse(Collapsed.Impact == "Hospital Ambulance", "Hospital or Ambulance Attendance", Collapsed.Impact),
         Collapsed.Impact = ifelse(Collapsed.Impact == "Climate Behaviors Policy", "Climate Behaviors & Policies", Collapsed.Impact),
  )

write.xlsx(documents_split, "./R/DATA-RAW/categorized effects impacts.xlsx")




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