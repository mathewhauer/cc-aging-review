###------FIGURE Flow Diagram-----
## @knitr heatmapcrosstab

## Importing working sample of reviewed articles
documents_reviewed <- read.xlsx("../R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")

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
food <- c("Food Security")
mortality <- c("mortality","long-term survival")
Flag_Review <- c("vulnerability","Exposure","multiple","exposure","Multiple")
# hospital_ambulance <- c("hospital admissions","Ambulance attendance")
economy <- c("economic", "leisure activities")
migration <- c("migration")
morbidity <- c("morbidity","cardiac health", 
               "hospital admissions","Ambulance attendance",
               "emotional wellbeing","mental disorder hospital admissions",
               "Sleep","sleep")
# mental_health <- c("emotional wellbeing","mental disorder hospital admissions")
climate_behaviors_policy <- c("Energy Use","urban climate change policies","home protection","emissions")
# sleep <- c("Sleep","sleep")
no_impact <- c("No")

##Creating list of those vectors
impacts_list <- list(mortality, Flag_Review, #hospital_ambulance, mental_health,sleep,
                     economy, migration, morbidity, climate_behaviors_policy,  no_impact, food)
names(impacts_list) <- c("mortality", "Flag_Review", #"hospital_ambulance", "mental_health","sleep",
                         "economy", "migration", "morbidity",  "climate_behaviors_policy",  "none", "food security")

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
  separate_rows(Climate.Impact, sep = ", ") %>%
  filter(Review == "No")

## Collapsing Climate Impact Variable
documents_split <- documents_split %>% 
  mutate(Collapsed.Impact = "",
         Collapsed.Impact = impact_collapse(Climate.Impact, Collapsed.Impact))
documents_split <- documents_split[,c(30,31)]

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

## Create df for crosstab
heatmap_df <- as.data.frame(table(documents_split$Collapsed.Effect, documents_split$Collapsed.Impact))
colnames(heatmap_df) <- c("Climate Effect", "Climate Impact", "Freq")
heatmap_df <- heatmap_df %>%
  filter(`Climate Effect` != "None") %>%
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