###------FIGURE Network Graph-----
## @knitr networkgraph

### NEW ATTEMPT OF NETWORK WITH ARTICLES AS NODES AND CITATIONS AS EDGES
#install.packages("tidyverse")
#install.packages("listviewer")
#remotes::install_github("ropensci/rcrossref")
#file.edit("~/.Renviron")          # add crossref_email="name@example.com" to your .Renviron to query APIs with rcrossref package
library(rcrossref)
library(tidyverse)
library(listviewer)
library(tidygraph)
library(ggraph)

## IMPORTING DATA
documents_reviewed <- read.xlsx("../R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")
documents_reviewed[177,3] <- "10.1016/S0140-6736(21)01209-5"   #DOI was missing for this observation, so I write it in here
documents_reviewed <- documents_reviewed[documents_reviewed$DI != "10.23749/mdl.v109i3.6851",] #This DOI can't be found by crossref package THIS SHOULD BE NOTED IN METHODS OR NETWORK SETION

## Pulling metadata for references/citations
### Saving the results to the hard drive and then reimporting because cr_works crawls the web and this saves time.
reviewed_dois <- documents_reviewed$DI
# reviewed_articles_meta <- cr_works(dois = reviewed_dois) %>%
#   purrr::pluck("data")
# saveRDS(reviewed_articles_meta, "./R/DATA-PROCESSED/reviewed_articles_meta.rds")
reviewed_articles_meta <- readRDS("../R/DATA-PROCESSED/reviewed_articles_meta.rds")


## Creating objects to be referenced in network Wrangling
ref_list <- list()
ref_vector <- vector()
from_to <- list()
cited_ids <- vector()
nodes <- as.data.frame(reviewed_dois) %>% 
  mutate(id = as.character(1:176))
colnames(nodes) <- c("label", "id")

## Wrangling into network format
for(this.article in 1:nrow(reviewed_articles_meta)){
  ref_vector <- reviewed_articles_meta[[32]][[this.article]]$DOI
  ref_list[[this.article]] <- list(ref_vector)
  for(this.citedart in 1:length(reviewed_dois)){
    cited_ids <- ifelse(reviewed_dois[[this.citedart]] %in% ref_list[[this.article]][[1]], 
                        ifelse(is.na(cited_ids) == "logical(0)",
                               paste(nodes$id[[this.citedart]]),
                               paste(cited_ids, nodes$id[[this.citedart]], sep = ",")), 
                        cited_ids)
    from_to[[this.article]] <- list(cited_ids)
  }
    cited_ids <- vector()
}

## Turning the nodes data into Tidy Data. Slightly recoding the Climate Effects to reduce them to 6 categories
nodesinfo <- nodes %>%
  left_join(., documents_reviewed, by = c("label" = "DI")) %>%
  mutate(origin = as.numeric(id)) %>%
  dplyr::select(origin, everything()) %>%
  mutate(Climate.Effect = factor(case_when(
    Climate.Effect %in% c("SLR", "Flooding") ~ "SLR & Flooding",
    Climate.Effect %in% c("Temperature", "temperature") ~ "Temperature",
    Climate.Effect %in% c("wildfires", "drought") ~ "Wildfires & Drought",
    TRUE ~ Climate.Effect
  ),
  levels = c("Temperature", "None", "pollution", "SLR & Flooding", "Wildfires & Drought", "extreme weather events"))) %>%
  separate(AU, into = "First", sep = ",") %>%
  mutate(labels = paste(First, "et al,", PY)) %>%
  dplyr::select(labels, everything())

## Turning the edge data into Tidy Data.
a<- 
  data.frame(matrix(unlist(from_to), nrow=length(from_to), byrow=TRUE)) %>%
  rename(destinations =`matrix.unlist.from_to...nrow...length.from_to...byrow...TRUE.`) %>%
  mutate(origin = row_number()) %>%
  separate_rows(destinations, sep=",") %>%
  # mutate(destinations = ifelse(is.na(destinations),0, destinations)) %>%
  count(origin, destinations) %>%
  spread(destinations, n, fill = 0) %>%
  select(-2) %>% as.data.frame() %>%
  pivot_longer(cols = 2:106,
               names_to = "destination",
               values_to = "connection") %>%
  mutate(destination = as.numeric(destination)) %>%
  filter(connection >0) %>%
  na.omit() %>%
  dplyr::select(-connection) 

# Generating the Top 5 Destinations ie the Most Central papers
b <- a %>%
  group_by(destination) %>%
  dplyr::summarise(count = n()) %>%
  rename(origin = destination) %>%
  top_n(5, count) %>%
  left_join(., nodesinfo) %>%
  dplyr::select(origin, Authors, PY, count) %>%
  separate(Authors, into = "First", sep = ",") %>%
  mutate(labels = paste(First, "et al.,", PY))

# Joining them together, Ensuring the LABELS are the abbreviated citations
z <- left_join(nodesinfo,b) %>%
  dplyr::select(labels, everything())
# Turning the data into a graph
graph <- tbl_graph(nodes = z, edges = a, directed = T)

# Graphing it
ggraph(graph, "circlepack") +
  geom_edge_link(alpha=0.1) +
  geom_node_point(aes(size = TC, 
                      color = Climate.Impact, 
                      shape = factor(Climate.Effect,  
                                                              levels = c("Temperature", "None", "pollution", "SLR & Flooding", 
                                                                         "Wildfires & Drought", "extreme weather events") ))) +
  # geom_node_text(aes(label = labels),repel=T) +
  guides(size = "none", color = "none") +
  labs(shape = "Climate Effect",
       caption = "Color is the Climate Impact, size is the # of Citations") +
  
  theme_void() +
  # theme(legend.position="bottom") +
  NULL


## Tabling count of citations within and between climate effects
effect_cites <- data.frame()
for(this.effect in unique(as.character(z$Climate.Effect))){
  effect <- as.character(this.effect)
  within_df <- a[a$origin %in% z[z$Climate.Effect == this.effect,]$id & a$destination %in% z[z$Climate.Effect == this.effect,]$id,]
  edges_within <- nrow(within_df)
  count_within <- nrow(z[z$origin %in% within_df$origin,])
  percent_within <- percent(nrow(z[z$origin %in% within_df$origin,]) / nrow(z[z$Climate.Effect == this.effect,]))
  between_df <- a[a$origin %in% z[z$Climate.Effect == this.effect,]$id & a$destination %in% z[z$Climate.Effect != this.effect,]$id,]
  edges_between <- nrow(between_df)
  count_between <- nrow(z[z$origin %in% between_df$origin,])
  percent_between <- percent(nrow(z[z$origin %in% between_df$origin,]) / nrow(z[z$Climate.Effect == this.effect,]))
  temp_vector <- c(effect, percent_within, percent_between, count_within, count_between, edges_within, edges_between)
  effect_cites <- rbind(effect_cites,temp_vector)
}
colnames(effect_cites) <-  c("effect", "percent citing within", "percent citing between", "count citing within", "count citing between", "count edges within", "count edges between")

# ## EXAMPLE OF HOW TO INTERPRET effect_cites DATAFRAME CREATED ABOVE:
# "117 (87%) of the articles in our sample which concerned the effect of temperature 
# cited other articles in our sample concerning the effect of temperature a total of
# 443 times. This contrasts articles in our sample that concerned the effect of pollution,
# 4 (33%) of which cited other articles in our sample concerning the effect of pollution 
# a total of 4 times"

# ## Tabling count of citations within and between climate impacts DONT DELETE
# impact_cites <- data.frame()
# for(this.effect in unique(as.character(z$Climate.Effect))){
#   effect <- as.character(this.effect)
#   within_df <- a[a$origin %in% z[z$Climate.Effect == this.effect,]$id & a$destination %in% z[z$Climate.Effect == this.effect,]$id,]
#   edges_within <- nrow(within_df)
#   count_within <- nrow(z[z$origin %in% within_df$origin,])
#   percent_within <- percent(nrow(z[z$origin %in% within_df$origin,]) / nrow(z[z$Climate.Effect == this.effect,]))
#   between_df <- a[a$origin %in% z[z$Climate.Effect == this.effect,]$id & a$destination %in% z[z$Climate.Effect != this.effect,]$id,]
#   edges_between <- nrow(between_df)
#   count_between <- nrow(z[z$origin %in% between_df$origin,])
#   percent_between <- percent(nrow(z[z$origin %in% between_df$origin,]) / nrow(z[z$Climate.Effect == this.effect,]))
#   temp_vector <- c(effect, percent_within, percent_between, count_within, count_between, edges_within, edges_between)
#   impact_cites <- rbind(impact_cites,temp_vector)
# }
# colnames(impact_cites) <-  c("effect", "percent citing within", "percent citing between", "count citing within", "count citing between", "count edges within", "count edges between")


#### EVERYTHING BELOW THIS LINE CAN BE DELETED
# edges <- data.frame(t(sapply(from_to,c)))
# edges <- as.data.frame(t(edges)) 
# edges$V1 <- gsub("NA,", "", edges$V1)
# edges$V1 <- gsub("NA", "", edges$V1)
# edges$from <- rownames(edges)
# edges$from <- gsub("X","",edges$from)
# edges <- edges[,c(2,1)]
# colnames(edges) <- c("from", "to")
# edges <- edges %>% 
#   separate_rows(to, sep = ",")
# edges <- edges[edges$to != "",]
# edges <- edges %>% 
#   mutate(from = as.numeric(edges$from),
#          to = as.numeric(edges$to))
# 
# routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
# 
# ggraph(routes_tidy, layout = "circlepack") + 
#   geom_node_point() +
#   geom_edge_link(alpha = 0.8) + 
#   scale_edge_width(range = c(0.2, 2)) +
#   theme_graph()
# 
# 
