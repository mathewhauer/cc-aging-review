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

## IMPORTING DATA
documents_reviewed <- read.xlsx("../R/DATA-RAW/papers2.xlsx") %>%
  filter(Summary != "N/A")
documents_reviewed[177,3] <- "10.1016/S0140-6736(21)01209-5"   #DOI was missing for this observation, so I write it in here
documents_reviewed <- documents_reviewed[documents_reviewed$DI != "10.23749/mdl.v109i3.6851",] #This DOI can't be found by crossref package THIS SHOULD BE NOTED IN METHODS OR NETWORK SETION

## Pulling metadata for references/citations
reviewed_dois <- documents_reviewed$DI
reviewed_articles_meta <- cr_works(dois = reviewed_dois) %>% 
  purrr::pluck("data")

## Creating objects to be referenced in network rangling
ref_list <- list()
ref_vector <- vector()
from_to <- list()
cited_ids <- vector()
nodes <- as.data.frame(reviewed_dois) %>% 
  mutate(id = as.character(1:176))
colnames(nodes) <- c("label", "id")

## Wrangling into network format
for(i in 1:nrow(reviewed_articles_meta)){
  ref_vector <- reviewed_articles_meta[[32]][[i]]$DOI
  ref_list[[i]] <- list(ref_vector)
  for(j in 1:length(reviewed_dois)){
    cited_ids <- ifelse(reviewed_dois[[j]] %in% ref_list[[i]][[1]], 
                        ifelse(is.na(cited_ids) == "logical(0)",
                               paste(nodes$id[[j]]),
                               paste(cited_ids, nodes$id[[j]], sep = ",")), 
                        cited_ids)
    from_to[[i]] <- list(cited_ids)
  }
    cited_ids <- vector()
}
edges <- data.frame(t(sapply(from_to,c)))
edges <- as.data.frame(t(edges)) 
edges$V1 <- gsub("NA,", "", edges$V1)
edges$V1 <- gsub("NA", "", edges$V1)
edges$from <- rownames(edges)
edges$from <- gsub("X","",edges$from)
edges <- edges[,c(2,1)]
colnames(edges) <- c("from", "to")
edges <- edges %>% 
  separate_rows(to, sep = ",")
edges <- edges[edges$to != "",]
edges <- edges %>% 
  mutate(from = as.numeric(edges$from),
         to = as.numeric(edges$to))

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(routes_tidy, layout = "circlepack") + 
  geom_node_point() +
  geom_edge_link(alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  theme_graph()


