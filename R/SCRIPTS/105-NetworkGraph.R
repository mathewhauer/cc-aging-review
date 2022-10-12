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
# reviewed_dois <- documents_reviewed$DI
# reviewed_articles_meta <- cr_works(dois = reviewed_dois) %>%
#   purrr::pluck("data")
# saveRDS(reviewed_articles_meta, "./R/DATA-PROCESSED/reviewed_articles_meta.rds")
reviewed_articles_meta <- readRDS("./R/DATA-PROCESSED/reviewed_articles_meta.rds")


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


