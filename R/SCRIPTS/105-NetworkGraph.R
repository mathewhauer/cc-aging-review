###------FIGURE Network Graph-----
## @knitr networkgraph

#install.packages("newtork")
#library(network)
library(tidygraph)
library(ggraph)

## Categorized & Separated climate effects & impacts
effect_impact <- read.xlsx("../R/DATA-RAW/categorized effects impacts.xlsx")

effects <- effect_impact %>% 
  distinct(Collapsed.Effect) %>% 
  rename(label = Collapsed.Effect)

impacts <- effect_impact %>% 
  distinct(Collapsed.Impact) %>% 
  rename(label = Collapsed.Impact)

nodes <- full_join(effects, impacts, by = "label") %>% 
  rowid_to_column("id")

cooccurence <- effect_impact %>% 
  group_by(Collapsed.Effect, Collapsed.Impact) %>% 
  summarise(weight = n()) %>% 
  ungroup()

edges <- cooccurence %>% 
  left_join(nodes, by = c("Collapsed.Effect" = "label")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("Collapsed.Impact" = "label")) %>% 
  rename(to = id)

edges <- edges[,c(4,5,3)]

#routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Weight") +
  theme_graph()




