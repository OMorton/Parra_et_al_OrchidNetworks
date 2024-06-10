library(tidygraph)
library(ggraph)
library(tidyverse)
library(rnaturalearth)
library(terra)

options(scipen = 999)

## read in data
## get orchid data
orchid_raw <- readRDS("Data/CITES/Orchidaceae.wildcap.commercial.raw.rds")

## get world centroids
world <- ne_countries(type = "countries", returnclass = "sf", scale = 50) %>% 
  mutate(iso_a2 = ifelse(iso_a3 == "NAM", "NA", iso_a2))
ce <- centroids(vect(select(world, name, type, iso_a2, geometry)),
                inside = TRUE)
world_ce <- as.data.frame(ce, geom = "XY") %>% 
  mutate(iso_a2 = ifelse(name == "Namibia", "NA", iso_a2))

#### Data prep ####

w1 <- orchid_raw %>% filter(Source_clean == "Wild") %>% group_by(Exporter, Importer) %>%
  summarise(n = sum(Quantity)) %>% rename("from" = "Exporter", "to" = "Importer") %>%
  ## remove XX, RE (Reunion - France), MQ (Martinique - France) - do better 
  filter(!from %in% c("XX", "RE", "MQ"), !to %in% c("XX", "RE", "MQ"))

nodes <- data.frame(name = c(unique(c(w1$from, w1$to))))

network <- tbl_graph(nodes = nodes, edges = w1, directed = TRUE)

network2 <- network %>% activate(nodes) %>%
  left_join(world_ce, by = c("name" = "iso_a2"))

r = data.frame(lx = NA , ly = NA)

pl1 <- ggraph(graph = network2, r) +
  geom_sf(data = world, fill = NA, size = .01) +
  geom_edge_fan(aes(colour = n, alpha = ..index..), width = 1) +
  scale_edge_alpha(guide = "edge_direction") +
  geom_node_point(aes(filter = !node_is_isolated()), shape = 21, size = 4, fill = "black") +
  #geom_node_point(aes(filter = !node_is_isolated()), shape = 21, size = 10, fill = "white") +
  #geom_node_text(aes(label = country_code, filter = !node_is_isolated())) + 
  scale_edge_colour_viridis(option = "turbo", direction = 1, 
                            guide = guide_edge_colourbar(available_aes = c("edge_colour"), display = "gradient"),
                            trans = "log10", name = "Adjusted edge weight") +
  guides(edge_alpha="none", edge_width = "none") +
  coord_sf(ylim = c(90, -50)) +
  theme_graph(base_size = 24) +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"), legend.title = element_text(face = "bold"))


ggsave(pl1, file = "Outputs/Prelim/raw_wild_plt.png", width = 20, height = 12)
