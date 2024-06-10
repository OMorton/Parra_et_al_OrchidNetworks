
library(tidyverse)

## read in the data
CITES_MASTER <- list.files(path="G:/My Drive/TUoS/Data_Sources/CITES/CITES_all_records_2023.1", 
                           full.names = TRUE, pattern="*.csv") %>% 
  lapply(read_csv, na = "", col_types = cols(Unit = col_character(), 
                                             Import.permit.RandomID = col_character(),
                                             Export.permit.RandomID = col_character(), 
                                             Origin.permit.RandomID = col_character(),
                                             Purpose = col_character())) %>% 
  bind_rows

## Remove all re-exports as per published CITES guidelines and Pers Comm with UNEP-WMC and CITES
## remove all reports where the origin is stated and is not the same as the exporter.
## This avoids double counting i.e. where a trade passes through multiple countries. 
CITES_TRUE <- CITES_MASTER %>% filter(Origin == Exporter  | is.na(Origin), Family == "Orchidaceae",
                                      Term == "live", is.na(Unit)|Unit == "Number of specimens") %>% 
  mutate(Source_clean = case_when(Source %in% c("W", "X", "R") ~ "Wild",
                                  Source %in% c("A", "C", "D", "F") ~ "Captive",
                                  Source %in% c(NA, "I", "U", "O") ~ "Ambiguous")) %>%
  filter(Source_clean != "Ambiguous", Purpose == "T")

write_rds(CITES_TRUE, "Data/CITES/Orchidaceae.wildcap.commercial.raw.rds")

length(unique(CITES_TRUE$Taxon))
CITES_TRUE %>% filter(Year > 1999) %>% group_by(Source_clean) %>% summarise(length(unique(Taxon)))
CITES_TRUE %>% filter(Year > 1999) %>% summarise(length(unique(Taxon)))

taxa_summary <- CITES_TRUE %>% group_by(Taxon, Order, Family, Genus) %>% 
  summarise(total.traded.individuals = sum(Quantity))

write_csv(taxa_summary, "Data/CITES/Orchidaceae.wildcap.commercial.taxonomy.csv")
