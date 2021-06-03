### get list of distinct species name per island from the tables *taxa-name*_data_med

GB_island_species_list <- function(taxa_data_med) {
  
  my_species_list <- taxa_data_med %>% 
    filter(sample_origin %in% c("Balearic Islands", "Corsica", "Crete",
                                "Cyprus", "Sardinia", "Sicily", "Malta")) %>% # keep only sequences from the islands
    # arrange(sample_origin) %>% # sort the tibble by alphabetical order of the column sample_origin
    group_by(sample_origin) %>% # group by island
    distinct(species_level) %>% # keep all distinct species names per island
    filter(!is.na(species_level)) %>% # remove NAs
    filter(species_level != "hybrid") %>% # remove hybrids
    arrange(species_level) %>% # sort the tibble by alphabetical order of the column species_level
    mutate(row = row_number()) %>% # create a column with a unique identifier for each row (otherwise pivot_wider creates list-cols)
    pivot_wider(names_from = sample_origin, values_from = species_level) %>%
    select(-row) # drop the identifier column
  
  my_species_list <- select(my_species_list, order(colnames(my_species_list))) # reorder columns alphabetically
  
  # reorder columns so they are in the same order for every taxa
  # my_species_list <- my_species_list[c("Balearic Islands", "Corsica", "Crete", "Cyprus", "Sardinia", "Sicily", "Malta")]
  
  return(my_species_list)
}

