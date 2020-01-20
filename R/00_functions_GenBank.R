#################################################
#
# Biodivmex project
#
# 00_functions_GenBank.R
#
# jan.perret@cefe.cnrs.fr
#################################################


# function to extract general informations for a diven taxa
GB_extract_general_info <- function(kingdom_data, taxa_data, taxa_data_med, taxa_name) {
  
  # initiaze table to store the 6 general general descriptive informations
  descritive_table <- setNames(data.frame(matrix(ncol = 7, nrow = 1)),
                               c("taxa", "n_seq", "taxa_n_seq", "loc_rate", "n_seq_med", "sp_level_rate_med", "n_sp_med"))
  descritive_table$taxa <- taxa_name
  
  # 1. number of sequences in GenBank for the reign (metazoa, thracheophyta or fungi)
  descritive_table$n_seq <- length(kingdom_data$access_num)
  
  # 2. number of sequences for the chosen taxonomic group (identical for fungi and plants)
  descritive_table$taxa_n_seq <- length(taxa_data$access_num)
  
  # 3. percentage of sequences with a sample origin information
  descritive_table$loc_rate <- round((sum(!is.na(taxa_data$sample_origin))/length(taxa_data$access_num))*100, 2)
  
  # 4. number of sequences affiliated to a country of the mediterranean basin for the taxa
  descritive_table$n_seq_med <- length(taxa_data_med$access_num)
  
  # 5. percentage of sequences affiliated at least to species level (can be more precise) for the taxa in the mediterranean basin
  a = sum(!is.na(taxa_data_med$species_level)) - length(which(taxa_data_med$species_level == "hybrid"))
  descritive_table$sp_level_rate_med <- round((a/length(taxa_data_med$species_level))*100, 2)
  
  # 6. number of different species names for the taxa in the mediterranean basin
  descritive_table$n_sp_med <- length(levels(as.factor(taxa_data_med$species_level))) # here levels() if better than unique() because it don't count NA as a value
  
  if ("hybrid" %in% levels(as.factor(taxa_data_med$species_level))) {
    descritive_table$n_sp_med <- descritive_table$n_sp_med - 1 # minus one if the "hybrid" level is present because it is not a species name
  }
  
  return(descritive_table)
}

