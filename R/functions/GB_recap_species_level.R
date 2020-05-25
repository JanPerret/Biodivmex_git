### make recap tables with rate of sequences assigned at species level and number of different species (or species names)

GB_recap_species_level <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  sp_level_table <- setNames(data.frame(matrix(ncol = 5, nrow = 27)),
                             c("country", "taxa", "sp_level_rate", "n_seq", "n_sp"))
  sp_level_table$country <- sample_origin_med_levels
  sp_level_table$taxa <- taxa_name
  
  # loop to fill the table
  for (i in 1:length(sp_level_table$country)) {
    country = sp_level_table$country[i]
    inter_tab <- subset(taxa_data_med, taxa_data_med$sample_origin == country)
    
    # country number of sequences
    sp_level_table$n_seq[i] <- length(inter_tab$species_level)
    
    # species level rate
    a = sum(!is.na(inter_tab$species_level)) - length(which(inter_tab$species_level == "hybrid")) # total number of sequences without hybrids
    sp_level_table$sp_level_rate[i] <- round((a/length(inter_tab$species_level))*100, 2) # in percentage
    
    # number of species
    sp_level_table$n_sp[i] <- length(levels(as.factor(inter_tab$species_level))) # here levels() if better than unique() because it don't count NA as a value
    
  }
  
  return(sp_level_table)
}