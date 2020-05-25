### make map subtitle text

GB_make_map_subtitle <- function(taxa_table, kingdom_name, taxa_name) {
  
  text_reign_n_seq <- paste("Total number of sequences in GenBank for ", kingdom_name, " : ", taxa_table$n_seq[which(taxa_table$taxa == taxa_name)], sep="")
  text_taxa_n_seq <- paste("Number of sequences for the taxa : ", taxa_table$taxa_n_seq[which(taxa_table$taxa == taxa_name)], sep="")
  text_loc_rate <- paste("Sample origin fill rate : ", taxa_table$loc_rate[which(taxa_table$taxa == taxa_name)], " %", sep="")
  text_taxa_med_n_seq <- paste("Number of sequences for the taxa : ", taxa_table$n_seq_med[which(taxa_table$taxa == taxa_name)], sep="")
  text_n_species <- paste("Number of species with at least one sequence : ", taxa_table$n_sp_med[which(taxa_table$taxa == taxa_name)], sep="")
  
  text_subtitle <- paste("At world scale : ", text_reign_n_seq, " ; ", text_taxa_n_seq, " ; ", text_loc_rate, "\n",
                         "In the mediterranean basin : ", text_taxa_med_n_seq, " ; ", text_n_species, sep="")
  
  return(text_subtitle)
}