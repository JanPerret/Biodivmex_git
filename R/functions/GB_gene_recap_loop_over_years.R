### extract the same information than GB_extract_gene() but for each year

GB_gene_recap_loop_over_years <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  gene_table <- setNames(data.frame(matrix(ncol = 28, nrow = 0)), c("taxa", "year", paste0(gene_list), "n_NA", "tot_n_seq"))
  
  # list years since the first sequence was deposited in GenBank (for our taxa + genes conditions)
  vect_year <- as.integer(c(1987:2020)) # first sequences is from 1987 in our dataframe
  
  # loop over years
  for (year in vect_year) {
    given_year <- as.integer(year) # because otherway it doesn't work with filter()
    
    # subset tables for the given year
    taxa_data_med_year <- taxa_data_med %>% filter(year == given_year)
    
    # extract information of the year tables with GB_extract_general_info() function
    tab_given_year <- GB_extract_gene(taxa_data_med = taxa_data_med_year, taxa_name = taxa_name)
    
    # binding extracted informations to the output table
    tab_given_year <- cbind(year = given_year, tab_given_year)
    gene_table <- rbind(gene_table, tab_given_year)
  }
  
  return(gene_table)
}