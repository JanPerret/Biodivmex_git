### extract the same information than GB_extract_general_info() but for each year

GB_loop_over_years <- function(kingdom_data, taxa_data, taxa_data_med, taxa_name) {
  
  # initiaze table to store the 6 general descriptive informations
  general_table <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                            c("year", "taxa", "n_seq", "taxa_n_seq", "loc_rate", "n_seq_med", "sp_level_rate_med", "n_sp_med"))
  
  # list years since the first sequence was deposited in GenBank (for our taxa + genes conditions)
  vect_year <- as.integer(c(1987:2020)) # first sequences is from 1987 in our data
  
  # loop over years
  for (year in vect_year) {
    given_year <- as.integer(year) # because else it doesn't work with filter()
    
    # subset tables for the given year
    kingdom_data_year <- kingdom_data %>% filter(year == given_year)
    taxa_data_year <- taxa_data %>% filter(year == given_year)
    taxa_data_med_year <- taxa_data_med %>% filter(year == given_year)
    
    # extract information of the year tables with GB_extract_general_info() function
    tab_given_year <- GB_extract_general_info(kingdom_data = kingdom_data_year, taxa_data = taxa_data_year, taxa_data_med = taxa_data_med_year, taxa_name = taxa_name)
    
    # binding extracted informations to the output table
    tab_given_year <- cbind(year = given_year, tab_given_year)
    general_table <- rbind(general_table, tab_given_year)
  }
  
  return(general_table)
}