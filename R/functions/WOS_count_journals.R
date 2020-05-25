### count number of journals for a given taxa and time span

WOS_count_journals <- function(taxa_journals_data) {
  
  # pass year from factor to numeric for the subset() condition
  taxa_journals_data$year <- as.numeric(levels(taxa_journals_data$year))[taxa_journals_data$year]
  
  # vector to return
  journals_vect <- c(rep(NA, length(year_list)))
  
  # loop through year_list
  for (i in 1:length(year_list)) {
    given_year <- year_list[i]
    year_df <- subset(taxa_journals_data, taxa_journals_data$year <= given_year)
    journals_vect[i] <- length(unique(year_df$publisher))
  }
  return(journals_vect)
}