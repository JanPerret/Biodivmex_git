#################################################
#
# Biodivmex project
#
# 00_functions_WOS.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### transform table with number of sequences in GenBank per country (rows) and years (columns) for accumulation curve
WOS_accumulate <- function(year_tab, first_column) {
  
  len <- c(1:nrow(year_tab))
  large <- c((first_column+1):ncol(year_tab))
  
  for (i in len){ # loop over rows
    for (n in large){ # loop over columns
      
      year_tab[i,n] <- year_tab[i,n] + year_tab[i,n-1]
      
    }
  }
  
  return(year_tab)
}

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