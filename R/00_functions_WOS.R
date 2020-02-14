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
