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


### make publication accumulation curve for a given taxa with one curve per country
WOS_acc_curve_per_country <- function(taxa_acc_data, taxa_name, marine) {
  
  # exclude missing data
  if(marine == FALSE) {
    taxa_acc_data <- subset(taxa_acc_data, taxa_acc_data$fieldwork_country != "(Missing)")
  } else {
    taxa_acc_data <- subset(taxa_acc_data, taxa_acc_data$marine_region != "(Missing)")
  }
  
  # pass data from wide to long format
  taxa_acc_data_long <- as_tibble(pivot_longer(taxa_acc_data, 
                                               cols = colnames(taxa_acc_data)[2]:colnames(taxa_acc_data)[ncol(taxa_acc_data)], 
                                               names_to = "year",
                                               values_to = "n_article"))
  
  # convert year from character to numeric
  taxa_acc_data_long$year <- as.numeric(taxa_acc_data_long$year)
  # taxa_acc_data_long$n_article <- as.numeric(levels(taxa_acc_data_long$n_article))[taxa_acc_data_long$n_article]
  
  # test if marine or terrestrial taxa
  if(marine == FALSE) {
    # curve with colour per country and linear y axis
    WOS_curve_art_acc_country <- ggplot(taxa_acc_data_long, aes(x = year, y = n_article, group = fieldwork_country, colour = fieldwork_country)) +
      geom_line(size=1.2) +
      labs(title = paste0("Number of publications for ", taxa_name, " per country")) +
      xlab("Year") + ylab("Number of publications") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_x_continuous("Year", labels = as.character(taxa_acc_data_long$year), breaks = taxa_acc_data_long$year)
    
    pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_", taxa_name, ".pdf"), width = 20, height = 12)
    print(WOS_curve_art_acc_country)
    dev.off()
  } else {
    # curve with colour per marine region and linear y axis
    WOS_curve_art_acc_marine_region <- ggplot(taxa_acc_data_long, aes(x = year, y = n_article, group = marine_region, colour = marine_region)) +
      geom_line(size=1.2) +
      labs(title = paste0("Number of publications for ", taxa_name, " per marine region")) +
      xlab("Year") + ylab("Number of publications") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_x_continuous("Year", labels = as.character(taxa_acc_data_long$year), breaks = taxa_acc_data_long$year)
    
    pdf(file = paste0("./output/plots/WOS_curve_art_acc_marine_region_", taxa_name, ".pdf"), width = 20, height = 12)
    print(WOS_curve_art_acc_marine_region)
    dev.off()
  }
}
