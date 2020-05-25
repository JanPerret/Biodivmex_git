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

  # test if marine or terrestrial taxa
  if(marine == FALSE) {
    # curve with colour per country and linear y axis
    WOS_curve_art_acc <- ggplot(taxa_acc_data_long, aes(x = year, y = n_article, group = fieldwork_country, colour = fieldwork_country)) +
      geom_line(size=1.2) +
      labs(title = paste0("Number of publications per country for ", taxa_name)) +
      xlab("Year") + ylab("Number of publications") +
      labs(colour = "Fieldwork country") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_x_continuous("Year", labels = as.character(taxa_acc_data_long$year), breaks = taxa_acc_data_long$year) +
      directlabels::geom_dl(aes(label = fieldwork_country), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
      coord_cartesian(clip = 'off') + # allow labels to go past the canvas boundaries
      theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) + # remove legend and adjust margins (margins are t-r-b-l)
      expand_limits(x = c(2021.5)) # expand plot window x limit to ensure it includes 2021.5 (space needed for the longest label)

  } else {
    # curve with colour per marine region and linear y axis
    WOS_curve_art_acc <- ggplot(taxa_acc_data_long, aes(x = year, y = n_article, group = marine_region, colour = marine_region)) +
      geom_line(size=1.2) +
      labs(title = paste0("Number of publications per marine region for ", taxa_name)) +
      xlab("Year") + ylab("Number of publications") +
      labs(colour = "Marine region") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
      scale_x_continuous("Year", labels = as.character(taxa_acc_data_long$year), breaks = taxa_acc_data_long$year) +
      directlabels::geom_dl(aes(label = marine_region), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
      coord_cartesian(clip = 'off') +
      theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
      expand_limits(x = c(2022))
    
  }
  return(WOS_curve_art_acc)
}