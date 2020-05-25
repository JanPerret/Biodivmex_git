### make sequence accumulation curve for a given taxa with one curve per country

GB_acc_curve_per_country <- function(taxa_acc_data, taxa_name) {
  
  # pass data from wide to long format
  taxa_acc_data_long <- as_tibble(pivot_longer(taxa_acc_data, 
                                               cols = colnames(taxa_acc_data)[2]:colnames(taxa_acc_data)[ncol(taxa_acc_data)], 
                                               names_to = "year",
                                               values_to = "n_seq"))
  
  # convert year from character to numeric
  taxa_acc_data_long$year <- as.numeric(taxa_acc_data_long$year)
  # taxa_acc_data_long$n_seq <- as.numeric(levels(taxa_acc_data_long$n_seq))[taxa_acc_data_long$n_seq]
  
  # truncate at 1995 because the first sequences are from 1996
  taxa_acc_data_long <- subset(taxa_acc_data_long, taxa_acc_data_long$year > 1994)
  
  # curve with colour per country and linear y axis
  GenBank_curve_seq_acc_country <- ggplot(taxa_acc_data_long, aes(x = year, y = n_seq, group = sample_origin, colour = sample_origin)) +
                                      geom_line(size=1.2) +
                                      labs(title = paste0("Number of sequences per country for ", taxa_name)) +
                                      xlab("Year") + ylab("Number of sequences") +
                                      theme_bw() +
                                      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                      scale_x_continuous("Year", labels = as.character(taxa_acc_data_long$year), breaks = taxa_acc_data_long$year) +
                                      directlabels::geom_dl(aes(label = sample_origin), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
                                      coord_cartesian(clip = 'off') +
                                      theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
                                      expand_limits(x = c(2021))
  
  return(GenBank_curve_seq_acc_country)
}