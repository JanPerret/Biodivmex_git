### make map with EEZ filling by number of species
# this function requires values for the folowing objects not present in the arguments :
# med_clipped_marine
# med_EEZ
# point_grid_marine
# palette

GB_map_number_species_marine_taxa <- function(species_level_tab, subtitle_text, taxa_name) {
  
  # add the information to plot to med_EEZ
  tab_for_join <- species_level_tab[,c(1,5)]
  
  # join information to MULTIPOLYGON object
  med_EEZ_joined <- left_join(med_EEZ, tab_for_join, by = c("country" = "country"))
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid_marine, aes(x = point_grid_marine$X, y = point_grid_marine$Y), color = "grey83", size = 0.2) +  # point grid to make the sea
    geom_sf(data = med_EEZ_joined, aes(fill = n_sp)) +  # EEZ layer with the colour filling
    geom_sf(data = med_clipped_marine, mapping = aes(), fill = "white") +  # countries background with white filling
    scale_fill_gradientn(name="Number of species", colours=palette, na.value="white")+
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(paste("Number of species with at least one sequence in GenBank for", taxa_name), subtitle = subtitle_text) +
    theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
    labs(x=NULL, y=NULL) +
    theme(plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="right",
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(10,10,10,10))
  
  return(map)
}