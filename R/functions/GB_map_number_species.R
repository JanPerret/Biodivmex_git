### make map with country filling by number of species
# this function requires values for the folowing objects not present in the arguments :
# med_clipped
# med_countries_islands
# point_grid
# palette
GB_map_number_species <- function(species_level_tab, subtitle_text, taxa_name) {
  
  # add the information to plot to med_countries_islands
  tab_for_join <- species_level_tab[,c(1,5)]
  
  # join information to MULTIPOLYGON object
  med_countries_islands_joined <- left_join(med_countries_islands, tab_for_join, by = c("country" = "country"))
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid, aes(x = point_grid$X, y = point_grid$Y), color = "grey83", size = 0.2) +  # point grid to make the sea filling
    geom_sf(data = med_clipped, mapping = aes(), fill = "white") +                                          # countries background with white filling
    geom_sf(data = med_countries_islands_joined, aes(fill = n_sp)) +                                        # countries and islands layer with the colour filling
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