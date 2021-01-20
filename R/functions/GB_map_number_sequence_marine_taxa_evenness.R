### make map with EEZ filling by number of sequences and pie-charts indicating sequencer locality
# this function requires values for the folowing objects not present in the arguments :
# med_clipped_marine
# med_EEZ
# EEZ_centroids
# point_grid_marine
# palette_evenness

GB_map_number_sequence_marine_taxa_evenness <- function(locality_table, subtitle_text, taxa_name) {
  
  # add information to plot to med_EEZ
  # plant_article_loc_tab
  tab_for_join <- locality_table[,c(1, 8)]
  
  # join information to MULTIPOLYGON object
  med_EEZ_joined <- left_join(med_EEZ, tab_for_join, by = c("country" = "sample_origin"))
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid_marine, aes(x = point_grid_marine$X, y = point_grid_marine$Y), color = "grey83", size = 0.2) + # point grid to make the sea
    geom_sf(data = med_EEZ_joined, aes(fill = simpson)) + # EEZ layer with the colour filling
    geom_sf(data = med_clipped_marine, mapping = aes(), fill = "white") + # countries background with white filling
    scale_fill_gradientn(name="Simpson evenness index", colours = palette_evenness, na.value="white")+
    # scale_fill_gradientn(name="Percentage of the total number of sequences", colours = palette_evenness, values = myscale, na.value = "white") + 
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(paste(taxa_name, "evenness in GenBank"), subtitle = subtitle_text) +
    theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
    labs(x=NULL, y=NULL) +
    theme(plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="right",
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(10,10,10,10))


  # ### make pie charts with from_country / outside_med / inside_med information
  # # data for the pie charts
  # locality_table <- locality_table[,c(1:4)]
  # loc_tab_long <- as_tibble(pivot_longer(locality_table, 
  #                                        cols = colnames(locality_table)[2]:colnames(locality_table)[ncol(locality_table)], 
  #                                        names_to = "author_loc",
  #                                        values_to = "n_seq"))
  # 
  # # join the informations to plot to EEZ_centroids
  # EEZ_centroids_joined <- left_join(EEZ_centroids, locality_table, by = c("country" = "sample_origin"))
  # 
  # # countries' FID list
  # centroids_ID_list <- unique(as.integer(EEZ_centroids_joined$FID))
  # 
  # # make a list with the pie-charts objects
  # pie_chart_list <- 
  #   lapply( (centroids_ID_list), function(n){
  #     
  #     # title of the future pie chart
  #     coun_name <- EEZ_centroids_joined[EEZ_centroids_joined$FID == as.character(n),]$country
  #     
  #     # make a small tibble containing just the 3 summary lines from_country / inside_med / outside_med
  #     wos_totals_country <- loc_tab_long[loc_tab_long$sample_origin == coun_name,]
  #     wos_totals_country <- wos_totals_country %>% 
  #       group_by(author_loc) %>% 
  #       summarise (sum_n_articles = sum(n_seq))
  #     
  #     gt_plot <- ggplotGrob(
  #       
  #       # make the pie-charts
  #       ggplot(wos_totals_country, aes(x=1, y = sum_n_articles, fill = author_loc)) +
  #         geom_bar(width = 1, stat = "identity", colour = "black") +
  #         coord_polar("y", start=0) +
  #         geom_text(aes(label = sum_n_articles, group = author_loc),
  #                   position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
  #         scale_fill_manual(values = c("#636363", "#bdbdbd", "#FFFFFF")) +
  #         theme_minimal()+
  #         ggtitle(coun_name)+
  #         theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5, vjust = -0.5,margin=margin(b = 2, unit = "pt")))+
  #         theme(axis.line=element_blank(),
  #               axis.text.x=element_blank(),
  #               axis.text.y=element_blank(),
  #               axis.ticks=element_blank(),
  #               axis.title.x=element_blank(),
  #               axis.title.y=element_blank(),
  #               legend.position="none",
  #               panel.background=element_blank(),
  #               panel.border=element_blank(),
  #               panel.grid.major=element_blank(),
  #               panel.grid.minor=element_blank(),
  #               plot.background=element_blank())+
  #         theme(plot.margin = unit(c(0,0,0,0), "cm"))
  #       
  #     )
  #   } )
  # 
  # ### insert the pie-charts on the map
  # pie_charts_annotation_list <- vector(mode = "list", length = length(pie_chart_list))
  # pie_half_width = 1.3
  # pie_half_height = 1.3
  # 
  # for (i in 1:length(pie_chart_list)) {
  #   value <- annotation_custom(grob = pie_chart_list[[i]],
  #                              xmin = EEZ_centroids$x[[i]] - pie_half_width,
  #                              xmax = EEZ_centroids$x[[i]] + pie_half_width,
  #                              ymin = EEZ_centroids$y[[i]] - pie_half_height,
  #                              ymax = EEZ_centroids$y[[i]] + pie_half_height)
  #   pie_charts_annotation_list[[i]] <- value
  # }
  # 
  # result_plot <- Reduce('+', pie_charts_annotation_list, map)
  # 
  # return(result_plot)
  return(map)
}
