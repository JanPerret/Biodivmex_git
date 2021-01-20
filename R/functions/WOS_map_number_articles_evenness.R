### make map with country filling by number of articles and pie-charts indicationg corresponding author locality
# this function requires values for the folowing objects not present in the arguments :
# med_clipped
# med_countries_islands
# med_area_bounderies
# med_centroids
# point_grid
# palette_evenness

WOS_map_number_articles_evenness <- function(locality_table, subtitle_text, taxa_name) {
  
  # add information to plot to med_countries_islands
  # plant_article_loc_tab
  tab_for_join <- locality_table[, c(1, 18)]
  
  # join information to MULTIPOLYGON object
  med_countries_islands_joined <- left_join(med_countries_islands, tab_for_join, by = c("country" = "fieldwork_country"))
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid, aes(x = point_grid$X, y = point_grid$Y), color = "grey83", size = 0.2) + # point grid to make the sea filling
    geom_sf(data = med_clipped, mapping = aes(), fill = "white") +                                         # countries background with white filling
    geom_sf(data = med_countries_islands_joined, aes(fill = simpson)) +                                    # countries and islands layer with the colour filling
    geom_sf(data = med_area_bounderies, mapping = aes(), color = "orangered4", size = 0.6, fill = NA) +    # mediterranean area outline
    scale_fill_gradientn(name="Simpson evenness index", colours=palette_evenness, na.value = "white") +
    # scale_fill_gradientn(name="Percentage of the total number of articles", colours = palette_evenness, values = myscale, na.value = "white") + 
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(paste(taxa_name, "evenness in Web of Science"), subtitle = subtitle_text) +
    theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
    labs(x=NULL, y=NULL) +
    theme(plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="right",
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(10,10,10,10)) #+
  #   geom_segment(aes(x = 18.191610, y = 43.183912, xend = 24.27514, yend = 47.11614), size = 0.5) + # segment for Bosnia
  #   geom_segment(aes(x = 19.044711, y = 42.527407, xend = 24.27514, yend = 43.58586), size = 0.5) + # segment for Montenegro
  #   geom_segment(aes(x = 36.154312, y = 33.837813, xend = 38.89619, yend = 32.015), size = 0.5) +   # segment for Lebanon
  #   geom_segment(aes(x = 35.258268, y = 32.170236, xend = 38.89619, yend = 27.615), size = 0.5) +   # segment for Palestine
  #   geom_segment(aes(x = 15.87, y = 44.05, xend = 18.6, yend = 47.1), size = 0.5)                   # segment for Croatia
  # 
  # ### make pie charts with from_country / outside_med / inside_med information
  # # data for the pie charts
  # locality_table <- locality_table[,c(1:4)]
  # loc_tab_long <- as_tibble(pivot_longer(locality_table, 
  #                                        cols = colnames(locality_table)[2]:colnames(locality_table)[ncol(locality_table)], 
  #                                        names_to = "author_loc",
  #                                        values_to = "n_articles"))
  # 
  # # join the informations to plot to med_centroids
  # med_centroids_joined <- left_join(med_centroids, locality_table, by = c("country" = "fieldwork_country"))
  # 
  # # countries' FID list
  # centroids_ID_list <- unique(as.integer(med_centroids_joined$FID))
  # 
  # # make a list with the pie-charts objects
  # pie_chart_list <- 
  #   lapply( (centroids_ID_list), function(n){
  #     
  #     # title of the future pie chart
  #     coun_name <- med_centroids_joined[med_centroids_joined$FID == as.character(n),]$country
  #     
  #     # make a small tibble containing just the 3 summary lines from_country / inside_med / outside_med
  #     wos_totals_country <- loc_tab_long[loc_tab_long$fieldwork_country == coun_name,]
  #     wos_totals_country <- wos_totals_country %>% 
  #       group_by(author_loc) %>% 
  #       summarise (sum_n_articles = sum(n_articles))
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
  #                              xmin = med_centroids$x[[i]] - pie_half_width,
  #                              xmax = med_centroids$x[[i]] + pie_half_width,
  #                              ymin = med_centroids$y[[i]] - pie_half_height,
  #                              ymax = med_centroids$y[[i]] + pie_half_height)
  #   pie_charts_annotation_list[[i]] <- value
  # }
  # 
  # result_plot <- Reduce('+', pie_charts_annotation_list, map)
  # 
  # return(result_plot)
  return(map)
}
