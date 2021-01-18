### make map with marine region filling by number of articles
# this function requires values for the folowing objects not present in the arguments :
# med_clipped_marine
# med_marine_region
# point_grid_marine
# palette
# circle_centroid
# western_med
# eastern_med
# marine_region_centroids

WOS_map_number_articles_marine_synthesis <- function(locality_table, subtitle_text, taxa_name, color_scale_mid) {
  
  # add the information to plot to med_marine_region
  tab_for_join <- locality_table[,c(1, 6)]
  
  # join information to MULTIPOLYGON object
  med_marine_region_joined <- left_join(med_marine_region, tab_for_join, by = c("marine_reg" ="marine_region"))
  
  med_marine_region_joined_under_layer <- subset(med_marine_region_joined, 
                                                 med_marine_region_joined$marine_reg == "Northwestern Mediterranean Sea" |
                                                 med_marine_region_joined$marine_reg == "Tunisian Plateau")
  
  med_marine_region_joined <- subset(med_marine_region_joined,
                                     med_marine_region_joined$marine_reg != "Northwestern Mediterranean Sea" &
                                     med_marine_region_joined$marine_reg != "Tunisian Plateau")
  
  # join information for filling the circles
  circle_centroid_joined <- left_join(circle_centroid, tab_for_join, by = c("marine_reg" ="marine_region"))
  
  # make color scale values vector
  # because with scale_fill_gradientn, values have to be provided in rescaled space (0-1)
  scale_mid <- color_scale_mid/max(locality_table$n_articles_percent, na.rm = TRUE)
  # myscale <- c(seq(from = 0, to = scale_mid, length.out = 5), seq(from = scale_mid, to = 1, length.out = 5)[-1]) # scale with 9 values
  myscale <- c(seq(from = 0, to = scale_mid, length.out = 4), seq(from = scale_mid, to = 1, length.out = 4)[-1]) # scale with 7 values
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid_marine, aes(x = point_grid_marine$X, y = point_grid_marine$Y), color = "grey83", size = 0.2) +  # point grid to make the sea
    geom_sf(data = med_clipped_marine, mapping = aes(), fill = "white") +  # countries background with white filling
    geom_sf(data = med_marine_region_joined_under_layer, aes(fill = n_articles_percent)) +  # layer with NW med and Tunisian Plateau to avoid overlap
    geom_sf(data = med_marine_region_joined, aes(fill = n_articles_percent)) +  # layer with the other marine regions
    geom_segment(aes(x = 5.165294, y = 36.667825, xend = -0.26, yend = 32.33), size = 0.7, colour = "green2") + # segment for western med
    geom_segment(aes(x = 30.824110, y = 36.848481, xend = 36.422313, yend = 42), size = 0.7, colour = "royalblue1") + # segment for eastern med
    geom_point(data = circle_centroid_joined, aes(x = x, y = y, fill = n_articles_percent), shape=21, size = 70, stroke = 1, col = c("green2", "royalblue1")) + # circles with the filling for the 2 big regions
    # scale_fill_gradientn(name="Number of articles", colours=palette, na.value="white") +
    scale_fill_gradientn(name="Percentage of the total number of articles", colours = palette_synthesis, values = myscale, na.value = "white") + 
    geom_sf(data = western_med, fill = NA, colour = "green2", size = 0.5, inherit.aes = FALSE) +  # western med bounders
    geom_sf(data = eastern_med, fill = NA, colour = "royalblue1", size = 0.5, inherit.aes = FALSE) +  # eastern med bounders
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(paste("Number of articles in Web of Science about", taxa_name), subtitle = subtitle_text) +
    theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
    labs(x=NULL, y=NULL) +
    theme(plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="right",
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(10,10,10,10))

  # ### make pie charts with from_country / outside_med / inside_med information
  # # data for the pie charts
  # locality_table <- locality_table[,c(1:3)]
  # loc_tab_long <- as_tibble(pivot_longer(locality_table, 
  #                                        cols = colnames(locality_table)[2]:colnames(locality_table)[ncol(locality_table)], 
  #                                        names_to = "author_loc",
  #                                        values_to = "n_articles"))
  # 
  # # join the informations to plot to marine_region_centroids
  # marine_region_centroids_joined <- left_join(marine_region_centroids, locality_table, by = c("marine_reg" = "marine_region"))
  # 
  # # countries' FID list
  # centroids_ID_list <- unique(as.integer(marine_region_centroids_joined$FID))
  # 
  # # make a list with the pie-charts objects
  # pie_chart_list <- 
  #   lapply( (centroids_ID_list), function(n){
  #     
  #     # title of the future pie chart
  #     coun_name <- marine_region_centroids_joined[marine_region_centroids_joined$FID == as.character(n),]$marine_reg
  #     
  #     # make a small tibble containing just the 2 summary lines inside_med / outside_med
  #     wos_totals_marine_region <- loc_tab_long[loc_tab_long$marine_region == coun_name,]
  #     wos_totals_marine_region <- wos_totals_marine_region %>% 
  #                                        group_by(author_loc) %>% 
  #                                        summarise (sum_n_articles = sum(n_articles))
  #     
  #     gt_plot <- ggplotGrob(
  #       
  #       # make the pie-charts
  #       ggplot(wos_totals_marine_region, aes(x=1, y = sum_n_articles, fill = author_loc)) +
  #         geom_bar(width = 1, stat = "identity", colour = "black") +
  #         coord_polar("y", start=0) +
  #         geom_text(aes(label = sum_n_articles, group = author_loc),
  #                   position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
  #         scale_fill_manual(values = c("#bdbdbd", "#FFFFFF")) +
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
  #                              xmin = marine_region_centroids$x[[i]] - pie_half_width,
  #                              xmax = marine_region_centroids$x[[i]] + pie_half_width,
  #                              ymin = marine_region_centroids$y[[i]] - pie_half_height,
  #                              ymax = marine_region_centroids$y[[i]] + pie_half_height)
  #   pie_charts_annotation_list[[i]] <- value
  # }
  # 
  # result_plot <- Reduce('+', pie_charts_annotation_list, map)
  # 
  # return(result_plot)
  return(map)
}
