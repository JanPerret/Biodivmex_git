#################################################
#
# Biodivmex project
#
# 00_functions_WOS.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### transform table with number of sequences in GenBank per country (rows) and years (columns) for accumulation curve
table_accumulate <- function(year_tab, first_column) {
  
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
      expand_limits(x = c(2021.5))
    
  }
  return(WOS_curve_art_acc)
}


### make the text with general informations to add as subtitles to the maps
WOS_make_map_subtitle <- function(taxa_table, taxa_name) {
  text_n_articles <- paste("Total number of articles : ", taxa_table$n_articles[which(taxa_table$taxa == taxa_name)], sep="")
  text_loc_rate <- paste("Study area assignation rate : ", taxa_table$loc_rate[which(taxa_table$taxa == taxa_name)], " %", sep="")
  text_subtitle <- paste(text_n_articles, text_loc_rate, sep=" ; ")
  return(text_subtitle)
}


### store the legend for the pie charts with 3 categories
WOS_store_pie_chart_legend_3 <- function() {
  # make a small tibble containing just the 3 lines from_country / inside_med / outside_med
  tibble_for_pie_legend <- tibble(author_loc = c("from_country", "inside_med", "outside_med"), sum_n_articles = c(457,143,130))
  
  # make a pie chart from the small tibble
  pie_for_legend <- ggplot(tibble_for_pie_legend, aes(x=1, y = sum_n_articles, fill = author_loc)) +
    geom_bar(width = 1, stat = "identity", colour = "black") +
    coord_polar("y", start=0) +
    geom_text(aes(label = sum_n_articles, group = author_loc),
              position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
    scale_fill_manual(values = c("#636363", "#bdbdbd", "#FFFFFF")) + # labels = c("from the country", "mediterranean country", "outside med"), 
    labs(fill='Corresponding author') +
    theme(legend.background=element_blank(), legend.position="right")
  # pie_for_legend
  
  # function to extract legend 
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)} 
  
  # extract legend
  legend_pie_3_categories <- g_legend(pie_for_legend)
  legend_pie_3_categories <- ggpubr::ggarrange(legend_pie_3_categories, ncol = 1, nrow = 1)
  
  return(legend_pie_3_categories)
}


### store the legend for the pie charts with 2 categories
WOS_store_pie_chart_legend_2 <- function() {
  # make a small tibble containing just the 2 lines inside_med / outside_med
  tibble_for_pie_legend <- tibble(author_loc = c("inside_med", "outside_med"), sum_n_articles = c(143,130))
  
  # make a pie chart from the small tibble
  pie_for_legend <- ggplot(tibble_for_pie_legend, aes(x=1, y = sum_n_articles, fill = author_loc)) +
    geom_bar(width = 1, stat = "identity", colour = "black") +
    coord_polar("y", start=0) +
    geom_text(aes(label = sum_n_articles, group = author_loc),
              position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
    scale_fill_manual(values = c("#bdbdbd", "#FFFFFF")) + # labels = c("from the country", "mediterranean country", "outside med"), 
    labs(fill='Corresponding author') +
    theme(legend.background=element_blank(), legend.position="right")
  # pie_for_legend
  
  # function to extract legend 
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)} 
  
  # extract legend
  legend_pie_2_categories <- g_legend(pie_for_legend)
  legend_pie_2_categories <- ggpubr::ggarrange(legend_pie_2_categories, ncol = 1, nrow = 1)
  
  return(legend_pie_2_categories)
}

### make map with country filling by number of articles and pie-charts indicationg corresponding author locality
# this function requires values for the folowing objects not present in the arguments :
# med_clipped
# med_countries_islands
# med_area_bounderies
# med_centroids
# point_grid
# palette
WOS_map_number_articles <- function(locality_table, subtitle_text, taxa_name) {
  
  # add information to plot to med_countries_islands
  # plant_article_loc_tab
  tab_for_join <- locality_table[,c(1,6)]
  
  # join information to MULTIPOLYGON object
  med_countries_islands_joined <- left_join(med_countries_islands, tab_for_join, by = c("country" = "fieldwork_country"))
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid, aes(x = point_grid$X, y = point_grid$Y), color = "grey83", size = 0.2) + # point grid to make the sea filling
    geom_sf(data = med_clipped, mapping = aes(), fill = "white") +                                         # countries background with white filling
    geom_sf(data = med_countries_islands_joined, aes(fill = n_articles)) +                                 # countries and islands layer with the colour filling
    geom_sf(data = med_area_bounderies, mapping = aes(), color = "orangered4", size = 0.6, fill = NA) +    # mediterranean area outline
    scale_fill_gradientn(name="Number of articles", colours=palette, na.value="white")+
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
          legend.box.margin=margin(10,10,10,10)) +
    geom_segment(aes(x = 18.191610, y = 43.183912, xend = 24.27514, yend = 47.11614), size = 0.5) + # segment for Bosnia
    geom_segment(aes(x = 19.044711, y = 42.527407, xend = 24.27514, yend = 43.58586), size = 0.5) + # segment for Montenegro
    geom_segment(aes(x = 36.154312, y = 33.837813, xend = 38.89619, yend = 32.015), size = 0.5) +   # segment for Lebanon
    geom_segment(aes(x = 35.258268, y = 32.170236, xend = 38.89619, yend = 27.615), size = 0.5) +   # segment for Palestine
    geom_segment(aes(x = 15.87, y = 44.05, xend = 18.6, yend = 47.1), size = 0.5)                   # segment for Croatia
  
  ### make pie charts with from_country / outside_med / inside_med information
  # data for the pie charts
  locality_table <- locality_table[,c(1:4)]
  loc_tab_long <- as_tibble(pivot_longer(locality_table, 
                                         cols = colnames(locality_table)[2]:colnames(locality_table)[ncol(locality_table)], 
                                         names_to = "author_loc",
                                         values_to = "n_articles"))
  
  # join the informations to plot to med_centroids
  med_centroids_joined <- left_join(med_centroids, locality_table, by = c("country" = "fieldwork_country"))
  
  # countries' FID list
  centroids_ID_list <- unique(as.integer(med_centroids_joined$FID))
  
  # make a list with the pie-charts objects
  pie_chart_list <- 
    lapply( (centroids_ID_list), function(n){
      
      # title of the future pie chart
      coun_name <- med_centroids_joined[med_centroids_joined$FID == as.character(n),]$country
      
      # make a small tibble containing just the 3 summary lines from_country / inside_med / outside_med
      wos_totals_country <- loc_tab_long[loc_tab_long$fieldwork_country == coun_name,]
      wos_totals_country <- wos_totals_country %>% 
        group_by(author_loc) %>% 
        summarise (sum_n_articles = sum(n_articles))
      
      gt_plot <- ggplotGrob(
        
        # make the pie-charts
        ggplot(wos_totals_country, aes(x=1, y = sum_n_articles, fill = author_loc)) +
          geom_bar(width = 1, stat = "identity", colour = "black") +
          coord_polar("y", start=0) +
          geom_text(aes(label = sum_n_articles, group = author_loc),
                    position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
          scale_fill_manual(values = c("#636363", "#bdbdbd", "#FFFFFF")) +
          theme_minimal()+
          ggtitle(coun_name)+
          theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5, vjust = -0.5,margin=margin(b = 2, unit = "pt")))+
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())+
          theme(plot.margin = unit(c(0,0,0,0), "cm"))
        
      )
    } )
  
  ### insert the pie-charts on the map
  pie_charts_annotation_list <- vector(mode = "list", length = length(pie_chart_list))
  pie_half_width = 1.3
  pie_half_height = 1.3
  
  for (i in 1:length(pie_chart_list)) {
    value <- annotation_custom(grob = pie_chart_list[[i]],
                               xmin = med_centroids$x[[i]] - pie_half_width,
                               xmax = med_centroids$x[[i]] + pie_half_width,
                               ymin = med_centroids$y[[i]] - pie_half_height,
                               ymax = med_centroids$y[[i]] + pie_half_height)
    pie_charts_annotation_list[[i]] <- value
  }
  
  result_plot <- Reduce('+', pie_charts_annotation_list, map)
  
  return(result_plot)
}


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

WOS_map_number_articles_marine <- function(locality_table, subtitle_text, taxa_name) {
  
  # add the information to plot to med_marine_region
  tab_for_join <- locality_table[,c(1,5)]
  
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
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid_marine, aes(x = point_grid_marine$X, y = point_grid_marine$Y), color = "grey83", size = 0.2) +  # point grid to make the sea
    geom_sf(data = med_clipped_marine, mapping = aes(), fill = "white") +  # countries background with white filling
    geom_sf(data = med_marine_region_joined_under_layer, aes(fill = n_articles)) +  # layer with NW med and Tunisian Plateau to avoid overlap
    geom_sf(data = med_marine_region_joined, aes(fill = n_articles)) +  # layer with the other marine regions
    geom_segment(aes(x = 5.165294, y = 36.667825, xend = -0.26, yend = 32.33), size = 0.7, colour = "green2") + # segment for western med
    geom_segment(aes(x = 30.824110, y = 36.848481, xend = 36.422313, yend = 42), size = 0.7, colour = "royalblue1") + # segment for eastern med
    geom_point(data = circle_centroid_joined, aes(x = x, y = y, fill = n_articles), shape=21, size = 70, stroke = 1, col = c("green2", "royalblue1")) + # circles with the filling for the 2 big regions
    scale_fill_gradientn(name="Number of articles", colours=palette, na.value="white") +
    geom_sf(data = western_med, fill = NA, colour = "green2", size = 0.5, inherit.aes = FALSE) +  # western med bounder
    geom_sf(data = eastern_med, fill = NA, colour = "royalblue1", size = 0.5, inherit.aes = FALSE) +  # eastern med bounder
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

  ### make pie charts with from_country / outside_med / inside_med information
  # data for the pie charts
  locality_table <- locality_table[,c(1:3)]
  loc_tab_long <- as_tibble(pivot_longer(locality_table, 
                                         cols = colnames(locality_table)[2]:colnames(locality_table)[ncol(locality_table)], 
                                         names_to = "author_loc",
                                         values_to = "n_articles"))
  
  # join the informations to plot to marine_region_centroids
  marine_region_centroids_joined <- left_join(marine_region_centroids, locality_table, by = c("marine_reg" = "marine_region"))
  
  # countries' FID list
  centroids_ID_list <- unique(as.integer(marine_region_centroids_joined$FID))
  
  # make a list with the pie-charts objects
  pie_chart_list <- 
    lapply( (centroids_ID_list), function(n){
      
      # title of the future pie chart
      coun_name <- marine_region_centroids_joined[marine_region_centroids_joined$FID == as.character(n),]$marine_reg
      
      # make a small tibble containing just the 2 summary lines inside_med / outside_med
      wos_totals_marine_region <- loc_tab_long[loc_tab_long$marine_region == coun_name,]
      wos_totals_marine_region <- wos_totals_marine_region %>% 
                                         group_by(author_loc) %>% 
                                         summarise (sum_n_articles = sum(n_articles))
      
      gt_plot <- ggplotGrob(
        
        # make the pie-charts
        ggplot(wos_totals_marine_region, aes(x=1, y = sum_n_articles, fill = author_loc)) +
          geom_bar(width = 1, stat = "identity", colour = "black") +
          coord_polar("y", start=0) +
          geom_text(aes(label = sum_n_articles, group = author_loc),
                    position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
          scale_fill_manual(values = c("#bdbdbd", "#FFFFFF")) +
          theme_minimal()+
          ggtitle(coun_name)+
          theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5, vjust = -0.5,margin=margin(b = 2, unit = "pt")))+
          theme(axis.line=element_blank(),
                axis.text.x=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                legend.position="none",
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank())+
          theme(plot.margin = unit(c(0,0,0,0), "cm"))
        
      )
    } )
  
  ### insert the pie-charts on the map
  pie_charts_annotation_list <- vector(mode = "list", length = length(pie_chart_list))
  pie_half_width = 1.3
  pie_half_height = 1.3
  
  for (i in 1:length(pie_chart_list)) {
    value <- annotation_custom(grob = pie_chart_list[[i]],
                               xmin = marine_region_centroids$x[[i]] - pie_half_width,
                               xmax = marine_region_centroids$x[[i]] + pie_half_width,
                               ymin = marine_region_centroids$y[[i]] - pie_half_height,
                               ymax = marine_region_centroids$y[[i]] + pie_half_height)
    pie_charts_annotation_list[[i]] <- value
  }
  
  result_plot <- Reduce('+', pie_charts_annotation_list, map)
  
  return(result_plot)
}

