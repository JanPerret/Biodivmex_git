#################################################
#
# Biodivmex project
#
# 00_functions_GenBank.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### extract general informations for a diven taxa
GB_extract_general_info <- function(kingdom_data, taxa_data, taxa_data_med, taxa_name) {
  
  # initiaze table to store the 6 general  descriptive informations
  descritive_table <- setNames(data.frame(matrix(ncol = 7, nrow = 1)),
                               c("taxa", "n_seq", "taxa_n_seq", "loc_rate", "n_seq_med", "sp_level_rate_med", "n_sp_med"))
  descritive_table$taxa <- taxa_name
  
  # 1. number of sequences in GenBank for the reign (metazoa, thracheophyta or fungi)
  descritive_table$n_seq <- length(kingdom_data$access_num)
  
  # 2. number of sequences for the chosen taxonomic group (identical for fungi and plants)
  descritive_table$taxa_n_seq <- length(taxa_data$access_num)
  
  # 3. percentage of sequences with a sample origin information
  descritive_table$loc_rate <- round((sum(!is.na(taxa_data$sample_origin))/length(taxa_data$access_num))*100, 2)
  
  # 4. number of sequences affiliated to a country of the mediterranean basin for the taxa
  descritive_table$n_seq_med <- length(taxa_data_med$access_num)
  
  # 5. percentage of sequences affiliated at least to species level (can be more precise) for the taxa in the mediterranean basin
  a = sum(!is.na(taxa_data_med$species_level)) - length(which(taxa_data_med$species_level == "hybrid"))
  descritive_table$sp_level_rate_med <- round((a/length(taxa_data_med$species_level))*100, 2)
  
  # 6. number of different species names for the taxa in the mediterranean basin
  descritive_table$n_sp_med <- length(levels(as.factor(taxa_data_med$species_level))) # here levels() if better than unique() because it don't count NA as a value
  
  if ("hybrid" %in% levels(as.factor(taxa_data_med$species_level))) {
    descritive_table$n_sp_med <- descritive_table$n_sp_med - 1 # minus one if the "hybrid" level is present because it is not a species name
  }
  
  return(descritive_table)
}


### extract the same information than GB_extract_general_info() but for each year
GB_loop_over_years <- function(kingdom_data, taxa_data, taxa_data_med, taxa_name) {
  
  # initiaze table to store the 6 general descriptive informations
  general_table <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                            c("year", "taxa", "n_seq", "taxa_n_seq", "loc_rate", "n_seq_med", "sp_level_rate_med", "n_sp_med"))
  
  # list years since the first sequence was deposited in GenBank (for our taxa + genes conditions)
  vect_year <- as.integer(c(1987:2019)) # first sequences is from 1987 in our data
  
  # loop over years
  for (year in vect_year) {
    given_year <- as.integer(year) # because else it doesn't work with filter()
    
    # subset tables for the given year
    kingdom_data_year <- kingdom_data %>% filter(year == given_year)
    taxa_data_year <- taxa_data %>% filter(year == given_year)
    taxa_data_med_year <- taxa_data_med %>% filter(year == given_year)
    
    # extract information of the year tables with GB_extract_general_info() function
    tab_given_year <- GB_extract_general_info(kingdom_data = kingdom_data_year, taxa_data = taxa_data_year, taxa_data_med = taxa_data_med_year, taxa_name = taxa_name)
    
    # binding extracted informations to the output table
    tab_given_year <- cbind(year = given_year, tab_given_year)
    general_table <- rbind(general_table, tab_given_year)
  }
  
  return(general_table)
}


### extract number of sequences containing each gene for a given taxa
GB_extract_gene <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  gene_table <- setNames(data.frame(matrix(ncol = 27, nrow = 1)), c("taxa", paste0(gene_list), "n_NA", "tot_n_seq"))
  
  # loop through gene names and fill gene_table
  gene = ""
  for (gene in gene_list) {
    gene_table$taxa <- taxa_name
    
    # count number of sequences containing the given gene
    gene_table[gene] <- sum(grepl(gene, taxa_data_med$gene)) # grepl is like str_detect but it doesn't return NA when used on NA
    
    # number of sequences without a gene assignation
    gene_table$n_NA <- sum(is.na(taxa_data_med$gene))
    
    # total number of sequences
    gene_table$tot_n_seq <- length(taxa_data_med$access_num)
  }
  
  return(gene_table)
}


### extract the same information than GB_extract_gene() but for each year
GB_gene_recap_loop_over_years <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  gene_table <- setNames(data.frame(matrix(ncol = 28, nrow = 0)), c("taxa", "year", paste0(gene_list), "n_NA", "tot_n_seq"))
  
  # list years since the first sequence was deposited in GenBank (for our taxa + genes conditions)
  vect_year <- as.integer(c(1987:2019)) # first sequences is from 1987 in our dataframe
  
  # loop over years
  for (year in vect_year) {
    given_year <- as.integer(year) # because otherway it doesn't work with filter()
    
    # subset tables for the given year
    taxa_data_med_year <- taxa_data_med %>% filter(year == given_year)
    
    # extract information of the year tables with GB_extract_general_info() function
    tab_given_year <- GB_extract_gene(taxa_data_med = taxa_data_med_year, taxa_name = taxa_name)
    
    # binding extracted informations to the output table
    tab_given_year <- cbind(year = given_year, tab_given_year)
    gene_table <- rbind(gene_table, tab_given_year)
  }
  
  return(gene_table)
}


### add column with local / mediterranean / distant sequencer information
GB_generate_sequencer_loc <- function(kingdom_data_med) {
  
  # add empty column "sequencer_loc"
  kingdom_data_med["sequencer_loc"] <- NA
  
  # loop to fill the column
  for (i in 1:length(kingdom_data_med$access_num)) {
    if(is.na(kingdom_data_med$sequencer_nationality[i])) {} # no action if sequencer_nationality == NA
    else if(kingdom_data_med$sequencer_nationality[i] == kingdom_data_med$sample_origin[i]) {kingdom_data_med$sequencer_loc[i] <- "from_country"} # from the country
    else if(kingdom_data_med$sample_origin[i] == "Corsica" & kingdom_data_med$sequencer_nationality[i] == "France") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Balearic Islands" & kingdom_data_med$sequencer_nationality[i] == "Spain") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Sardinia" & kingdom_data_med$sequencer_nationality[i] == "Italy") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Sicily" & kingdom_data_med$sequencer_nationality[i] == "Italy") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Crete" & kingdom_data_med$sequencer_nationality[i] == "Greece") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sequencer_nationality[i] %in% med_countries_list) {kingdom_data_med$sequencer_loc[i] <- "inside_med"} # from the mediterranean basin
    else if(!is.na(kingdom_data_med$sequencer_nationality[i])) {kingdom_data_med$sequencer_loc[i] <- "outside_med"} # outside mediterranean basin
  }
  
  kingdom_data_med$sequencer_loc <- factor(kingdom_data_med$sequencer_loc, levels = c("from_country", "outside_med", "inside_med"), ordered = FALSE) # fix factor levels
  kingdom_data_med$sequencer_loc <- fct_explicit_na(kingdom_data_med$sequencer_loc) # give missing values an explicit factor level to ensure that they appear in summaries and plots
  return(kingdom_data_med)
}


### make recap tables with rate of sequences assigned at species level and number of different species (or species names)
GB_recap_species_level <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  sp_level_table <- setNames(data.frame(matrix(ncol = 5, nrow = 27)),
                             c("country", "taxa", "sp_level_rate", "n_seq", "n_sp"))
  sp_level_table$country <- sample_origin_med_levels
  sp_level_table$taxa <- taxa_name
  
  # loop to fill the table
  for (i in 1:length(sp_level_table$country)) {
    country = sp_level_table$country[i]
    inter_tab <- subset(taxa_data_med, taxa_data_med$sample_origin == country)
    
    # country number of sequences
    sp_level_table$n_seq[i] <- length(inter_tab$species_level)
    
    # species level rate
    a = sum(!is.na(inter_tab$species_level)) - length(which(inter_tab$species_level == "hybrid")) # total number of sequences without hybrids
    sp_level_table$sp_level_rate[i] <- round((a/length(inter_tab$species_level))*100, 2) # in percentage
    
    # number of species
    sp_level_table$n_sp[i] <- length(levels(as.factor(inter_tab$species_level))) # here levels() if better than unique() because it don't count NA as a value
    
  }
  
  return(sp_level_table)
}


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


### make map subtitle text
GenBank_make_map_subtitle <- function(taxa_table, kingdom_name, taxa_name) {
  
  text_reign_n_seq <- paste("Total number of sequences in GenBank for ", kingdom_name, " : ", taxa_table$n_seq[which(taxa_table$taxa == taxa_name)], sep="")
  text_taxa_n_seq <- paste("Number of sequences for the taxa : ", taxa_table$taxa_n_seq[which(taxa_table$taxa == taxa_name)], sep="")
  text_loc_rate <- paste("Sample origin fill rate : ", taxa_table$loc_rate[which(taxa_table$taxa == taxa_name)], " %", sep="")
  text_taxa_med_n_seq <- paste("Number of sequences for the taxa : ", taxa_table$n_seq_med[which(taxa_table$taxa == taxa_name)], sep="")
  text_n_species <- paste("Number of species with at least one sequence : ", taxa_table$n_sp_med[which(taxa_table$taxa == taxa_name)], sep="")
  
  text_subtitle <- paste("At world scale : ", text_reign_n_seq, " ; ", text_taxa_n_seq, " ; ", text_loc_rate, "\n",
                         "In the mediterranean basin : ", text_taxa_med_n_seq, " ; ", text_n_species, sep="")
  return(text_subtitle)
}


### store the legend for the pie charts with 3 categories
GenBank_store_pie_chart_legend <- function() {
  # make a small tibble containing just the 3 lines from_country / inside_med / outside_med
  tibble_for_pie_legend <- tibble(author_loc = c("from_country", "inside_med", "outside_med"), sum_n_articles = c(457,143,130))
  
  # make a pie chart from the small tibble
  pie_for_legend <- ggplot(tibble_for_pie_legend, aes(x=1, y = sum_n_articles, fill = author_loc)) +
    geom_bar(width = 1, stat = "identity", colour = "black") +
    coord_polar("y", start=0) +
    geom_text(aes(label = sum_n_articles, group = author_loc),
              position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
    scale_fill_manual(values = c("#636363", "#bdbdbd", "#FFFFFF")) + # labels = c("from the country", "mediterranean country", "outside med"), 
    labs(fill='Sequencer identity') +
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



### make map with country filling by number of sequences and pie-charts indicating sequencer locality
# this function requires values for the folowing objects not present in the arguments :
# med_clipped
# med_countries_islands
# med_centroids
# point_grid
# palette
GenBank_map_number_sequence <- function(locality_table, subtitle_text, taxa_name) {
  
  # add information to plot to med_countries_islands
  # plant_article_loc_tab
  tab_for_join <- locality_table[,c(1,6)]
  
  # join information to MULTIPOLYGON object
  med_countries_islands_joined <- left_join(med_countries_islands, tab_for_join, by = c("country" = "sample_origin"))
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid, aes(x = point_grid$X, y = point_grid$Y), color = "grey83", size = 0.2) + # point grid to make the sea filling
    geom_sf(data = med_clipped, mapping = aes(), fill = "white") +                                         # countries background with white filling
    geom_sf(data = med_countries_islands_joined, aes(fill = n_seq)) +                                      # countries and islands layer with the colour filling
    scale_fill_gradientn(name="Number of sequences", colours=palette, na.value="white")+
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(paste("Number of sequences in GenBank for", taxa_name), subtitle = subtitle_text) +
    theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
    labs(x=NULL, y=NULL) +
    theme(plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="right",
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(10,10,10,10)) +
    geom_segment(aes(x = 19.342929, y = 44.248783, xend = 24.27514, yend = 47.11614), size = 0.5) + # segment for Bosnia
    geom_segment(aes(x = 19.712831, y = 43.170238, xend = 24.27514, yend = 43.58586), size = 0.5) + # segment for Montenegro
    geom_segment(aes(x = 36.154312, y = 33.837813, xend = 38.89619, yend = 32.015), size = 0.5) +   # segment for Lebanon
    geom_segment(aes(x = 35.541040, y = 31.967576, xend = 38.89619, yend = 27.615), size = 0.5) +   # segment for Palestine
    geom_segment(aes(x = 17.256789, y = 46.039755, xend = 18.6, yend = 47.1), size = 0.5)           # segment for Croatia
  
  ### make pie charts with from_country / outside_med / inside_med information
  # data for the pie charts
  locality_table <- locality_table[,c(1:4)]
  loc_tab_long <- as_tibble(pivot_longer(locality_table, 
                                         cols = colnames(locality_table)[2]:colnames(locality_table)[ncol(locality_table)], 
                                         names_to = "author_loc",
                                         values_to = "n_seq"))
  
  # join the informations to plot to med_centroids
  med_centroids_joined <- left_join(med_centroids, locality_table, by = c("country" = "sample_origin"))
  
  # countries' FID list
  centroids_ID_list <- unique(as.integer(med_centroids_joined$FID))
  
  # make a list with the pie-charts objects
  pie_chart_list <- 
    lapply( (centroids_ID_list), function(n){
      
      # title of the future pie chart
      coun_name <- med_centroids_joined[med_centroids_joined$FID == as.character(n),]$country
      
      # make a small tibble containing just the 3 summary lines from_country / inside_med / outside_med
      wos_totals_country <- loc_tab_long[loc_tab_long$sample_origin == coun_name,]
      wos_totals_country <- wos_totals_country %>% 
        group_by(author_loc) %>% 
        summarise (sum_n_articles = sum(n_seq))
      
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


### make map with country filling by number of species
# this function requires values for the folowing objects not present in the arguments :
# med_clipped
# med_countries_islands
# point_grid
# palette
GenBank_map_number_species <- function(species_level_tab, subtitle_text, taxa_name) {
  
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

####################
### FOR MARINE TAXA
####################

### make map with EEZ filling by number of sequences and pie-charts indicating sequencer locality
# this function requires values for the folowing objects not present in the arguments :
# med_clipped_marine
# med_EEZ
# EEZ_centroids
# point_grid_marine
# palette
GenBank_map_number_sequence_marine_taxa <- function(locality_table, subtitle_text, taxa_name) {
  
  # add information to plot to med_EEZ
  # plant_article_loc_tab
  tab_for_join <- locality_table[,c(1,6)]
  
  # join information to MULTIPOLYGON object
  med_EEZ_joined <- left_join(med_EEZ, tab_for_join, by = c("country" = "sample_origin"))
  
  # plot the map
  map <- ggplot(NULL) + 
    geom_point(data = point_grid_marine, aes(x = point_grid_marine$X, y = point_grid_marine$Y), color = "grey83", size = 0.2) + # point grid to make the sea
    geom_sf(data = med_EEZ_joined, aes(fill = n_seq)) + # EEZ layer with the colour filling
    geom_sf(data = med_clipped_marine, mapping = aes(), fill = "white") + # countries background with white filling
    scale_fill_gradientn(name="Number of sequences", colours=palette, na.value="white")+
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(paste("Number of sequences in GenBank for", taxa_name), subtitle = subtitle_text) +
    theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
    labs(x=NULL, y=NULL) +
    theme(plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="right",
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(10,10,10,10))


  ### make pie charts with from_country / outside_med / inside_med information
  # data for the pie charts
  locality_table <- locality_table[,c(1:4)]
  loc_tab_long <- as_tibble(pivot_longer(locality_table, 
                                         cols = colnames(locality_table)[2]:colnames(locality_table)[ncol(locality_table)], 
                                         names_to = "author_loc",
                                         values_to = "n_seq"))
  
  # join the informations to plot to EEZ_centroids
  EEZ_centroids_joined <- left_join(EEZ_centroids, locality_table, by = c("country" = "sample_origin"))
  
  # countries' FID list
  centroids_ID_list <- unique(as.integer(EEZ_centroids_joined$FID))
  
  # make a list with the pie-charts objects
  pie_chart_list <- 
    lapply( (centroids_ID_list), function(n){
      
      # title of the future pie chart
      coun_name <- EEZ_centroids_joined[EEZ_centroids_joined$FID == as.character(n),]$country
      
      # make a small tibble containing just the 3 summary lines from_country / inside_med / outside_med
      wos_totals_country <- loc_tab_long[loc_tab_long$sample_origin == coun_name,]
      wos_totals_country <- wos_totals_country %>% 
        group_by(author_loc) %>% 
        summarise (sum_n_articles = sum(n_seq))
      
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
                               xmin = EEZ_centroids$x[[i]] - pie_half_width,
                               xmax = EEZ_centroids$x[[i]] + pie_half_width,
                               ymin = EEZ_centroids$y[[i]] - pie_half_height,
                               ymax = EEZ_centroids$y[[i]] + pie_half_height)
    pie_charts_annotation_list[[i]] <- value
  }
  
  result_plot <- Reduce('+', pie_charts_annotation_list, map)
  
  return(result_plot)
}


### make map with EEZ filling by number of species
# this function requires values for the folowing objects not present in the arguments :
# med_clipped_marine
# med_EEZ
# point_grid_marine
# palette

GenBank_map_number_species_marine_taxa <- function(species_level_tab, subtitle_text, taxa_name) {
  
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

