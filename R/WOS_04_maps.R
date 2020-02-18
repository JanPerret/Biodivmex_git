#################################################
#
# Biodivmex project
#
# WOS_04_maps.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### map of number of articles per country for terrestrial taxa

## pour les informations generales :
taxa_table

## pour les informations geographiques :
plant_article_loc_tab
fungi_article_loc_tab
amph_article_loc_tab
rept_article_loc_tab
bird_article_loc_tab
mammal_article_loc_tab
coleo_article_loc_tab
papilio_article_loc_tab
lumbri_article_loc_tab
tree_article_loc_tab

# fish_article_loc_tab
# sponge_article_loc_tab
# crusta_article_loc_tab

localisation_table = plant_article_loc_tab
taxa_name = "plant"

## texte pour le sous-titre des cartes :
taxa_name = "plant"
text_n_articles <- paste("Number of articles : ", taxa_table$n_articles[which(taxa_table$taxa == taxa_name)], sep="")
text_loc_rate <- paste("Fieldwork assignation rate : ", taxa_table$loc_rate[which(taxa_table$taxa == taxa_name)], " %", sep="")
text_subtitle <- paste(text_n_articles, text_loc_rate, sep=" ; ")


### l. 405 du fichier .\Biodivmex\Focus_experts\ScriptWOS_maps_all_taxa_v2_without_surface_ratio.R
### ON A LE DATAFRAME AVEC TOUTES LES DONNEES, ON PASSE AU CARTES

library(rworldxtra)
library(rgdal)
library(rgeos)
library(broom)
library(raster)


data(countriesHigh)
world <- countriesHigh

# on ajoute une colonne avec un ID unique par pays
{{world@data$id <- row.names(world@data)}}

# creation d'un data_frame avec les noms et identifiants des pays
country_id <- world@data$id
country_name <- world@data$GEOUNIT
country_df <- data.frame(country_id, country_name)
# View(country_df)

# to make a bounding box polygon

# coordonnees du cadre de la carte
y_coord <- c(49.651575,  49.651575,  24.550494, 24.550494)
x_coord <- c(-15.8519, 43.261471, 43.261471, -15.8519)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
med_box = SpatialPolygons(list(ps), proj4string = CRS("+proj=longlat +ellps=WGS84"))
med_box <- spTransform(med_box, CRS(proj4string(countriesHigh)))

world <- spTransform(world, CRS(proj4string(countriesHigh)))
# summary(world)

# Get polygons that are only in the bounding box
med_clipped <- {{  rgeos::gIntersection(world, med_box, byid = TRUE, id=world$id)}}

tidy_med_clipped <- broom::tidy(med_clipped)

## Import med area shapefile
med_area <- raster::shapefile("./data/med_area_melted_final/med_area_melted_final.shp")

proj4string(med_area) <- CRS(proj4string(countriesHigh))

# transform the shapefile to a dataframe to be plottable with ggplot2
tidy_med_area <- broom::tidy(med_area)

# clip the polygons of med_clipped with the med_area bounderies
med_clipped_quezel_medail <- {{  rgeos::gIntersection(med_clipped, med_area, byid = TRUE)}}

# transform to dataframe for ggplot2
tidy_med_clipped_quezel_medail <- tidy(med_clipped_quezel_medail)
# sort(unique(tidy_med_clipped_quezel_medail$id)) # -> je sais pas pourquoi il y a " 0" apres chaque numero d'ID
# on supprime donc ces caracteres en trop :
tidy_med_clipped_quezel_medail$id <- substr(tidy_med_clipped_quezel_medail$id,1,nchar(tidy_med_clipped_quezel_medail$id)-2)

# on donne a Gaza et West Bank l'identifiant de la palestine et celui de Chypre a Chypre du nord
tidy_med_clipped_quezel_medail$id[tidy_med_clipped_quezel_medail$id == "81" | tidy_med_clipped_quezel_medail$id == "246"] <- "254"
tidy_med_clipped_quezel_medail$id[tidy_med_clipped_quezel_medail$id == "46" | tidy_med_clipped_quezel_medail$id == "58"] <- "59"
# 46 c'est l'ID de "Cyprus No Mans Area" et 58 celui de "Northern Cyprus"

### Extraction des centroides de chaque pays et modification des valeurs en prevision de la mise en page de la carte
med_centroids <- gCentroid(med_clipped, byid = TRUE)
med_centroids <- as.data.frame(med_centroids)
med_centroids$COUNTRY_IDS <- row.names(med_centroids)
med_centroids <- inner_join(med_centroids, country_df, by = c("COUNTRY_IDS" = "country_id"))

med_centroids <- subset(med_centroids, med_centroids$country_name == "Albania" | med_centroids$country_name == "Algeria"
                        | med_centroids$country_name == "Bosnia and Herzegovina" | med_centroids$country_name == "Croatia"
                        | med_centroids$country_name == "Cyprus" | med_centroids$country_name == "Egypt"
                        | med_centroids$country_name == "France"
                        | med_centroids$country_name == "Greece" | med_centroids$country_name == "Israel"
                        | med_centroids$country_name == "Italy" | med_centroids$country_name == "Lebanon"
                        | med_centroids$country_name == "Libya" | med_centroids$country_name == "Malta"
                        | med_centroids$country_name == "Montenegro" | med_centroids$country_name == "Morocco"
                        | med_centroids$country_name == "Palestine" | med_centroids$country_name == "Portugal"
                        | med_centroids$country_name == "Slovenia" | med_centroids$country_name == "Spain"
                        | med_centroids$country_name == "Syria" | med_centroids$country_name == "Tunisia"
                        | med_centroids$country_name == "Turkey")

# on change les coordonnes des centroides pour que les diagrammes ne se chevauchent pas
med_centroids$country_name <- as.character(med_centroids$country_name)
med_centroids <- rbind(med_centroids, "48" = c("35.363307", "31.988903", "254", "Palestine"))
med_centroids[med_centroids$country_name == "Israel",]$x <- "33.200000"
med_centroids[med_centroids$country_name == "Israel",]$y <- "32.700000"
med_centroids[med_centroids$country_name == "Malta",]$x <- "16.000000"
med_centroids[med_centroids$country_name == "Malta",]$y <- "34.900000"
med_centroids[med_centroids$country_name == "Italy",]$x <- "14.014282"
med_centroids[med_centroids$country_name == "Italy",]$y <- "41.722337"
med_centroids[med_centroids$country_name == "Portugal",]$x <- "-8.97781503"
med_centroids[med_centroids$country_name == "Morocco",]$y <- "31.6287122"
med_centroids[med_centroids$country_name == "Morocco",]$x <- "-7.66966472"
med_centroids[med_centroids$country_name == "Greece",]$y <- "37.55121967"
med_centroids[med_centroids$country_name == "Greece",]$x <- "23.97514"
med_centroids[med_centroids$country_name == "Slovenia",]$x <- "13.80813927"
med_centroids[med_centroids$country_name == "Slovenia",]$y <- "47.21614"
med_centroids[med_centroids$country_name == "Albania",]$x <- "19.75399"
med_centroids[med_centroids$country_name == "Albania",]$y <- "40.14258"
med_centroids[med_centroids$country_name == "Croatia",]$x <- "19.620315"
med_centroids[med_centroids$country_name == "Croatia",]$y <- "47.858435"
med_centroids[med_centroids$country_name == "Montenegro",]$x <- "25.47514"
med_centroids[med_centroids$country_name == "Montenegro",]$y <- "43.58586"
med_centroids[med_centroids$country_name == "Bosnia and Herzegovina",]$x <- "25.47514"
med_centroids[med_centroids$country_name == "Bosnia and Herzegovina",]$y <- "47.51614"
med_centroids[med_centroids$country_name == "Cyprus",]$x <- "30.8"
med_centroids[med_centroids$country_name == "Cyprus",]$y <- "35.115"
med_centroids[med_centroids$country_name == "Syria",]$x <- "39.09619"
med_centroids[med_centroids$country_name == "Syria",]$y <- "35.33435"
med_centroids[med_centroids$country_name == "Lebanon",]$x <- "40.09619"
med_centroids[med_centroids$country_name == "Lebanon",]$y <- "31.515"
med_centroids[med_centroids$country_name == "Palestine",]$x <- "40.09619"
med_centroids[med_centroids$country_name == "Palestine",]$y <- "27.115"
med_centroids[med_centroids$country_name == "Egypt",]$y <- "26.91488"

# on les repasse en numerique pour que les coordonnees puissent etre utilisees
med_centroids$x <- as.numeric(med_centroids$x)
med_centroids$y <- as.numeric(med_centroids$y)

# chargement des donnees de surface des pays
# country_surfaces <- read.csv2("D:/Fac/Vacation CEFE/Focus_experts/Data/raw/country_surfaces.csv")




# on charge les listes pour la boucle
list_data = list(plant, fungi, amphibian, reptile, bird, mammal, coleoptera, lumbricina, papilionoidea, trees)
list_n_articles = c(plant_n_articles, fungi_n_articles, amph_n_articles, rept_n_articles, bird_n_articles,
                    mammal_n_articles, coleo_n_articles, lumbri_n_articles, papilio_n_articles, trees_n_articles)
list_loc_rate = c(plant_loc_rate, fungi_loc_rate, amph_loc_rate, rept_loc_rate, bird_loc_rate,
                  mammal_loc_rate, coleo_loc_rate, lumbri_loc_rate, papilio_loc_rate, trees_loc_rate)
list_taxa_name = c("plant", "fungi", "amphibian", "reptile", "bird", "mammal", "coleoptera", "lumbricina", "papilionoidea", "trees")


# on initalise les objets sur lesquels on va utiliser une fonction *join...* car sinon ca fait bugger le script 
# comme a chaque iteration de la boucle on ajoute des colonnes qui portent le meme nom !
med_centroids_true <- med_centroids
tidy_med_clipped_quezel_medail_true <- tidy_med_clipped_quezel_medail




###################################################
### boucle pour faire les cartes
# len = 10
# seq_along(1:10)

i = 2
for (i in 1:10) {
  
  taxa_data = list_data[[i]]
  taxa_name = list_taxa_name[i]
  n_articles = list_n_articles[i]
  loc_rate = list_loc_rate[i]
  
  
  data_art_per_country <- taxa_data[,c(2,3)]
  colnames(data_art_per_country)[2] <- "tot_art"
  data_art_per_country <- left_join(data_art_per_country, country_surfaces, by = "country") ######### country_surfaces
  art_density <- data_art_per_country$tot_art/data_art_per_country$surface_zone_med
  data_art_per_country <- cbind(data_art_per_country, art_density)
  
  med_centroids <- med_centroids_true
  med_centroids <- left_join(med_centroids, data_art_per_country, by = c("country_name" = "country"))
  # on a le dataframe avec toutes les donnees pour remplir les pays avec une echelle de gris
  # View(med_centroids)
  # med_centroids$art_density[15] <- NA
  
  
  tidy_med_clipped_quezel_medail <- tidy_med_clipped_quezel_medail_true
  tidy_med_clipped_quezel_medail <- left_join(tidy_med_clipped_quezel_medail, med_centroids, by = c("id" = "COUNTRY_IDS"))
  
  ### FOND DE CARTE
  # coordonnees des pointilles qui vont faire la mer sur la carte
  point_grid <- expand.grid(seq(from = -15.8519, to = 43.261471, by = 0.55),seq(from = 24.7, to = 49.66, by = 0.5))
  colnames(point_grid) <- c("X","Y")
  
  # palette couleurs pour remplissage
  # palette <- c("#87CEFF", "#CAE1FF", "#FFF68F", "#FFF68F", "#FFC125", "#FFC125", "#FF8C00", "#FF8C00", "#FF7F00", "#FF7F00", "#EE7600", "#EE7600", "#FF4500", "#FF4500", "#EE4000", "#EE4000", "#CD3700", "#CD3700", "#CD0000", "#CD0000")
  palette <- brewer.pal(9, "YlOrRd")
  
  map <- ggplot(NULL)+
    geom_point(data = point_grid, aes(x = point_grid$X, y = point_grid$Y), color = "grey83", size = 0.2)+
    geom_polygon(data=tidy_med_clipped, aes(long, lat, group=group), fill = "white")+
    geom_polygon(data=tidy_med_clipped_quezel_medail, aes(long, lat, group=group, fill = tot_art))+
    geom_path(data=med_clipped, color="grey48", mapping=aes(long, lat, group=group), size=0.2)+
    geom_path(data=tidy_med_area, color="orangered4", mapping=aes(long, lat, group=group), size=0.6)+
    scale_fill_gradientn(name="Number of articles", colours=palette, na.value="white")+
    theme_minimal() +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    ggtitle(paste("Number of articles in Web of Science about", taxa_name), subtitle = paste(n_articles, loc_rate, sep=" ; ")) +
    theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
    labs(x=NULL, y=NULL) +
    theme(plot.margin=unit(c(0,0,0,0),"mm")) +
    theme(legend.position="right",
          legend.justification="left",
          legend.margin=margin(0,0,0,0),
          legend.box.margin=margin(10,10,10,10))+
    coord_quickmap() +
    geom_segment(aes(x = 18.191610, y = 43.183912, xend = 24.27514, yend = 47.11614), size = 0.5) + # segment for Bosnia
    geom_segment(aes(x = 19.044711, y = 42.527407, xend = 24.27514, yend = 43.58586), size = 0.5) + # segment for Montenegro
    geom_segment(aes(x = 36.154312, y = 33.837813, xend = 38.89619, yend = 32.015), size = 0.5) + # segment for Lebanon
    geom_segment(aes(x = 35.258268, y = 32.170236, xend = 38.89619, yend = 27.615), size = 0.5) + # segment for Palestine
    geom_segment(aes(x = 15.87, y = 44.05, xend = 18.6, yend = 47.1), size = 0.5) # segment for Croatia
  # map
  
  
  ## Pour inserer les camemberts local/distant
  # passer en format long pour pouvoir mettre 2 couleurs en fonction des auteurs local/distant
  data_long <- gather(taxa_data[,-3], key = "local_dist_article", value = "n_articles", c(local_articles,distant_articles), na.rm = FALSE)
  
  med_centroids_wos <- full_join(med_centroids, wos_totals, by = c("country_name" = "country"))
  med_centroids_wos_taxa <- subset(med_centroids_wos, med_centroids_wos$taxa == taxa_name)
  
  
  #####  FINAL MAP
  # Pie-charts
  centroids_ID_list <- sort(unique(as.integer(med_centroids_wos_taxa$COUNTRY_IDS)))
  pie_chart_list <- 
    lapply( (centroids_ID_list), function(n){
      name <- med_centroids[med_centroids$COUNTRY_IDS == as.character(n),]$country_name
      wos_totals_country <- data_long[data_long$country == name,]
      wos_totals_country <- wos_totals_country %>% 
        group_by(local_dist_article) %>% 
        summarise (sum_n_articles = sum(n_articles))
      gt_plot <- ggplotGrob(
        ggplot(wos_totals_country, aes(x=1, y = sum_n_articles, fill = local_dist_article)) +
          geom_bar(width = 1, stat = "identity", colour = "black") +
          coord_polar("y", start=0) +
          geom_text(aes(label = wos_totals_country$sum_n_articles, group = wos_totals_country$local_dist_article),
                    position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
          scale_fill_manual(values = c("#7A7A7A", "#FFFFFF")) +
          # theme_nothing() +
          theme_minimal()+
          ggtitle(name)+
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
          theme(plot.margin = unit(c(0,0,0,0), "cm")))
    } )
  
  # Pour inserer les camemberts sur la carte
  pie_charts_annotation_list <- vector(mode = "list", length = length(pie_chart_list))
  pie_half_width = 1.3
  pie_half_height = 1.3
  
  
  for (num in 1:length(pie_chart_list)) {
    value <- annotation_custom(grob = pie_chart_list[[num]],
                               xmin = med_centroids$x[[num]] - pie_half_width,
                               xmax = med_centroids$x[[num]] + pie_half_width,
                               ymin = med_centroids$y[[num]] - pie_half_height,
                               ymax = med_centroids$y[[num]] + pie_half_height)
    pie_charts_annotation_list[[num]] <- value
  }
  
  result_plot <- Reduce('+', pie_charts_annotation_list, map)
  # result_plot
  
  # Storing pie legend 
  med_centroids_france <- subset(med_centroids_wos, med_centroids_wos$country_name=="France")
  name <- as.list(med_centroids_france$country_name)[1]
  wos_totals_country <- data_long[data_long$country == name,]
  wos_totals_country <- wos_totals_country %>% 
    group_by(local_dist_article) %>% 
    summarise (sum_n_articles = sum(n_articles))
  pie_for_legend <- ggplot(wos_totals_country, aes(x=1, y = sum_n_articles, fill = local_dist_article)) +
    geom_bar(width = 1, stat = "identity", colour = "black") +
    coord_polar("y", start=0) +
    geom_text(aes(label = wos_totals_country$sum_n_articles, group = wos_totals_country$local_dist_article),
              position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
    scale_fill_manual(labels = c("abroad", "within the country"),values = c("#7A7A7A", "#FFFFFF")) +
    labs(fill='Laboratory of the\ncorresponding author') +
    theme(legend.background=element_blank(), legend.position="right")
  # pie_for_legend
  
  # extract Legend 
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)} 
  
  legend_pie <- g_legend(pie_for_legend)
  legend_pie <- ggarrange(legend_pie, ncol = 1, nrow = 1)
  
  
  # Pour composer le graphique final avec les 2 legendes sous la carte
  file_path = paste("D:/Fac/Vacation CEFE/Focus_experts/Figures/wos_run_3_without_surface_ratio/wos_map_", taxa_name,"_raw_counts.pdf", sep="")
  pdf(file=file_path, width = 20, height = 12)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(12, 12)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
  print(result_plot, vp = vplayout(1:12, 1:12))
  print(legend_pie, vp = vplayout(9, 12))
  dev.off()
  
}














