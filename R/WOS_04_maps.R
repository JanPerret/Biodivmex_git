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


### l. 405 du fichier .\Biodivmex\Focus_experts\Script\WOS_maps_all_taxa_v2_without_surface_ratio_VERSION_REPRISE.R
### ON A LE DATAFRAME AVEC TOUTES LES DONNEES, ON PASSE AU CARTES

library(sf)
library(RColorBrewer)
# library(rworldxtra)
# library(rgdal)
# library(rgeos)
# library(broom)
# library(raster)


### load shapefiles 
# couche shapefile avec les pays et les iles separees
med_countries_islands <- read_sf("D:/Jan_Perret_CONSULTANCE/Biodivmex/Biodivmex_git/data/med_countries_islands/med_countries_islands.shp")

# set all Chyprus polygons to the same country name
med_countries_islands$country[med_countries_islands$country == "Cyprus_Norther Cyprus"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Southern Cyprus"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Green Zone"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Dhekelia"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Akrotiri"] <- "Cyprus"

# set Gaza strip and West Bank polygons to "Palestine"
med_countries_islands$country[med_countries_islands$country == "West Bank"] <- "Palestine"
med_countries_islands$country[med_countries_islands$country == "Gaza Strip"] <- "Palestine"

# remove countries that are too small or non_mediterranean countries 
med_countries_islands <- subset(med_countries_islands, med_countries_islands$country != "Bulgaria" &
                                      med_countries_islands$country != "Gibraltar" &
                                      med_countries_islands$country != "Monaco" &
                                      med_countries_islands$country != "Vatican" &
                                      med_countries_islands$country != "Jordan")

# couche shapefile avec le contour de la zone mediterraneenne selon Quezel & Medail (2003)
med_area_bounderies <- read_sf("D:/Jan_Perret_CONSULTANCE/Biodivmex/Focus_experts/Data/med_area_melted_final/med_area_melted_final.shp")
st_crs(med_area_bounderies) <- st_crs(med_countries_islands) # set coordinates system

# fond de carte des pays
data(countriesHigh)
world <- countriesHigh

# convert to sf format
world_sf <- st_as_sf(world)

# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -15.8519, ymin = 24.550494, xmax = 43.261471, ymax = 49.651575)
med_clipped <- st_crop(world_sf, box)

# add information to plot to med_countries_islands
# plant_article_loc_tab
tab_for_join <- plant_article_loc_tab[,c(1,6)]

# join information to MULTIPOLYGON object
med_countries_islands <- left_join(med_countries_islands, tab_for_join, by = c("country" = "fieldwork_country"))
table(med_countries_islands$country, med_countries_islands$n_articles)

# create colour palette for the country filling
palette <- RColorBrewer::brewer.pal(9, "YlOrRd")

# plot the map
map <- ggplot(NULL) + 
  geom_point(data = point_grid, aes(x = point_grid$X, y = point_grid$Y), color = "grey83", size = 0.2) + # les points qui fond la mer
  geom_sf(data = med_clipped, mapping = aes(), fill = "white") +                                         # les pays en blanc
  geom_sf(data = med_countries_islands, aes(fill = n_articles)) +                                        # les pays et iles mediterraneennes avec le remplissage
  geom_sf(data = med_area_bounderies, mapping = aes(), color = "orangered4", size = 0.6, fill = NA) +    # le contour de la zone mediterraneenne selon Quezel & Medail (2003)
  scale_fill_gradientn(name="Number of articles", colours=palette, na.value="white")+
  theme_minimal() +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ggtitle(paste("Number of articles in Web of Science about", taxa_name), subtitle = paste(text_subtitle)) +
  theme(plot.title = element_text(size = 24, hjust = 0.5), plot.subtitle=element_text(hjust=0.5)) +
  labs(x=NULL, y=NULL) +
  theme(plot.margin=unit(c(0,0,0,0),"mm")) +
  theme(legend.position="right",
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10)) +
  geom_segment(aes(x = 18.191610, y = 43.183912, xend = 24.27514, yend = 47.11614), size = 0.5) + # segment for Bosnia
  geom_segment(aes(x = 19.044711, y = 42.527407, xend = 24.27514, yend = 43.58586), size = 0.5) + # segment for Montenegro
  geom_segment(aes(x = 36.154312, y = 33.837813, xend = 38.89619, yend = 32.015), size = 0.5) + # segment for Lebanon
  geom_segment(aes(x = 35.258268, y = 32.170236, xend = 38.89619, yend = 27.615), size = 0.5) + # segment for Palestine
  geom_segment(aes(x = 15.87, y = 44.05, xend = 18.6, yend = 47.1), size = 0.5) # segment for Croatia
# map

#########################
#########################
#########################
#########################

# plant_article_loc_tab # donnees a representer
# med_countries_islands # donnees ou recuperer les centroids

# donnees a representer (info from_country / outside_med / inside_med)
loc_tab = plant_article_loc_tab
loc_tab <- loc_tab[,c(1:4)] # donnees a representer

loc_tab_long <- as_tibble(pivot_longer(loc_tab, 
                                        cols = colnames(loc_tab)[2]:colnames(loc_tab)[ncol(loc_tab)], 
                                        names_to = "author_loc",
                                        values_to = "n_articles"))

# recuperation des centroids
# test <- st_centroid(med_countries_islands)
# write_csv2(test, path = "C:/Users/Perret/Desktop/centroides.csv")
med_centroids <- read_csv2("D:/Jan_Perret_CONSULTANCE/Biodivmex/Biodivmex_git/data/country_centroids_for_maps.csv")

# centroid modifications for the islands
med_centroids[med_centroids$country == "Balearic Islands",]$x <- "4.442475"
med_centroids[med_centroids$country == "Balearic Islands",]$y <- "38.649819"
med_centroids[med_centroids$country == "Crete",]$x <- "25.707470"
med_centroids[med_centroids$country == "Crete",]$y <- "33.280089"
med_centroids[med_centroids$country == "Corsica",]$x <- "6.983637"
med_centroids[med_centroids$country == "Corsica",]$y <- "41.830008"
med_centroids[med_centroids$country == "Sardinia",]$x <- "10.859372"
med_centroids[med_centroids$country == "Sardinia",]$y <- "40.075018"
med_centroids[med_centroids$country == "Sicily",]$x <- "17.244710"
med_centroids[med_centroids$country == "Sicily",]$y <- "37.59007"
med_centroids$x <- as.numeric(med_centroids$x)
med_centroids$y <- as.numeric(med_centroids$y)


med_centroids <- left_join(med_centroids, loc_tab, by = c("country" = "fieldwork_country"))
# # A tibble: 32 x 7
# FID     country                    x     y from_country outside_med inside_med
# <dbl>   <chr>                  <dbl> <dbl>        <int>       <int>      <int>
# 1     0 Albania                19.8   40.1            7          11         25
# 2     6 Algeria                 2.07  29.8           53          26         81
# 3    43 Balearic Islands        2.91  39.6          167          20         19
# 4     2 Bosnia and Herzegovina 25.5   47.5            2           5          8
# 5    42 Corsica                 9.10  42.2           58          30         52
# 6    44 Crete                  24.8   35.2           33          47         40

# ## Pour inserer les camemberts local/distant
# # passer en format long pour pouvoir mettre 2 couleurs en fonction des auteurs local/distant
# data_long <- gather(taxa_data[,-3], key = "local_dist_article", value = "n_articles", c(local_articles,distant_articles), na.rm = FALSE)
# 
# med_centroids_wos <- full_join(med_centroids, wos_totals, by = c("country_name" = "country"))
# taxa_name = "plant"
# med_centroids_wos_taxa <- subset(med_centroids_wos, med_centroids_wos$taxa == taxa_name)
# # > med_centroids_wos_taxa
# #             x        y COUNTRY_IDS           country_name X.1  X n_articles  taxa local_articles distant_articles
# # 1   19.753990 40.14258           5                Albania   1  1         38 plant              6               32
# # 11  25.475140 47.51614          27 Bosnia and Herzegovina   3  3         14 plant              2               12
# # 21  30.800000 35.11500          59                 Cyprus   5  5        113 plant             20               93
# # 31   2.072943 29.83401          66                Algeria   2  2        161 plant             49              112
# # 41  29.623170 26.91488          68                  Egypt   6  6        184 plant             71              113


############ NOUVEAU PROGRAMME
#####  FINAL MAP
# Pie-charts

# country IF list
# centroids_ID_list <- sort(unique(as.integer(med_centroids$FID)))
centroids_ID_list <- unique(as.integer(med_centroids$FID))

# make a list of the pie charts objects
pie_chart_list <- 
  lapply( (centroids_ID_list), function(n){
    
    # title of the future pie chart
    name <- med_centroids[med_centroids$FID == as.character(n),]$country
    
    # make a small tibble containing just the 3 summary lines from_country / inside_med / outside_med
    wos_totals_country <- loc_tab_long[loc_tab_long$fieldwork_country == name,]
    wos_totals_country <- wos_totals_country %>% 
                              group_by(author_loc) %>% 
                              summarise (sum_n_articles = sum(n_articles))
   
     gt_plot <- ggplotGrob(
      # make the pie chart plot
      ggplot(wos_totals_country, aes(x=1, y = sum_n_articles, fill = author_loc)) +
        geom_bar(width = 1, stat = "identity", colour = "black") +
        coord_polar("y", start=0) +
        geom_text(aes(label = wos_totals_country$sum_n_articles, group = wos_totals_country$author_loc),
                  position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
        scale_fill_manual(values = c("#636363", "#bdbdbd", "#FFFFFF")) +
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
        theme(plot.margin = unit(c(0,0,0,0), "cm"))
      
      )
  } )

# Pour inserer les camemberts sur la carte
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
result_plot


# Storing pie legend 

# title of the future pie chart
name <- med_centroids[med_centroids$FID == as.character(n),]$country

# make a small tibble containing just the 3 summary lines from_country / inside_med / outside_med


med_centroids_france <- subset(med_centroids_wos, med_centroids_wos$country == "France")
name <- as.list(med_centroids_france$country_name)[1]
wos_totals_country <- loc_tab_long[loc_tab_long$fieldwork_country == name,]
wos_totals_country <- wos_totals_country %>% 
  group_by(author_loc) %>% 
  summarise (sum_n_articles = sum(n_articles))

pie_for_legend <- ggplot(wos_totals_country, aes(x=1, y = sum_n_articles, fill = author_loc)) +
  geom_bar(width = 1, stat = "identity", colour = "black") +
  coord_polar("y", start=0) +
  geom_text(aes(label = wos_totals_country$sum_n_articles, group = wos_totals_country$author_loc),
            position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
  scale_fill_manual(values = c("#636363", "#bdbdbd", "#FFFFFF")) + # labels = c("from", "within the country"), 
  labs(fill='Corresponding author') +
  theme(legend.background=element_blank(), legend.position="right")
# pie_for_legend

# extract Legend 
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 

library(ggpubr)
legend_pie <- g_legend(pie_for_legend)
legend_pie <- ggpubr::ggarrange(legend_pie, ncol = 1, nrow = 1)


# Pour composer le graphique final avec les 2 legendes sous la carte
# file_path = paste("D:/Fac/Vacation CEFE/Focus_experts/Figures/wos_run_3_without_surface_ratio/wos_map_", taxa_name,"_raw_counts.pdf", sep="")
# file_path = paste("./Figures/test/wos_map_", taxa_name,"_raw_counts.pdf", sep="")
file_path = "D:/Jan_Perret_CONSULTANCE/Biodivmex/Biodivmex_git/output/plots/WOS_map_test.pdf"

library(gridExtra)
library(grid)

pdf(file=file_path, width = 20, height = 12)
grid.newpage()
pushViewport(viewport(layout = grid.layout(12, 12)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(result_plot, vp = vplayout(1:12, 1:12))
print(legend_pie, vp = vplayout(9, 12))
dev.off()









# ############ ANCIEN PROGRAMME
# #####  FINAL MAP
# # Pie-charts
# centroids_ID_list <- sort(unique(as.integer(med_centroids_wos_taxa$COUNTRY_IDS)))
# pie_chart_list <- 
#   lapply( (centroids_ID_list), function(n){
#     name <- med_centroids[med_centroids$COUNTRY_IDS == as.character(n),]$country_name
#     wos_totals_country <- data_long[data_long$country == name,]
#     wos_totals_country <- wos_totals_country %>% 
#       group_by(local_dist_article) %>% 
#       summarise (sum_n_articles = sum(n_articles))
#     gt_plot <- ggplotGrob(
#       ggplot(wos_totals_country, aes(x=1, y = sum_n_articles, fill = local_dist_article)) +
#         geom_bar(width = 1, stat = "identity", colour = "black") +
#         coord_polar("y", start=0) +
#         geom_text(aes(label = wos_totals_country$sum_n_articles, group = wos_totals_country$local_dist_article),
#                   position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
#         scale_fill_manual(values = c("#7A7A7A", "#FFFFFF")) +
#         # theme_nothing() +
#         theme_minimal()+
#         ggtitle(name)+
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
#         theme(plot.margin = unit(c(0,0,0,0), "cm")))
#   } )
# 
# # Pour inserer les camemberts sur la carte
# pie_charts_annotation_list <- vector(mode = "list", length = length(pie_chart_list))
# pie_half_width = 1.3
# pie_half_height = 1.3
# 
# 
# for (num in 1:length(pie_chart_list)) {
#   value <- annotation_custom(grob = pie_chart_list[[num]],
#                              xmin = med_centroids$x[[num]] - pie_half_width,
#                              xmax = med_centroids$x[[num]] + pie_half_width,
#                              ymin = med_centroids$y[[num]] - pie_half_height,
#                              ymax = med_centroids$y[[num]] + pie_half_height)
#   pie_charts_annotation_list[[num]] <- value
# }
# 
# result_plot <- Reduce('+', pie_charts_annotation_list, map)
# result_plot



