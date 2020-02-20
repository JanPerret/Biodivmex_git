#################################################
#
# Biodivmex project
#
# WOS_04_maps.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### map of number of articles per country for terrestrial taxa

### informations per countries are in the following data frames :
# plant_article_loc_tab
# fungi_article_loc_tab
# amph_article_loc_tab
# rept_article_loc_tab
# bird_article_loc_tab
# mammal_article_loc_tab
# coleo_article_loc_tab
# papilio_article_loc_tab
# lumbri_article_loc_tab
# tree_article_loc_tab

# fish_article_loc_tab
# sponge_article_loc_tab
# crusta_article_loc_tab

### and general informations for each taxa are in the following data frame :
# taxa_table


### make texts with general informations to add as subtitles to the maps
plant_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "plant")
fungi_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "fungi")
amph_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "amphibian")
rept_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "reptile")
bird_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "bird")
mammal_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "mammal")
fish_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "fish")
sponge_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "sponge")
crusta_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "crustacea")
coleo_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "coleoptera")
papilio_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "papilionoidea")
lumbri_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "lumbricina")
tree_subtitle <- WOS_make_map_subtitle(taxa_table = taxa_table, taxa_name = "tree")



### store the legend for the pie charts
legend_pie_3_categories <- WOS_store_pie_chart_legend()


### load shapefiles
# layer with separated countries and islands
med_countries_islands <- read_sf("./data/med_countries_islands/med_countries_islands.shp")

# set all Chyprus polygons to the same country name
med_countries_islands$country[med_countries_islands$country == "Cyprus_Norther Cyprus"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Southern Cyprus"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Green Zone"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Dhekelia"] <- "Cyprus"
med_countries_islands$country[med_countries_islands$country == "Cyprus_Akrotiri"] <- "Cyprus"

# set Gaza strip and West Bank polygons to "Palestine" country name
med_countries_islands$country[med_countries_islands$country == "West Bank"] <- "Palestine"
med_countries_islands$country[med_countries_islands$country == "Gaza Strip"] <- "Palestine"

# remove countries that are too small or non-mediterranean 
med_countries_islands <- subset(med_countries_islands, med_countries_islands$country != "Bulgaria" &
                                      med_countries_islands$country != "Gibraltar" &
                                      med_countries_islands$country != "Monaco" &
                                      med_countries_islands$country != "Vatican" &
                                      med_countries_islands$country != "Jordan")

# load layer with the outline of the mediterranean region sensu Quezel & Medail (2003)
med_area_bounderies <- read_sf("./data/med_area_melted_final/med_area_melted_final.shp")

# set coordinates system
st_crs(med_area_bounderies) <- st_crs(med_countries_islands)

# load countries background maps from {rworldxtra} package 
data(countriesHigh)
world <- countriesHigh

# convert to sf format
world_sf <- st_as_sf(world)

# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -15.8519, ymin = 24.550494, xmax = 43.261471, ymax = 49.651575)
med_clipped <- st_crop(world_sf, box)

# get country centroids
# centroids <- st_centroid(med_countries_islands)
# write_csv2(centroids, path = "C:/Users/Perret/Desktop/centroids.csv")

# table with the cendroids to plot the pie charts to
med_centroids <- read_csv2("./data/country_centroids_for_maps.csv")

# generate coordinates for the point grid which will make the sea background
point_grid <- expand.grid(seq(from = -15.8519, to = 43.261471, by = 0.55),seq(from = 24.7, to = 49.66, by = 0.5))
colnames(point_grid) <- c("X","Y")

# create colour palette for the country filling
palette <- RColorBrewer::brewer.pal(9, "YlOrRd")
# palette <- RColorBrewer::brewer.pal(9, "PuBu") # for number of species


### make maps with country filling by number of articles and pie-charts indicationg corresponding author locality
plant_result_map <- WOS_map_number_articles(locality_table = plant_article_loc_tab, subtitle_text = plant_subtitle, taxa_name = "Embryophyta")
fungi_result_map <- WOS_map_number_articles(locality_table = fungi_article_loc_tab, subtitle_text = fungi_subtitle, taxa_name = "Fungi")
amph_result_map <- WOS_map_number_articles(locality_table = amph_article_loc_tab, subtitle_text = amph_subtitle, taxa_name = "Amphibians")
rept_result_map <- WOS_map_number_articles(locality_table = rept_article_loc_tab, subtitle_text = rept_subtitle, taxa_name = "Reptiles")
bird_result_map <- WOS_map_number_articles(locality_table = bird_article_loc_tab, subtitle_text = bird_subtitle, taxa_name = "Birds")
mammal_result_map <- WOS_map_number_articles(locality_table = mammal_article_loc_tab, subtitle_text = mammal_subtitle, taxa_name = "Mammals")
coleo_result_map <- WOS_map_number_articles(locality_table = coleo_article_loc_tab, subtitle_text = coleo_subtitle, taxa_name = "Coleoptera")
papilio_result_map <- WOS_map_number_articles(locality_table = papilio_article_loc_tab, subtitle_text = papilio_subtitle, taxa_name = "Papilionoidea")
lumbri_result_map <- WOS_map_number_articles(locality_table = lumbri_article_loc_tab, subtitle_text = lumbri_subtitle, taxa_name = "Lumbricina")
tree_result_map <- WOS_map_number_articles(locality_table = tree_article_loc_tab, subtitle_text = tree_subtitle, taxa_name = "Trees")


### compose and save the final maps (map + pie-charts + pie chart legend)
# plant
file_path = "./output/plots/WOS_map_plant.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(plant_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# fungi
file_path = "./output/plots/WOS_map_fungi.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fungi_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# amphibian
file_path = "./output/plots/WOS_map_amphibian.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(amph_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# reptile
file_path = "./output/plots/WOS_map_reptile.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(rept_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# bird
file_path = "./output/plots/WOS_map_bird.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(bird_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# mammal
file_path = "./output/plots/WOS_map_mammal.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(mammal_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# coleoptera
file_path = "./output/plots/WOS_map_coleoptera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(coleo_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# papilionoidea
file_path = "./output/plots/WOS_map_papilionoidea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(papilio_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# lumbricina
file_path = "./output/plots/WOS_map_lumbricina.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(lumbri_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# tree
file_path = "./output/plots/WOS_map_tree.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(tree_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

