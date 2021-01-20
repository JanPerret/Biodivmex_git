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
# porifera_article_loc_tab
# crusta_article_loc_tab

### and general informations for each taxa are in the following data frame :
# taxa_table


### make texts with general informations to add as subtitles to the maps
plant_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "plant")
fungi_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "fungi")
amph_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "amphibian")
rept_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "reptile")
bird_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "bird")
mammal_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "mammal")
fish_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "fish")
porifera_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "porifera")
crusta_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "crustacea")
coleo_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "coleoptera")
papilio_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "papilionoidea")
lumbri_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "lumbricina")
tree_subtitle <- WOS_make_map_subtitle(taxa_table = WOS_taxa_table, taxa_name = "tree")


### store the legend for the pie charts
legend_pie_3_categories <- WOS_store_pie_chart_legend_3()


### load shapefiles
# layer with separated countries and islands
med_countries_islands <- read_sf("./data/shapefiles/med_countries_islands_WOS/med_countries_islands.shp")

# set all Chyprus polygons to the same country name
med_countries_islands$country[med_countries_islands$country == "Cyprus_Northern Cyprus"] <- "Cyprus"
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
med_area_bounderies <- read_sf("./data/shapefiles/med_area_melted_final/med_area_melted_final.shp")

# set coordinates system
st_crs(med_area_bounderies) <- st_crs(med_countries_islands)

# load countries background maps from {rworldxtra} package 
data(countriesHigh)
world <- countriesHigh

# convert to sf format
world_sf <- st_as_sf(world)

# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -15.8519, ymin = 24.550494, xmax = 43.261471, ymax = 49.651575)
med_clipped <- st_crop(st_make_valid(world_sf), box)

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

### make synthesis maps
# sum data to plot
myers_article_loc_tab <- plant_article_loc_tab[, 2:6] + amph_article_loc_tab[, 2:6] + 
  rept_article_loc_tab[, 2:6] + bird_article_loc_tab[, 2:6] + mammal_article_loc_tab[, 2:6]
myers_article_loc_tab <- cbind(fieldwork_country = plant_article_loc_tab$fieldwork_country, myers_article_loc_tab)

non_myers_article_loc_tab <- fungi_article_loc_tab[, 2:6] + coleo_article_loc_tab[, 2:6] + 
  papilio_article_loc_tab[, 2:6]
non_myers_article_loc_tab <- cbind(fieldwork_country = plant_article_loc_tab$fieldwork_country, non_myers_article_loc_tab)

# to test with raw number of sequences
myers_map_raw <- WOS_map_number_articles(locality_table = myers_article_loc_tab, subtitle_text = "test map", taxa_name = "Myers taxa")
non_myers_map_raw <- WOS_map_number_articles(locality_table = non_myers_article_loc_tab, subtitle_text = "test map", taxa_name = "non-Myers taxa")

# add column with n_articles percentage per country
n_articles_percent <- myers_article_loc_tab$n_articles[1:27] / sum(myers_article_loc_tab$n_articles[1:27]) * 100
myers_article_loc_tab <- cbind(myers_article_loc_tab, n_articles_percent = c(n_articles_percent, NA))

n_articles_percent <- non_myers_article_loc_tab$n_articles[1:27] / sum(non_myers_article_loc_tab$n_articles[1:27]) * 100
non_myers_article_loc_tab <- cbind(non_myers_article_loc_tab, n_articles_percent = c(n_articles_percent, NA))

# save tables
write_csv2(myers_article_loc_tab, file = "./output/text/WOS_table_synthesis_map_myers.csv", col_names = TRUE)
write_csv2(non_myers_article_loc_tab, file = "./output/text/WOS_table_synthesis_map_non_myers.csv", col_names = TRUE)

# compose color palette for the synthesis maps
# palette_synthesis <- rev(RColorBrewer::brewer.pal(9, "RdYlBu")) # 9 colors
palette_synthesis <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))[-c(4, 5)] # 7 colors

# make the maps
terrestrial_myers_map <- WOS_map_number_articles_synthesis(locality_table = myers_article_loc_tab,
                                                           # subtitle_text = "Color scale midpoint: 4%",
                                                           subtitle_text = paste("Total number of articles :", sum(myers_article_loc_tab$n_articles[1:27])),
                                                           taxa_name = "Myers taxa",
                                                           color_scale_mid = 4)
terrestrial_non_myers_map <- WOS_map_number_articles_synthesis(locality_table = non_myers_article_loc_tab,
                                                               # subtitle_text = "Color scale midpoint: 4%",
                                                               subtitle_text = paste("Total number of articles :", sum(non_myers_article_loc_tab$n_articles[1:27])),
                                                               taxa_name = "non-Myers taxa",
                                                               color_scale_mid = 4)


### compose and save the final maps (map + pie-charts + pie chart legend)
# plant
file_path = "./output/plots/WOS_map_plant.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(plant_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# fungi
file_path = "./output/plots/WOS_map_fungi.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fungi_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# amphibian
file_path = "./output/plots/WOS_map_amphibian.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(amph_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
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
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# mammal
file_path = "./output/plots/WOS_map_mammal.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(mammal_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# coleoptera
file_path = "./output/plots/WOS_map_coleoptera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(coleo_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
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
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# tree
file_path = "./output/plots/WOS_map_tree.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(tree_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# synthesis map for Myers taxa
file_path = "./output/plots/WOS_map_myers_taxa.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(terrestrial_myers_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# synthesis map for non-Myers taxa
file_path = "./output/plots/WOS_map_non_myers_taxa.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(terrestrial_non_myers_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()


### maps for MARINE TAXA :
### store the legend for the pie charts
legend_pie_2_categories <- WOS_store_pie_chart_legend_2()

# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -10.5, ymin = 27.5, xmax = 41.5, ymax = 49)
med_clipped_marine <- st_crop(st_make_valid(world_sf), box)

# generate coordinates for the point grid which will make the sea background
point_grid_marine <- expand.grid(seq(from = -10.5, to = 41.5, by = 0.55), seq(from = 27.5, to = 49, by = 0.5))
colnames(point_grid_marine) <- c("X","Y")

# layer with the mediterranean marine regions
med_marine_region <- read_sf("./data/shapefiles/marine_regions/med_marine_regions.shp")

# layers with the borders of the 2 big regions
med_marine_big_region <- read_sf("./data/shapefiles/marine_regions/med_marine_big_regions.shp")
western_med <- subset(med_marine_big_region, med_marine_big_region$marine_reg == "Western Mediterranean Sea")
eastern_med <- subset(med_marine_big_region, med_marine_big_region$marine_reg == "Eastern Mediterranean Sea")

# information to draw the circles for the 2 big regions
circle_centroid <- data.frame(marine_reg = c("Western Mediterranean Sea","Eastern Mediterranean Sea"), x = c(0.02,35.5), y = c(31.7,40.3))

# table with the coordinates to plot the pie charts
marine_region_centroids <- read_csv2("./data/marine_region_centroid_for_maps.csv")

# make the maps
fish_result_map <- WOS_map_number_articles_marine(locality_table = fish_article_loc_tab, subtitle_text = fish_subtitle, taxa_name = "Fish")
porifera_result_map <- WOS_map_number_articles_marine(locality_table = porifera_article_loc_tab, subtitle_text = porifera_subtitle, taxa_name = "Porifera")
crusta_result_map <- WOS_map_number_articles_marine(locality_table = crusta_article_loc_tab, subtitle_text = crusta_subtitle, taxa_name = "Crustacea")


### make synthesis maps
# sum data to plot
marine_article_loc_tab <- fish_article_loc_tab[, 2:5] + porifera_article_loc_tab[, 2:5] + crusta_article_loc_tab[, 2:5]
marine_article_loc_tab <- cbind(marine_region = fish_article_loc_tab$marine_region, marine_article_loc_tab)

# test with raw number of sequences
# myers_map_raw <- WOS_map_number_articles_marine(locality_table = marine_article_loc_tab, subtitle_text = "test map", taxa_name = "Myers taxa")

# add column with n_articles percentage per country
n_articles_percent <- marine_article_loc_tab$n_articles[1:19] / sum(marine_article_loc_tab$n_articles[1:19]) * 100
marine_article_loc_tab <- cbind(marine_article_loc_tab, n_articles_percent = c(n_articles_percent, NA))

# save table
write_csv2(marine_article_loc_tab, file = "./output/text/WOS_table_synthesis_map_marine.csv", col_names = TRUE)

# make the map
marine_result_map <- WOS_map_number_articles_marine_synthesis(locality_table = marine_article_loc_tab,
                                                       subtitle_text = paste("Total number of articles :", sum(marine_article_loc_tab$n_articles[1:19])),
                                                       taxa_name = "Myers taxa",
                                                       color_scale_mid = 4)

### compose and save final maps for marine taxa
# fish
file_path = "./output/plots/WOS_map_fish.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fish_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_2_categories, vp = vplayout(9, 12))
dev.off()

# porifera
file_path = "./output/plots/WOS_map_porifera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(porifera_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_2_categories, vp = vplayout(9, 12))
dev.off()

# crustacea
file_path = "./output/plots/WOS_map_crusta.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(crusta_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_2_categories, vp = vplayout(9, 12))
dev.off()

# synthesis map for marine taxa
file_path = "./output/plots/WOS_map_marine_taxa.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(marine_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_2_categories, vp = vplayout(9, 12))
dev.off()


### make maps for EVENNESS
# MARINE TAXA
# get number of articles per marine region for each taxa
marine_article_evenness_tab <- cbind(fish = fish_article_loc_tab$n_articles,
                                    porifera = porifera_article_loc_tab$n_articles,
                                    crusta = crusta_article_loc_tab$n_articles)
# get percentages
marine_article_evenness_tab <- marine_article_evenness_tab/rowSums(marine_article_evenness_tab)
marine_article_evenness_tab[is.na(marine_article_evenness_tab)] <- 0 # because NAs are generated at the libe above

# square the percentage
marine_article_evenness_tab2 <- marine_article_evenness_tab*marine_article_evenness_tab
colnames(marine_article_evenness_tab2) <- paste0(colnames(marine_article_evenness_tab2), "2")

# calculate simpson index
simpson <- (1/rowSums(marine_article_evenness_tab2)) / ncol(marine_article_evenness_tab2)

# make final table
marine_article_evenness_tab <- cbind(marine_region = fish_article_loc_tab[,1], marine_article_evenness_tab, marine_article_evenness_tab2, simpson = simpson)
marine_article_evenness_tab[marine_article_evenness_tab == Inf] <- NA # this is when there is 0 reference for a study area


# TERRESTRIAL TAXA
# get number of articles per country for each taxa
terrestrial_article_evenness_tab <- cbind(plant = plant_article_loc_tab$n_articles,
                                    fungi = fungi_article_loc_tab$n_articles,
                                    amphibian = amph_article_loc_tab$n_articles,
                                    reptile = rept_article_loc_tab$n_articles,
                                    bird = bird_article_loc_tab$n_articles,
                                    mammal = mammal_article_loc_tab$n_articles,
                                    coleo = coleo_article_loc_tab$n_articles,
                                    papilio = papilio_article_loc_tab$n_articles)

# get percentages
terrestrial_article_evenness_tab <- terrestrial_article_evenness_tab/rowSums(terrestrial_article_evenness_tab)
terrestrial_article_evenness_tab[is.na(terrestrial_article_evenness_tab)] <- 0 # because NAs are generated at the libe above

# square the percentage
terrestrial_article_evenness_tab2 <- terrestrial_article_evenness_tab*terrestrial_article_evenness_tab
colnames(terrestrial_article_evenness_tab2) <- paste0(colnames(terrestrial_article_evenness_tab2), "2")

# calculate simpson index
simpson <- (1/rowSums(terrestrial_article_evenness_tab2)) / ncol(terrestrial_article_evenness_tab2)

# make final table
terrestrial_article_evenness_tab <- cbind(fieldwork_country = plant_article_loc_tab[,1], terrestrial_article_evenness_tab, terrestrial_article_evenness_tab2, simpson = simpson)
terrestrial_article_evenness_tab[terrestrial_article_evenness_tab == Inf] <- NA # this is when there is 0 reference for a study area

# save tables
write_csv2(marine_article_evenness_tab, file = "./output/text/WOS_table_evenness_map_marine.csv", col_names = TRUE)
write_csv2(terrestrial_article_evenness_tab, file = "./output/text/WOS_table_evenness_map_terrestrial.csv", col_names = TRUE)

# make the maps
palette_evenness <- RColorBrewer::brewer.pal(5, "YlGn")

WOS_marine_evenness_map <- WOS_map_number_articles_marine_evenness(locality_table = marine_article_evenness_tab,
                                                                   subtitle_text = paste("."),
                                                                   taxa_name = "Marine taxa")

WOS_terrestrial_evenness_map <- WOS_map_number_articles_evenness(locality_table = terrestrial_article_evenness_tab,
                                                                 subtitle_text = paste("."),
                                                                 taxa_name = "Terrestrial taxa")

# save maps
# marine taxa
file_path = "./output/plots/WOS_map_marine_taxa_evenness.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(WOS_marine_evenness_map, vp = vplayout(1:12, 1:12))
dev.off()

# terrestrial taxa
file_path = "./output/plots/WOS_map_terrestrial_taxa_evenness.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(WOS_terrestrial_evenness_map, vp = vplayout(1:12, 1:12))
dev.off()

