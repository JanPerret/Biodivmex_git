#################################################
#
# Biodivmex project
#
# GenBank_04_maps.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### informations per countries are in the following data frames :
# plant_seq_loc_tab
# fungi_seq_loc_tab
# amph_seq_loc_tab
# rept_seq_loc_tab
# bird_seq_loc_tab
# mammal_seq_loc_tab
# coleo_seq_loc_tab
# papilio_seq_loc_tab
# lumbri_seq_loc_tab
# fish_seq_loc_tab
# sponge_seq_loc_tab
# crusta_seq_loc_tab
# tree_seq_loc_tab

### general informations for each taxa to print in the subtitles are there :
# all_taxa_desc_tab


### make texts with general informations to add as subtitles to the maps
plant_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Embryophyta", taxa_name = "plant")
fungi_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Fungi", taxa_name = "fungi")
amph_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "amphibian")
rept_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "reptile")
bird_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "bird")
mammal_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "mammal")
fish_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "fish")
sponge_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "sponge")
crusta_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "crustacea")
coleo_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "coleoptera")
papilio_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "papilionoidea")
lumbri_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "lumbricina")
tree_subtitle <- GenBank_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Embryophyta", taxa_name = "tree")


### store the legend for the pie charts
legend_pie_3_categories <- GenBank_store_pie_chart_legend()


### load shapefiles
# layer with separated countries and islands
med_countries_islands <- read_sf("./data/med_countries_islands_GenBank/med_countries_islands_GenBank.shp") ################### "./data/med_countries_islands_GenBank/med_countries_islands.shp"

# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -15.8519, ymin = 24.550494, xmax = 43.261471, ymax = 49.651575)
med_countries_islands <- st_crop(med_countries_islands, box)

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

# load countries background maps from {rworldxtra} package 
data(countriesHigh)
world <- countriesHigh

# convert to sf format
world_sf <- st_as_sf(world)

####################
# export pour faire la nouvelle couche des pays :
# st_write(world_sf, "C:/Users/Perret/Desktop/world_sf.shp", driver="ESRI Shapefile")  # create to a shapefile 
####################

# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -15.8519, ymin = 24.550494, xmax = 43.261471, ymax = 49.651575)
med_clipped <- st_crop(world_sf, box)

# table with the cendroids to plot the pie charts to
med_centroids <- read_csv2("./data/country_centroids_for_maps.csv")

# generate coordinates for the point grid which will make the sea background
point_grid <- expand.grid(seq(from = -15.8519, to = 43.261471, by = 0.55),seq(from = 24.7, to = 49.66, by = 0.5))
colnames(point_grid) <- c("X","Y")

# create colour palette for the country filling
palette <- RColorBrewer::brewer.pal(9, "YlOrRd")


### make maps with country filling by number of articles and pie-charts indicationg corresponding author locality
plant_result_map <- GenBank_map_number_sequence(locality_table = plant_seq_loc_tab, subtitle_text = plant_subtitle, taxa_name = "Embryophyta")
fungi_result_map <- GenBank_map_number_sequence(locality_table = fungi_seq_loc_tab, subtitle_text = fungi_subtitle, taxa_name = "Fungi")
amph_result_map <- GenBank_map_number_sequence(locality_table = amph_seq_loc_tab, subtitle_text = amph_subtitle, taxa_name = "Amphibians")
rept_result_map <- GenBank_map_number_sequence(locality_table = rept_seq_loc_tab, subtitle_text = rept_subtitle, taxa_name = "Reptiles")
bird_result_map <- GenBank_map_number_sequence(locality_table = bird_seq_loc_tab, subtitle_text = bird_subtitle, taxa_name = "Birds")
mammal_result_map <- GenBank_map_number_sequence(locality_table = mammal_seq_loc_tab, subtitle_text = mammal_subtitle, taxa_name = "Mammals")
coleo_result_map <- GenBank_map_number_sequence(locality_table = coleo_seq_loc_tab, subtitle_text = coleo_subtitle, taxa_name = "Coleoptera")
papilio_result_map <- GenBank_map_number_sequence(locality_table = papilio_seq_loc_tab, subtitle_text = papilio_subtitle, taxa_name = "Papilionoidea")
lumbri_result_map <- GenBank_map_number_sequence(locality_table = lumbri_seq_loc_tab, subtitle_text = lumbri_subtitle, taxa_name = "Lumbricina")
tree_result_map <- GenBank_map_number_sequence(locality_table = tree_seq_loc_tab, subtitle_text = tree_subtitle, taxa_name = "Trees")

# marine taxa ################### I keep it like that for the moment but I will have to replace the layer with the countries by a layer with each country's ZEE
fish_result_map <- GenBank_map_number_sequence(locality_table = fish_seq_loc_tab, subtitle_text = fish_subtitle, taxa_name = "Fish")
sponge_result_map <- GenBank_map_number_sequence(locality_table = sponge_seq_loc_tab, subtitle_text = sponge_subtitle, taxa_name = "Porifera")
crusta_result_map <- GenBank_map_number_sequence(locality_table = crusta_seq_loc_tab, subtitle_text = crusta_subtitle, taxa_name = "Crustacea")



### compose and save the final maps (map + pie-charts + pie chart legend)
# plant
file_path = "./output/plots/GenBank_map_sequences_plant.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(plant_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# fungi
file_path = "./output/plots/GenBank_map_sequences_fungi.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fungi_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# amphibian
file_path = "./output/plots/GenBank_map_sequences_amphibian.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(amph_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# reptile
file_path = "./output/plots/GenBank_map_sequences_reptile.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(rept_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# bird
file_path = "./output/plots/GenBank_map_sequences_bird.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(bird_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# mammal
file_path = "./output/plots/GenBank_map_sequences_mammal.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(mammal_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# coleoptera
file_path = "./output/plots/GenBank_map_sequences_coleoptera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(coleo_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# papilionoidea
file_path = "./output/plots/GenBank_map_sequences_papilionoidea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(papilio_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# lumbricina
file_path = "./output/plots/GenBank_map_sequences_lumbricina.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(lumbri_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# tree
file_path = "./output/plots/GenBank_map_sequences_tree.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(tree_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# fish
file_path = "./output/plots/GenBank_map_sequences_fish.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fish_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# sponge
file_path = "./output/plots/GenBank_map_sequences_sponge.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(sponge_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# crustacea
file_path = "./output/plots/GenBank_map_sequences_crustacea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(crusta_result_map, vp = vplayout(1:12, 1:12))
print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()



### make maps with country filling by NUMBER OF SPECIES

# change color palette
palette <- RColorBrewer::brewer.pal(9, "Purples") # for number of species

# make the maps
plant_species_map <- GenBank_map_number_species(species_level_tab = plant_species_level_tab, subtitle_text = plant_subtitle, taxa_name = "Embryophyta")
fungi_species_map <- GenBank_map_number_species(species_level_tab = fungi_species_level_tab, subtitle_text = fungi_subtitle, taxa_name = "Fungi")
amph_species_map <- GenBank_map_number_species(species_level_tab = amph_species_level_tab, subtitle_text = amph_subtitle, taxa_name = "Amphibians")
rept_species_map <- GenBank_map_number_species(species_level_tab = rept_species_level_tab, subtitle_text = rept_subtitle, taxa_name = "Reptiles")
bird_species_map <- GenBank_map_number_species(species_level_tab = bird_species_level_tab, subtitle_text = bird_subtitle, taxa_name = "Birds")
mammal_species_map <- GenBank_map_number_species(species_level_tab = mammal_species_level_tab, subtitle_text = mammal_subtitle, taxa_name = "Mammals")
coleo_species_map <- GenBank_map_number_species(species_level_tab = coleo_species_level_tab, subtitle_text = coleo_subtitle, taxa_name = "Coleoptera")
papilio_species_map <- GenBank_map_number_species(species_level_tab = papilio_species_level_tab, subtitle_text = papilio_subtitle, taxa_name = "Papilionoidea")
lumbri_species_map <- GenBank_map_number_species(species_level_tab = lumbri_species_level_tab, subtitle_text = lumbri_subtitle, taxa_name = "Lumbricina")
tree_species_map <- GenBank_map_number_species(species_level_tab = tree_species_level_tab, subtitle_text = tree_subtitle, taxa_name = "Trees")

# marine taxa ################### I keep it like that for the moment but I will have to replace the layer with the countries by a layer with each country's ZEE
fish_species_map <- GenBank_map_number_species(species_level_tab = fish_species_level_tab, subtitle_text = fish_subtitle, taxa_name = "Fish")
sponge_species_map <- GenBank_map_number_species(species_level_tab = sponge_species_level_tab, subtitle_text = sponge_subtitle, taxa_name = "Porifera")
crusta_species_map <- GenBank_map_number_species(species_level_tab = crusta_species_level_tab, subtitle_text = crusta_subtitle, taxa_name = "Crustacea")


### save the maps
# plant
file_path = "./output/plots/GenBank_map_species_plant.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(plant_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# fungi
file_path = "./output/plots/GenBank_map_species_fungi.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fungi_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# amphibian
file_path = "./output/plots/GenBank_map_species_amphibian.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(amph_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# reptile
file_path = "./output/plots/GenBank_map_species_reptile.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(rept_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# bird
file_path = "./output/plots/GenBank_map_species_bird.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(bird_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# mammal
file_path = "./output/plots/GenBank_map_species_mammal.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(mammal_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# coleoptera
file_path = "./output/plots/GenBank_map_species_coleoptera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(coleo_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# papilionoidea
file_path = "./output/plots/GenBank_map_species_papilionoidea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(papilio_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# lumbricina
file_path = "./output/plots/GenBank_map_species_lumbricina.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(lumbri_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# tree
file_path = "./output/plots/GenBank_map_species_tree.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(tree_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# fish
file_path = "./output/plots/GenBank_map_species_fish.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fish_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# sponge
file_path = "./output/plots/GenBank_map_species_sponge.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(sponge_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# crustacea
file_path = "./output/plots/GenBank_map_species_crustacea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(crusta_species_map, vp = vplayout(1:12, 1:12))
dev.off()















