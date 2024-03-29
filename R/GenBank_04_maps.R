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
# porifera_seq_loc_tab
# crusta_seq_loc_tab
# tree_seq_loc_tab

### general informations for each taxa to print in the subtitles are there :
# all_taxa_desc_tab


### make texts with general informations to add as subtitles to the maps
plant_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Embryophyta", taxa_name = "plant")
fungi_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Fungi", taxa_name = "fungi")
amph_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "amphibian")
rept_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "reptile")
bird_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "bird")
mammal_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "mammal")
fish_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "fish")
porifera_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "porifera")
crusta_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "crustacea")
coleo_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "coleoptera")
papilio_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "papilionoidea")
lumbri_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Metazoa", taxa_name = "lumbricina")
tree_subtitle <- GB_make_map_subtitle(taxa_table = all_taxa_desc_tab, kingdom_name = "Embryophyta", taxa_name = "tree")


### store the legend for the pie charts
legend_pie_3_categories <- GB_store_pie_chart_legend()


### load shapefiles
# layer with separated countries and islands
med_countries_islands <- read_sf("./data/shapefiles/med_countries_islands_GenBank/med_countries_islands_GenBank.shp")

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

# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -15.8519, ymin = 24.550494, xmax = 43.261471, ymax = 49.651575)
med_clipped <- st_crop(st_make_valid(world_sf), box)

# table with the cendroids to plot the pie charts to
med_centroids <- read_csv2("./data/country_centroids_for_maps.csv")

# generate coordinates for the point grid which will make the sea background
point_grid <- expand.grid(seq(from = -15.8519, to = 43.261471, by = 0.55),seq(from = 24.7, to = 49.66, by = 0.5))
colnames(point_grid) <- c("X","Y")

# create colour palette for the country filling
palette <- RColorBrewer::brewer.pal(9, "YlOrRd")


### make maps with country filling by number of sequences and pie-charts indicationg sequencer nationality
plant_result_map <- GB_map_number_sequence(locality_table = plant_seq_loc_tab, subtitle_text = plant_subtitle, taxa_name = "Embryophyta")
fungi_result_map <- GB_map_number_sequence(locality_table = fungi_seq_loc_tab, subtitle_text = fungi_subtitle, taxa_name = "Fungi")
amph_result_map <- GB_map_number_sequence(locality_table = amph_seq_loc_tab, subtitle_text = amph_subtitle, taxa_name = "Amphibians")
rept_result_map <- GB_map_number_sequence(locality_table = rept_seq_loc_tab, subtitle_text = rept_subtitle, taxa_name = "Reptiles")
bird_result_map <- GB_map_number_sequence(locality_table = bird_seq_loc_tab, subtitle_text = bird_subtitle, taxa_name = "Birds")
mammal_result_map <- GB_map_number_sequence(locality_table = mammal_seq_loc_tab, subtitle_text = mammal_subtitle, taxa_name = "Mammals")
coleo_result_map <- GB_map_number_sequence(locality_table = coleo_seq_loc_tab, subtitle_text = coleo_subtitle, taxa_name = "Coleoptera")
papilio_result_map <- GB_map_number_sequence(locality_table = papilio_seq_loc_tab, subtitle_text = papilio_subtitle, taxa_name = "Papilionoidea")
lumbri_result_map <- GB_map_number_sequence(locality_table = lumbri_seq_loc_tab, subtitle_text = lumbri_subtitle, taxa_name = "Lumbricina")
tree_result_map <- GB_map_number_sequence(locality_table = tree_seq_loc_tab, subtitle_text = tree_subtitle, taxa_name = "Trees")


### maps for MARINE TAXA :
# crop world_sf polygons to keep only polygons inside a bounding box centered on the mediterranean sea
box = c(xmin = -10.5, ymin = 27.5, xmax = 41.5, ymax = 49)
med_clipped_marine <- st_crop(st_make_valid(world_sf), box)

# generate coordinates for the point grid which will make the sea background
point_grid_marine <- expand.grid(seq(from = -10.5, to = 41.5, by = 0.55), seq(from = 27.5, to = 49, by = 0.5))
colnames(point_grid_marine) <- c("X","Y")

# layer with mediterranean countries EEZ
med_EEZ <- read_sf("./data/shapefiles/marine_EEZ/EEZ_med_3.shp")

# table with the cendroids to plot the pie charts to
EEZ_centroids <- read_csv2("./data/EEZ_centroids_for_maps.csv")

# make the maps
fish_result_map <- GB_map_number_sequence_marine_taxa(locality_table = fish_seq_loc_tab, subtitle_text = fish_subtitle, taxa_name = "Fish")
porifera_result_map <- GB_map_number_sequence_marine_taxa(locality_table = porifera_seq_loc_tab, subtitle_text = porifera_subtitle, taxa_name = "Porifera")
crusta_result_map <- GB_map_number_sequence_marine_taxa(locality_table = crusta_seq_loc_tab, subtitle_text = crusta_subtitle, taxa_name = "Crustacea")

### SYNTHESIS MAPS
# sum data to plot
myers_seq_loc_tab <- plant_seq_loc_tab[, 2:6] + amph_seq_loc_tab[, 2:6] + rept_seq_loc_tab[, 2:6] + 
  bird_seq_loc_tab[, 2:6] + mammal_seq_loc_tab[, 2:6]
myers_seq_loc_tab <- cbind(sample_origin = plant_seq_loc_tab$sample_origin, myers_seq_loc_tab)

non_myers_seq_loc_tab <- fungi_seq_loc_tab[, 2:6] + coleo_seq_loc_tab[, 2:6] + papilio_seq_loc_tab[, 2:6]
non_myers_seq_loc_tab <- cbind(sample_origin = fungi_seq_loc_tab$sample_origin, non_myers_seq_loc_tab)

marine_seq_loc_tab <- fish_seq_loc_tab[, 2:6] + porifera_seq_loc_tab[, 2:6] + crusta_seq_loc_tab[, 2:6]
marine_seq_loc_tab <- cbind(sample_origin = fish_seq_loc_tab$sample_origin, marine_seq_loc_tab)

# add column with n_seq percentage per country
n_seq_percent <- myers_seq_loc_tab$n_seq[1:27] / sum(myers_seq_loc_tab$n_seq[1:27]) * 100
myers_seq_loc_tab <- cbind(myers_seq_loc_tab, n_seq_percent = n_seq_percent)

n_seq_percent <- non_myers_seq_loc_tab$n_seq[1:27] / sum(non_myers_seq_loc_tab$n_seq[1:27]) * 100
non_myers_seq_loc_tab <- cbind(non_myers_seq_loc_tab, n_seq_percent = n_seq_percent)

n_seq_percent <- marine_seq_loc_tab$n_seq[1:19] / sum(marine_seq_loc_tab$n_seq[1:19]) * 100
marine_seq_loc_tab <- cbind(marine_seq_loc_tab, n_seq_percent = n_seq_percent)

# save the tables
write_csv2(myers_seq_loc_tab, file = "./output/text/GenBank_table_synthesis_map_myers.csv", col_names = TRUE)
write_csv2(non_myers_seq_loc_tab, file = "./output/text/GenBank_table_synthesis_map_non_myers.csv", col_names = TRUE)
write_csv2(marine_seq_loc_tab, file = "./output/text/GenBank_table_synthesis_map_marine.csv", col_names = TRUE)

# make the maps
# compose color palette for the synthesis maps
# palette_synthesis <- rev(RColorBrewer::brewer.pal(9, "RdYlBu")) # 9 colors
palette_synthesis <- rev(RColorBrewer::brewer.pal(9, "RdYlBu"))[-c(4, 5)] # 7 colors

# test plot raw counts
# GB_map_number_sequence(locality_table = myers_seq_loc_tab, subtitle_text = "test map raw counts", taxa_name = "Myers taxa")
# GB_map_number_sequence(locality_table = non_myers_seq_loc_tab, subtitle_text = "test map raw counts", taxa_name = "non Myers taxa")
# GB_map_number_sequence_marine_taxa(locality_table = marine_seq_loc_tab, subtitle_text = "test map raw counts", taxa_name = "marine taxa")

# make the final maps
GB_myers_result_map <- GB_map_number_sequence_synthesis(locality_table = myers_seq_loc_tab,
                                 subtitle_text = paste("Total number of sequences :", sum(myers_seq_loc_tab$n_seq[1:27])),
                                 taxa_name = "Myers taxa",
                                 color_scale_mid = 4)

GB_non_myers_result_map <- GB_map_number_sequence_synthesis(locality_table = non_myers_seq_loc_tab,
                                 subtitle_text = paste("Total number of sequences :", sum(non_myers_seq_loc_tab$n_seq[1:27])),
                                 taxa_name = "non Myers taxa",
                                 color_scale_mid = 4)

GB_marine_result_map <- GB_map_number_sequence_marine_taxa_synthesis(locality_table = marine_seq_loc_tab,
                                             subtitle_text = paste("Total number of sequences :", sum(marine_seq_loc_tab$n_seq[1:19])),
                                             taxa_name = "marine taxa",
                                             color_scale_mid = 4)

### EVENNESS MAPS
# TERRESTRIAL TAXA
# get number of sequences per country for each taxa
terrestrial_seq_evenness_tab <- cbind(plant = plant_seq_loc_tab$n_seq,
                                      fungi = fungi_seq_loc_tab$n_seq,
                                      amphibian = amph_seq_loc_tab$n_seq,
                                      reptile = rept_seq_loc_tab$n_seq,
                                      bird = bird_seq_loc_tab$n_seq,
                                      mammal = mammal_seq_loc_tab$n_seq,
                                      coleo = coleo_seq_loc_tab$n_seq,
                                      papilio = papilio_seq_loc_tab$n_seq)

# get percentages
terrestrial_seq_evenness_tab <- terrestrial_seq_evenness_tab/rowSums(terrestrial_seq_evenness_tab)
terrestrial_seq_evenness_tab[is.na(terrestrial_seq_evenness_tab)] <- 0 # because NAs are generated at the libe above

# square the percentage
terrestrial_seq_evenness_tab2 <- terrestrial_seq_evenness_tab*terrestrial_seq_evenness_tab
colnames(terrestrial_seq_evenness_tab2) <- paste0(colnames(terrestrial_seq_evenness_tab2), "2")

# calculate simpson index
simpson <- (1/rowSums(terrestrial_seq_evenness_tab2)) / ncol(terrestrial_seq_evenness_tab2)

# make final table
terrestrial_seq_evenness_tab <- cbind(sample_origin = plant_seq_loc_tab[,1], terrestrial_seq_evenness_tab, terrestrial_seq_evenness_tab2, simpson = simpson)
terrestrial_seq_evenness_tab[terrestrial_seq_evenness_tab == Inf] <- NA # this is when there is 0 reference for a study area


# MARINE TAXA
# get number of seqs per marine region for each taxa
marine_seq_evenness_tab <- cbind(fish = fish_seq_loc_tab$n_seq,
                                     porifera = porifera_seq_loc_tab$n_seq,
                                     crusta = crusta_seq_loc_tab$n_seq)
# get percentages
marine_seq_evenness_tab <- marine_seq_evenness_tab/rowSums(marine_seq_evenness_tab)
marine_seq_evenness_tab[is.na(marine_seq_evenness_tab)] <- 0 # because NAs are generated at the libe above

# square the percentage
marine_seq_evenness_tab2 <- marine_seq_evenness_tab*marine_seq_evenness_tab
colnames(marine_seq_evenness_tab2) <- paste0(colnames(marine_seq_evenness_tab2), "2")

# calculate simpson index
simpson <- (1/rowSums(marine_seq_evenness_tab2)) / ncol(marine_seq_evenness_tab2)

# make final table
marine_seq_evenness_tab <- cbind(sample_origin = fish_seq_loc_tab[,1], marine_seq_evenness_tab, marine_seq_evenness_tab2, simpson = simpson)
marine_seq_evenness_tab[marine_seq_evenness_tab == Inf] <- NA # this is when there is 0 reference for a study area

# save tables
write_csv2(terrestrial_seq_evenness_tab, file = "./output/text/GenBank_table_evenness_map_terrestrial.csv", col_names = TRUE)
write_csv2(marine_seq_evenness_tab, file = "./output/text/GenBank_table_evenness_map_marine.csv", col_names = TRUE)

# make the maps
palette_evenness <- RColorBrewer::brewer.pal(5, "YlGn")

GB_terrestrial_evenness_map <- GB_map_number_sequence_evenness(locality_table = terrestrial_seq_evenness_tab,
                                                            subtitle_text = paste(""),
                                                            taxa_name = "Terrestrial taxa")

GB_marine_evenness_map <- GB_map_number_sequence_marine_taxa_evenness(locality_table = marine_seq_evenness_tab,
                                                                   subtitle_text = paste(""),
                                                                   taxa_name = "Marine taxa")



### compose and save the final maps (map + pie-charts + pie chart legend)
# plant
file_path = "./output/plots/GenBank_map_sequences_plant.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(plant_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# fungi
file_path = "./output/plots/GenBank_map_sequences_fungi.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fungi_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# amphibian
file_path = "./output/plots/GenBank_map_sequences_amphibian.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(amph_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# reptile
file_path = "./output/plots/GenBank_map_sequences_reptile.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(rept_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# bird
file_path = "./output/plots/GenBank_map_sequences_bird.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(bird_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# mammal
file_path = "./output/plots/GenBank_map_sequences_mammal.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(mammal_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# coleoptera
file_path = "./output/plots/GenBank_map_sequences_coleoptera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(coleo_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# papilionoidea
file_path = "./output/plots/GenBank_map_sequences_papilionoidea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(papilio_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# lumbricina
file_path = "./output/plots/GenBank_map_sequences_lumbricina.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(lumbri_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# tree
file_path = "./output/plots/GenBank_map_sequences_tree.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(tree_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# fish
file_path = "./output/plots/GenBank_map_sequences_fish.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(fish_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# porifera
file_path = "./output/plots/GenBank_map_sequences_porifera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(porifera_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# crustacea
file_path = "./output/plots/GenBank_map_sequences_crustacea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(crusta_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# Myers synthesis map
file_path = "./output/plots/GenBank_map_sequences_myers_taxa.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(GB_myers_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# non-Myers synthesis map
file_path = "./output/plots/GenBank_map_sequences_non_myers_taxa.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(GB_non_myers_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# marine taxa synthesis map
file_path = "./output/plots/GenBank_map_sequences_marine_taxa.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(GB_marine_result_map, vp = vplayout(1:12, 1:12))
# print(legend_pie_3_categories, vp = vplayout(9, 12))
dev.off()

# terrestrial taxa evenness map
file_path = "./output/plots/GenBank_map_terrestrial_taxa_evenness.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(GB_terrestrial_evenness_map, vp = vplayout(1:12, 1:12))
dev.off()

# marine taxa evenness map
file_path = "./output/plots/GenBank_map_marine_taxa_evenness.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(GB_marine_evenness_map, vp = vplayout(1:12, 1:12))
dev.off()


### make maps with country filling by NUMBER OF SPECIES

# change color palette
palette <- RColorBrewer::brewer.pal(9, "Purples") # for number of species

# make the maps
plant_species_map <- GB_map_number_species(species_level_tab = plant_species_level_tab, subtitle_text = plant_subtitle, taxa_name = "Embryophyta")
fungi_species_map <- GB_map_number_species(species_level_tab = fungi_species_level_tab, subtitle_text = fungi_subtitle, taxa_name = "Fungi")
amph_species_map <- GB_map_number_species(species_level_tab = amph_species_level_tab, subtitle_text = amph_subtitle, taxa_name = "Amphibians")
rept_species_map <- GB_map_number_species(species_level_tab = rept_species_level_tab, subtitle_text = rept_subtitle, taxa_name = "Reptiles")
bird_species_map <- GB_map_number_species(species_level_tab = bird_species_level_tab, subtitle_text = bird_subtitle, taxa_name = "Birds")
mammal_species_map <- GB_map_number_species(species_level_tab = mammal_species_level_tab, subtitle_text = mammal_subtitle, taxa_name = "Mammals")
coleo_species_map <- GB_map_number_species(species_level_tab = coleo_species_level_tab, subtitle_text = coleo_subtitle, taxa_name = "Coleoptera")
papilio_species_map <- GB_map_number_species(species_level_tab = papilio_species_level_tab, subtitle_text = papilio_subtitle, taxa_name = "Papilionoidea")
lumbri_species_map <- GB_map_number_species(species_level_tab = lumbri_species_level_tab, subtitle_text = lumbri_subtitle, taxa_name = "Lumbricina")
tree_species_map <- GB_map_number_species(species_level_tab = tree_species_level_tab, subtitle_text = tree_subtitle, taxa_name = "Trees")

# marine taxa 
fish_species_map <- GB_map_number_species_marine_taxa(species_level_tab = fish_species_level_tab, subtitle_text = fish_subtitle, taxa_name = "Fish")
porifera_species_map <- GB_map_number_species_marine_taxa(species_level_tab = porifera_species_level_tab, subtitle_text = porifera_subtitle, taxa_name = "Porifera")
crusta_species_map <- GB_map_number_species_marine_taxa(species_level_tab = crusta_species_level_tab, subtitle_text = crusta_subtitle, taxa_name = "Crustacea")


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

# porifera
file_path = "./output/plots/GenBank_map_species_porifera.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(porifera_species_map, vp = vplayout(1:12, 1:12))
dev.off()

# crustacea
file_path = "./output/plots/GenBank_map_species_crustacea.pdf"
pdf(file=file_path, width = 20, height = 12)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout = grid::grid.layout(12, 12)))
vplayout <- function(x, y) grid::viewport(layout.pos.row = x, layout.pos.col = y, just = "left", width = unit(2, "npc"), height = unit(2, "npc"))
print(crusta_species_map, vp = vplayout(1:12, 1:12))
dev.off()



#### maps for Figure 2 ####
# d. GenBank_map_sequences_myers_taxa
GB_myers_result_map <- GB_map_number_sequence_synthesis(locality_table = myers_seq_loc_tab,
                                                        subtitle_text = paste("Total number of sequences :", sum(myers_seq_loc_tab$n_seq[1:27])),
                                                        taxa_name = "Myers taxa",
                                                        color_scale_mid = 4)
fig2_genbank_map_continental_myers <- GB_myers_result_map +
  theme(title = element_blank(), 
        # legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position = c(0.45, 0.12),
        text = element_text(size = 16)) +
  ggtitle(element_blank(), subtitle = element_blank())

# e. GenBank_map_sequences_non_myers_taxa
GB_non_myers_result_map <- GB_map_number_sequence_synthesis(locality_table = non_myers_seq_loc_tab,
                                                            subtitle_text = paste("Total number of sequences :", sum(non_myers_seq_loc_tab$n_seq[1:27])),
                                                            taxa_name = "non Myers taxa",
                                                            color_scale_mid = 4)
fig2_genbank_map_continental_other <- GB_non_myers_result_map +
  theme(title = element_blank(), 
        # legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position = c(0.45, 0.12),
        text = element_text(size = 16)) +
  ggtitle(element_blank(), subtitle = element_blank())

# c. GenBank_map_sequences_marine_taxa
GB_marine_result_map <- GB_map_number_sequence_marine_taxa_synthesis(locality_table = marine_seq_loc_tab,
                                                                     subtitle_text = paste("Total number of sequences :", sum(marine_seq_loc_tab$n_seq[1:19])),
                                                                     taxa_name = "marine taxa",
                                                                     color_scale_mid = 4)
fig2_genbank_map_marine <- GB_marine_result_map +
  theme(title = element_blank(), 
        # legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position = c(0.29, 0.12),
        text = element_text(size = 16)) +
  ggtitle(element_blank(), subtitle = element_blank())


#### maps for Figure 4 ####
GB_terrestrial_evenness_map <- GB_map_number_sequence_evenness(locality_table = terrestrial_seq_evenness_tab,
                                                               subtitle_text = paste(""),
                                                               taxa_name = "Terrestrial taxa")
fig4_genbank_map_evenness_continental <- GB_terrestrial_evenness_map +
  scale_fill_gradientn(name = "Simpson evenness index", colours = palette_evenness, na.value = "white", breaks = c(0.3, 0.5, 0.7)) +
  theme(title = element_blank(), 
        # legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position = c(0.45, 0.12),
        text = element_text(size = 16)) +
  ggtitle(element_blank(), subtitle = element_blank())


GB_marine_evenness_map <- GB_map_number_sequence_marine_taxa_evenness(locality_table = marine_seq_evenness_tab,
                                                                      subtitle_text = paste(""),
                                                                      taxa_name = "Marine taxa")
fig4_genbank_map_evenness_marine <- GB_marine_evenness_map +
  theme(title = element_blank(), 
        # legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position = c(0.29, 0.12),
        text = element_text(size = 16)) +
  ggtitle(element_blank(), subtitle = element_blank())


