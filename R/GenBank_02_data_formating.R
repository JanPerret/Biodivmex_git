#################################################
#
# Biodivmex project
#
# GenBank_02_data_formating.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### load data
fungi_data <- read_delim("./data/TEST_v11_SIMPLE_RESULT_GENBANK_v10_fungi_references.csv", delim = ",", col_names = TRUE,
                         col_types = cols(
                           access_num = col_character(),
                           mol_type = col_character(),
                           taxa = col_character(),
                           species = col_character(),
                           gene = col_character(),
                           year = col_integer(),
                           sequencer_nationality = col_character(),
                           sample_origin = col_character(),
                           supp_origin_infos = col_character(),
                           species_level = col_character(),
                           fish = col_character(),
                           sponge = col_character(),
                           crustacea = col_character(),
                           tree = col_character()
                         ))

plant_data <- read_delim("./data/TEST_v11_SIMPLE_RESULT_GENBANK_v10_plant_references.csv", delim = ",", col_names = TRUE,
                         col_types = cols(
                           access_num = col_character(),
                           mol_type = col_character(),
                           taxa = col_character(),
                           species = col_character(),
                           gene = col_character(),
                           year = col_integer(),
                           sequencer_nationality = col_character(),
                           sample_origin = col_character(),
                           supp_origin_infos = col_character(),
                           species_level = col_character(),
                           fish = col_character(),
                           sponge = col_character(),
                           crustacea = col_character(),
                           tree = col_character()
                         ))

metazoa_data <- read_delim("./data/TEST_v11_SIMPLE_RESULT_GENBANK_v10_animal_references_mitochondrial_nuclear_seq.csv", delim = ",", col_names = TRUE,
                           col_types = cols(
                             access_num = col_character(),
                             mol_type = col_character(),
                             taxa = col_character(),
                             species = col_character(),
                             gene = col_character(),
                             year = col_integer(),
                             sequencer_nationality = col_character(),
                             sample_origin = col_character(),
                             supp_origin_infos = col_character(),
                             species_level = col_character(),
                             fish = col_character(),
                             sponge = col_character(),
                             crustacea = col_character(),
                             tree = col_character()
                           ))

# load list of mediterranean countries and islands
med_countries_list <- read_delim("./data/name_list_med_countries_and_islands_GenBank.csv", delim = ",", col_names = FALSE)
med_countries_list <- med_countries_list$X1

### correct minor data errors
# drop marine taxa columns for plants and fungi
fungi_data <- fungi_data %>%
  select(-fish, -sponge, -crustacea, -tree)
plant_data <- plant_data %>%
  select(-fish, -sponge, -crustacea)
metazoa_data <- metazoa_data %>%
  select(-tree)

# replace species_levels values "BUG" by NAs
# "BUG" indicates that the field that contains the species name 
# is in an unusual format and that the species name couldn't be extracted
fungi_data <- fungi_data %>% 
  mutate(species_level = replace(species_level, species_level == "BUG", NA))
plant_data <- plant_data %>% 
  mutate(species_level = replace(species_level, species_level == "BUG", NA))
metazoa_data <- metazoa_data %>% 
  mutate(species_level = replace(species_level, species_level == "BUG", NA))

# replace species_levels values which contain digits by NAs
fungi_data <- fungi_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[:digit:]"), NA))
plant_data <- plant_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[:digit:]"), NA))
metazoa_data <- metazoa_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[:digit:]"), NA))

# remove the single quotes of the field species_level
fungi_data$species_level <- str_replace_all(fungi_data$species_level, "'", "")
plant_data$species_level <- str_replace_all(plant_data$species_level, "'", "")
metazoa_data$species_level <- str_replace_all(metazoa_data$species_level, "'", "")

# replace species_levels values which contain punctuation signs other than "-" by NAs
fungi_data <- fungi_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[^-[:^punct:]]"), NA)) # you can exclude "-" sign from the POSIX class punct using a double negative like this : [^-[:^punct:]]
plant_data <- plant_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[^-[:^punct:]]"), NA))
metazoa_data <- metazoa_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[^-[:^punct:]]"), NA))

# reassign country names for Palestine
# remove "State of Palestine" name
fungi_data <- fungi_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "State of Palestine", "Palestine"))
plant_data <- plant_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "State of Palestine", "Palestine"))
metazoa_data <- metazoa_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "State of Palestine", "Palestine"))

# remove "West Bank" name
fungi_data <- fungi_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "West Bank", "Palestine"))
plant_data <- plant_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "West Bank", "Palestine"))
metazoa_data <- metazoa_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "West Bank", "Palestine"))

# remove "Gaza Strip" name
fungi_data <- fungi_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "Gaza Strip", "Palestine"))
plant_data <- plant_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "Gaza Strip", "Palestine"))
metazoa_data <- metazoa_data %>% 
  mutate(sample_origin = replace(sample_origin, sample_origin == "Gaza Strip", "Palestine"))


### create sub-data frame without nuclear genes (only used for marine taxa)
nuc_gene_names <- c("18S rRNA", "28S rRNA", "5.8S rRNA", "ITS", "Rag 2")

# save data frame with nuclear sequences for marine taxa
metazoa_data_with_nuc <- metazoa_data

# data frame for terrestrial taxa
metazoa_data <- metazoa_data %>% 
  filter(!str_detect(gene, paste(nuc_gene_names, sep = "|")))


### make sub-data frames for the mediterranean basin and each animal taxa
# make a sub-data frame for each animal taxa in the WORLD
amph_data <- metazoa_data %>% filter(taxa == "Amphibian")
rept_data <- metazoa_data %>% filter(taxa == "Reptile")
bird_data <- metazoa_data %>% filter(taxa == "Bird")
mammal_data <- metazoa_data %>% filter(taxa == "Mammal")
coleo_data <- metazoa_data %>% filter(taxa == "Coleoptera")
lumbri_data <- metazoa_data %>% filter(taxa == "Lumbricina")
papilio_data <- metazoa_data %>% filter(taxa == "Papilionoidea")
sponge_data <- metazoa_data_with_nuc %>% filter(taxa == "Sponge")
crusta_data <- metazoa_data_with_nuc %>% filter(taxa == "Crustacea")
fish_data <- metazoa_data_with_nuc %>% filter(taxa == "Fish")

# make a sub-data frame for each animal taxa in the MEDITERRANEAN BASIN
# filter dataframes with med countries
plant_data_med <- plant_data %>% filter(sample_origin %in% med_countries_list)
fungi_data_med <- fungi_data %>% filter(sample_origin %in% med_countries_list)
metazoa_data_med <- metazoa_data %>% filter(sample_origin %in% med_countries_list)
metazoa_data_with_nuc_med <- metazoa_data_with_nuc %>% filter(sample_origin %in% med_countries_list)

### add column "sequencer_loc" with local / mediterranean / distant sequencer information
plant_data_med <- GB_generate_sequencer_loc(plant_data_med)
fungi_data_med <- GB_generate_sequencer_loc(fungi_data_med)
metazoa_data_med <- GB_generate_sequencer_loc(metazoa_data_med)
metazoa_data_with_nuc_med <- GB_generate_sequencer_loc(metazoa_data_with_nuc_med)

# make sub-data frames
amph_data_med <- metazoa_data_med %>% filter(taxa == "Amphibian")
rept_data_med <- metazoa_data_med %>% filter(taxa == "Reptile")
bird_data_med <- metazoa_data_med %>% filter(taxa == "Bird")
mammal_data_med <- metazoa_data_med %>% filter(taxa == "Mammal")
coleo_data_med <- metazoa_data_med %>% filter(taxa == "Coleoptera")
lumbri_data_med <- metazoa_data_med %>% filter(taxa == "Lumbricina")
papilio_data_med <- metazoa_data_med %>% filter(taxa == "Papilionoidea")

# for marine taxa we take sequences assignated to taxonomic group with mediterranean sea species list and not GenBank's taxonomical hierarchy
sponge_data_med <- metazoa_data_with_nuc_med %>% filter(sponge == "sponge")
crusta_data_med <- metazoa_data_with_nuc_med %>% filter(crustacea == "crustacea")
fish_data_med <- metazoa_data_with_nuc_med %>% filter(fish == "fish")

# same thing for trees (sequences assignation to this group is done with a species name list and not GenBank's taxonomical hierarchy)
tree_data_med <- plant_data_med %>% filter(tree == "tree")

### extract general information for each taxa
plant_desc_tab <- GB_extract_general_info(kingdom_data = plant_data, taxa_data = plant_data, taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_desc_tab <- GB_extract_general_info(kingdom_data = fungi_data, taxa_data = fungi_data, taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = amph_data, taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = rept_data, taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = bird_data, taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = mammal_data, taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = coleo_data, taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
lumbri_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = lumbri_data, taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
papilio_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = papilio_data, taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
sponge_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data_with_nuc, taxa_data = sponge_data, taxa_data_med = sponge_data_med, taxa_name = "porifera")
crusta_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data_with_nuc, taxa_data = crusta_data, taxa_data_med = crusta_data_med, taxa_name = "crustacea")
fish_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data_with_nuc, taxa_data = fish_data, taxa_data_med = fish_data_med, taxa_name = "fish")
tree_desc_tab <- GB_extract_general_info(kingdom_data = plant_data, taxa_data = plant_data, taxa_data_med = tree_data_med, taxa_name = "tree")
tree_desc_tab[,3:4] <- NA # removing irrelevant informations : here we have no way to know taxa number of sequences outside of med region or the loc_rate

# make recap table
all_taxa_desc_tab <- rbind(plant_desc_tab, fungi_desc_tab, amph_desc_tab, rept_desc_tab, bird_desc_tab, mammal_desc_tab,
                           coleo_desc_tab, lumbri_desc_tab, papilio_desc_tab, sponge_desc_tab, crusta_desc_tab, fish_desc_tab,
                           tree_desc_tab)
# save table
write_csv2(all_taxa_desc_tab, path = "./output/text/GenBank_all_taxa_descriptive_mesures.csv", col_names = TRUE)


### extract these general information per year
plant_year_tab <- GB_loop_over_years(kingdom_data = plant_data, taxa_data = plant_data, taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_year_tab <- GB_loop_over_years(kingdom_data = fungi_data, taxa_data = fungi_data, taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = amph_data, taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = rept_data, taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = bird_data, taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = mammal_data, taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = coleo_data, taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
lumbri_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = lumbri_data, taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
papilio_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = papilio_data, taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
sponge_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data_with_nuc, taxa_data = sponge_data, taxa_data_med = sponge_data_med, taxa_name = "porifera")
crusta_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data_with_nuc, taxa_data = crusta_data, taxa_data_med = crusta_data_med, taxa_name = "crustacea")
fish_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data_with_nuc, taxa_data = fish_data, taxa_data_med = fish_data_med, taxa_name = "fish")
tree_year_tab <- GB_loop_over_years(kingdom_data = plant_data, taxa_data = plant_data, taxa_data_med = tree_data_med, taxa_name = "tree")
tree_year_tab[,3:4] <- NA # removing irrelevant informations : here we have no way to know taxa number of sequences outside of med region or the loc_rate


# save tables
write_csv2(plant_year_tab, path = "./output/text/GenBank_plant_year_tab.csv", col_names = TRUE)
write_csv2(fungi_year_tab, path = "./output/text/GenBank_fungi_year_tab.csv", col_names = TRUE)
write_csv2(amph_year_tab, path = "./output/text/GenBank_amph_year_tab.csv", col_names = TRUE)
write_csv2(rept_year_tab, path = "./output/text/GenBank_rept_year_tab.csv", col_names = TRUE)
write_csv2(bird_year_tab, path = "./output/text/GenBank_bird_year_tab.csv", col_names = TRUE)
write_csv2(mammal_year_tab, path = "./output/text/GenBank_mammal_year_tab.csv", col_names = TRUE)
write_csv2(coleo_year_tab, path = "./output/text/GenBank_coleo_year_tab.csv", col_names = TRUE)
write_csv2(lumbri_year_tab, path = "./output/text/GenBank_lumbri_year_tab.csv", col_names = TRUE)
write_csv2(papilio_year_tab, path = "./output/text/GenBank_papilio_year_tab.csv", col_names = TRUE)
write_csv2(sponge_year_tab, path = "./output/text/GenBank_sponge_year_tab.csv", col_names = TRUE)
write_csv2(crusta_year_tab, path = "./output/text/GenBank_crusta_year_tab.csv", col_names = TRUE)
write_csv2(fish_year_tab, path = "./output/text/GenBank_fish_year_tab.csv", col_names = TRUE)
write_csv2(tree_year_tab, path = "./output/text/GenBank_tree_year_tab.csv", col_names = TRUE)


### recap tables for number of sequences for each gene in med region
# load gene name list
gene_list <- read_delim("./data/name_list_all_genes.csv", delim = ";", col_names = FALSE)
gene_list <- gene_list$X1

### extract number of sequences containing each gene
plant_gene_tab <- GB_extract_gene(taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_gene_tab <- GB_extract_gene(taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_gene_tab <- GB_extract_gene(taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_gene_tab <- GB_extract_gene(taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_gene_tab <- GB_extract_gene(taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_gene_tab <- GB_extract_gene(taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_gene_tab <- GB_extract_gene(taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
lumbri_gene_tab <- GB_extract_gene(taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
papilio_gene_tab <- GB_extract_gene(taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
sponge_gene_tab <- GB_extract_gene(taxa_data_med = sponge_data_med, taxa_name = "porifera")
crusta_gene_tab <- GB_extract_gene(taxa_data_med = crusta_data_med, taxa_name = "crustacea")
fish_gene_tab <- GB_extract_gene(taxa_data_med = fish_data_med, taxa_name = "fish")
tree_gene_tab <- GB_extract_gene(taxa_data_med = tree_data_med, taxa_name = "tree")

# make recap table in effectives
all_taxa_gene_tab <- rbind(plant_gene_tab, fungi_gene_tab, amph_gene_tab, rept_gene_tab, bird_gene_tab, mammal_gene_tab,
                           coleo_gene_tab, lumbri_gene_tab, papilio_gene_tab, sponge_gene_tab, crusta_gene_tab, fish_gene_tab,
                           tree_gene_tab)

# recap table in percentage
all_taxa_gene_tab_percentage <- all_taxa_gene_tab[,-c(1, 27)] %>% sapply(`/`, all_taxa_gene_tab[,27])
all_taxa_gene_tab_percentage <- round(all_taxa_gene_tab_percentage, digits = 2)
all_taxa_gene_tab_percentage <- as.data.frame(cbind(taxa = all_taxa_gene_tab$taxa, all_taxa_gene_tab_percentage, tot_n_seq = all_taxa_gene_tab$tot_n_seq))

# save tables
write_csv2(all_taxa_gene_tab, path = "./output/text/GenBank_all_taxa_gene_tab.csv", col_names = TRUE)
write_csv2(all_taxa_gene_tab_percentage, path = "./output/text/GenBank_all_taxa_gene_tab_percentage.csv", col_names = TRUE)


# loop over years to have the number of sequences containing each gene for each year
plant_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
lumbri_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
papilio_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
sponge_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = sponge_data_med, taxa_name = "porifera")
crusta_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = crusta_data_med, taxa_name = "crustacea")
fish_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = fish_data_med, taxa_name = "fish")
tree_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = tree_data_med, taxa_name = "tree")

# save tables
write_csv2(plant_gene_year_tab, path = "./output/text/GenBank_plant_gene_year_tab.csv", col_names = TRUE)
write_csv2(fungi_gene_year_tab, path = "./output/text/GenBank_fungi_gene_year_tab.csv", col_names = TRUE)
write_csv2(amph_gene_year_tab, path = "./output/text/GenBank_amph_gene_year_tab.csv", col_names = TRUE)
write_csv2(rept_gene_year_tab, path = "./output/text/GenBank_rept_gene_year_tab.csv", col_names = TRUE)
write_csv2(bird_gene_year_tab, path = "./output/text/GenBank_bird_gene_year_tab.csv", col_names = TRUE)
write_csv2(mammal_gene_year_tab, path = "./output/text/GenBank_mammal_gene_year_tab.csv", col_names = TRUE)
write_csv2(coleo_gene_year_tab, path = "./output/text/GenBank_coleo_gene_year_tab.csv", col_names = TRUE)
write_csv2(lumbri_gene_year_tab, path = "./output/text/GenBank_lumbri_gene_year_tab.csv", col_names = TRUE)
write_csv2(papilio_gene_year_tab, path = "./output/text/GenBank_papilio_gene_year_tab.csv", col_names = TRUE)
write_csv2(sponge_gene_year_tab, path = "./output/text/GenBank_sponge_gene_year_tab.csv", col_names = TRUE)
write_csv2(crusta_gene_year_tab, path = "./output/text/GenBank_crusta_gene_year_tab.csv", col_names = TRUE)
write_csv2(fish_gene_year_tab, path = "./output/text/GenBank_fish_gene_year_tab.csv", col_names = TRUE)
write_csv2(tree_gene_year_tab, path = "./output/text/GenBank_tree_gene_year_tab.csv", col_names = TRUE)


### recap tables with sequencer localisation per taxa per country
# fix sample_origin factor levels for all taxa and give an explicit factor level to missing values to ensure that they appear in summaries and plots
sample_origin_med_levels <- c("Albania", "Algeria", "Balearic Islands", "Bosnia and Herzegovina", "Corsica", "Crete", "Croatia", "Cyprus", "Egypt", "France", 
                              "Greece", "Israel", "Italy", "Lebanon", "Libya", "Malta", "Montenegro", "Morocco", "Palestine", "Portugal", 
                              "Sardinia", "Sicily", "Slovenia", "Spain", "Syria", "Tunisia", "Turkey")

plant_data_med$sample_origin <- factor(plant_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
plant_data_med$sequencer_loc <- fct_explicit_na(plant_data_med$sequencer_loc)
fungi_data_med$sample_origin <- factor(fungi_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
fungi_data_med$sequencer_loc <- fct_explicit_na(fungi_data_med$sequencer_loc)
amph_data_med$sample_origin <- factor(amph_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
amph_data_med$sequencer_loc <- fct_explicit_na(amph_data_med$sequencer_loc)
rept_data_med$sample_origin <- factor(rept_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
rept_data_med$sequencer_loc <- fct_explicit_na(rept_data_med$sequencer_loc)
bird_data_med$sample_origin <- factor(bird_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
bird_data_med$sequencer_loc <- fct_explicit_na(bird_data_med$sequencer_loc)
mammal_data_med$sample_origin <- factor(mammal_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
mammal_data_med$sequencer_loc <- fct_explicit_na(mammal_data_med$sequencer_loc)
coleo_data_med$sample_origin <- factor(coleo_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
coleo_data_med$sequencer_loc <- fct_explicit_na(coleo_data_med$sequencer_loc)
lumbri_data_med$sample_origin <- factor(lumbri_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
lumbri_data_med$sequencer_loc <- fct_explicit_na(lumbri_data_med$sequencer_loc)
papilio_data_med$sample_origin <- factor(papilio_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
papilio_data_med$sequencer_loc <- fct_explicit_na(papilio_data_med$sequencer_loc)
sponge_data_med$sample_origin <- factor(sponge_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
sponge_data_med$sequencer_loc <- fct_explicit_na(sponge_data_med$sequencer_loc)
crusta_data_med$sample_origin <- factor(crusta_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
crusta_data_med$sequencer_loc <- fct_explicit_na(crusta_data_med$sequencer_loc)
fish_data_med$sample_origin <- factor(fish_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
fish_data_med$sequencer_loc <- fct_explicit_na(fish_data_med$sequencer_loc)
tree_data_med$sample_origin <- factor(tree_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
tree_data_med$sequencer_loc <- fct_explicit_na(tree_data_med$sequencer_loc)

# make recap tables
plant_seq_loc_tab <- plant_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
fungi_seq_loc_tab <- fungi_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
amph_seq_loc_tab <- amph_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
rept_seq_loc_tab <- rept_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
bird_seq_loc_tab <- bird_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
mammal_seq_loc_tab <- mammal_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
coleo_seq_loc_tab <- coleo_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
lumbri_seq_loc_tab <- lumbri_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
papilio_seq_loc_tab <- papilio_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
sponge_seq_loc_tab <- sponge_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
crusta_seq_loc_tab <- crusta_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
fish_seq_loc_tab <- fish_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
tree_seq_loc_tab <- tree_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)

# Nb : missing values in sequencer_loc are the consequence of a NA in sequencer_nationality, eg resulting from a typing error in the sequencer address

# save tables
write_csv2(plant_seq_loc_tab, path = "./output/text/GenBank_plant_seq_loc_tab.csv", col_names = TRUE)
write_csv2(fungi_seq_loc_tab, path = "./output/text/GenBank_fungi_seq_loc_tab.csv", col_names = TRUE)
write_csv2(amph_seq_loc_tab, path = "./output/text/GenBank_amph_seq_loc_tab.csv", col_names = TRUE)
write_csv2(rept_seq_loc_tab, path = "./output/text/GenBank_rept_seq_loc_tab.csv", col_names = TRUE)
write_csv2(bird_seq_loc_tab, path = "./output/text/GenBank_bird_seq_loc_tab.csv", col_names = TRUE)
write_csv2(mammal_seq_loc_tab, path = "./output/text/GenBank_mammal_seq_loc_tab.csv", col_names = TRUE)
write_csv2(coleo_seq_loc_tab, path = "./output/text/GenBank_coleo_seq_loc_tab.csv", col_names = TRUE)
write_csv2(lumbri_seq_loc_tab, path = "./output/text/GenBank_lumbri_seq_loc_tab.csv", col_names = TRUE)
write_csv2(papilio_seq_loc_tab, path = "./output/text/GenBank_papilio_seq_loc_tab.csv", col_names = TRUE)
write_csv2(sponge_seq_loc_tab, path = "./output/text/GenBank_sponge_seq_loc_tab.csv", col_names = TRUE)
write_csv2(crusta_seq_loc_tab, path = "./output/text/GenBank_crusta_seq_loc_tab.csv", col_names = TRUE)
write_csv2(fish_seq_loc_tab, path = "./output/text/GenBank_fish_seq_loc_tab.csv", col_names = TRUE)
write_csv2(tree_seq_loc_tab, path = "./output/text/GenBank_tree_seq_loc_tab.csv", col_names = TRUE)


### recap tables with proportion of sequences assigned at least at species level per country for each taxa
plant_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
lumbri_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
papilio_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
sponge_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = sponge_data_med, taxa_name = "porifera")
crusta_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = crusta_data_med, taxa_name = "crustacea")
fish_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = fish_data_med, taxa_name = "fish")
tree_species_level_tab <- GB_recap_species_level_rate(taxa_data_med = tree_data_med, taxa_name = "tree")

# save tables
write_csv2(plant_species_level_tab, path = "./output/text/GenBank_plant_species_level_tab.csv", col_names = TRUE)
write_csv2(fungi_species_level_tab, path = "./output/text/GenBank_fungi_species_level_tab.csv", col_names = TRUE)
write_csv2(amph_species_level_tab, path = "./output/text/GenBank_amph_species_level_tab.csv", col_names = TRUE)
write_csv2(rept_species_level_tab, path = "./output/text/GenBank_rept_species_level_tab.csv", col_names = TRUE)
write_csv2(bird_species_level_tab, path = "./output/text/GenBank_bird_species_level_tab.csv", col_names = TRUE)
write_csv2(mammal_species_level_tab, path = "./output/text/GenBank_mammal_species_level_tab.csv", col_names = TRUE)
write_csv2(coleo_species_level_tab, path = "./output/text/GenBank_coleo_species_level_tab.csv", col_names = TRUE)
write_csv2(lumbri_species_level_tab, path = "./output/text/GenBank_lumbri_species_level_tab.csv", col_names = TRUE)
write_csv2(papilio_species_level_tab, path = "./output/text/GenBank_papilio_species_level_tab.csv", col_names = TRUE)
write_csv2(sponge_species_level_tab, path = "./output/text/GenBank_sponge_species_level_tab.csv", col_names = TRUE)
write_csv2(crusta_species_level_tab, path = "./output/text/GenBank_crusta_species_level_tab.csv", col_names = TRUE)
write_csv2(fish_species_level_tab, path = "./output/text/GenBank_fish_species_level_tab.csv", col_names = TRUE)
write_csv2(tree_species_level_tab, path = "./output/text/GenBank_tree_species_level_tab.csv", col_names = TRUE)


### recap tables with number of different species (or species names) per country for each taxa
plant_number_species_tab <- GB_recap_number_species(taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_number_species_tab <- GB_recap_number_species(taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_number_species_tab <- GB_recap_number_species(taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_number_species_tab <- GB_recap_number_species(taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_number_species_tab <- GB_recap_number_species(taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_number_species_tab <- GB_recap_number_species(taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_number_species_tab <- GB_recap_number_species(taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
lumbri_number_species_tab <- GB_recap_number_species(taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
papilio_number_species_tab <- GB_recap_number_species(taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
sponge_number_species_tab <- GB_recap_number_species(taxa_data_med = sponge_data_med, taxa_name = "porifera")
crusta_number_species_tab <- GB_recap_number_species(taxa_data_med = crusta_data_med, taxa_name = "crustacea")
fish_number_species_tab <- GB_recap_number_species(taxa_data_med = fish_data_med, taxa_name = "fish")
tree_number_species_tab <- GB_recap_number_species(taxa_data_med = tree_data_med, taxa_name = "tree")

# save tables
write_csv2(plant_number_species_tab, path = "./output/text/GenBank_plant_number_species_tab.csv", col_names = TRUE)
write_csv2(fungi_number_species_tab, path = "./output/text/GenBank_fungi_number_species_tab.csv", col_names = TRUE)
write_csv2(amph_number_species_tab, path = "./output/text/GenBank_amph_number_species_tab.csv", col_names = TRUE)
write_csv2(rept_number_species_tab, path = "./output/text/GenBank_rept_number_species_tab.csv", col_names = TRUE)
write_csv2(bird_number_species_tab, path = "./output/text/GenBank_bird_number_species_tab.csv", col_names = TRUE)
write_csv2(mammal_number_species_tab, path = "./output/text/GenBank_mammal_number_species_tab.csv", col_names = TRUE)
write_csv2(coleo_number_species_tab, path = "./output/text/GenBank_coleo_number_species_tab.csv", col_names = TRUE)
write_csv2(lumbri_number_species_tab, path = "./output/text/GenBank_lumbri_number_species_tab.csv", col_names = TRUE)
write_csv2(papilio_number_species_tab, path = "./output/text/GenBank_papilio_number_species_tab.csv", col_names = TRUE)
write_csv2(sponge_number_species_tab, path = "./output/text/GenBank_sponge_number_species_tab.csv", col_names = TRUE)
write_csv2(crusta_number_species_tab, path = "./output/text/GenBank_crusta_number_species_tab.csv", col_names = TRUE)
write_csv2(fish_number_species_tab, path = "./output/text/GenBank_fish_number_species_tab.csv", col_names = TRUE)
write_csv2(tree_number_species_tab, path = "./output/text/GenBank_tree_number_species_tab.csv", col_names = TRUE)


### recap tables number of sequences per year per country for each taxa
# fix year factor levels for all taxa
year_list <- c(1987:2019)

plant_data_med$year <- factor(plant_data_med$year, levels = year_list, ordered = TRUE)
fungi_data_med$year <- factor(fungi_data_med$year, levels = year_list, ordered = FALSE)
amph_data_med$year <- factor(amph_data_med$year, levels = year_list, ordered = FALSE)
rept_data_med$year <- factor(rept_data_med$year, levels = year_list, ordered = FALSE)
bird_data_med$year <- factor(bird_data_med$year, levels = year_list, ordered = FALSE)
mammal_data_med$year <- factor(mammal_data_med$year, levels = year_list, ordered = FALSE)
coleo_data_med$year <- factor(coleo_data_med$year, levels = year_list, ordered = FALSE)
lumbri_data_med$year <- factor(lumbri_data_med$year, levels = year_list, ordered = FALSE)
papilio_data_med$year <- factor(papilio_data_med$year, levels = year_list, ordered = FALSE)
sponge_data_med$year <- factor(sponge_data_med$year, levels = year_list, ordered = FALSE)
crusta_data_med$year <- factor(crusta_data_med$year, levels = year_list, ordered = FALSE)
fish_data_med$year <- factor(fish_data_med$year, levels = year_list, ordered = FALSE)
tree_data_med$year <- factor(tree_data_med$year, levels = year_list, ordered = FALSE)

plant_year_tab <- plant_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
fungi_year_tab <- fungi_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
amph_year_tab <- amph_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
rept_year_tab <- rept_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
bird_year_tab <- bird_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
mammal_year_tab <- mammal_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
coleo_year_tab <- coleo_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
lumbri_year_tab <- lumbri_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
papilio_year_tab <- papilio_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
sponge_year_tab <- sponge_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
crusta_year_tab <- crusta_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
fish_year_tab <- fish_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
tree_year_tab <- tree_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)

# save tables
write_csv2(plant_year_tab, path = "./output/text/GenBank_plant_year_tab.csv", col_names = TRUE)
write_csv2(fungi_year_tab, path = "./output/text/GenBank_fungi_year_tab.csv", col_names = TRUE)
write_csv2(amph_year_tab, path = "./output/text/GenBank_amph_year_tab.csv", col_names = TRUE)
write_csv2(rept_year_tab, path = "./output/text/GenBank_rept_year_tab.csv", col_names = TRUE)
write_csv2(bird_year_tab, path = "./output/text/GenBank_bird_year_tab.csv", col_names = TRUE)
write_csv2(mammal_year_tab, path = "./output/text/GenBank_mammal_year_tab.csv", col_names = TRUE)
write_csv2(coleo_year_tab, path = "./output/text/GenBank_coleo_year_tab.csv", col_names = TRUE)
write_csv2(lumbri_year_tab, path = "./output/text/GenBank_lumbri_year_tab.csv", col_names = TRUE)
write_csv2(papilio_year_tab, path = "./output/text/GenBank_papilio_year_tab.csv", col_names = TRUE)
write_csv2(sponge_year_tab, path = "./output/text/GenBank_sponge_year_tab.csv", col_names = TRUE)
write_csv2(crusta_year_tab, path = "./output/text/GenBank_crusta_year_tab.csv", col_names = TRUE)
write_csv2(fish_year_tab, path = "./output/text/GenBank_fish_year_tab.csv", col_names = TRUE)
write_csv2(tree_year_tab, path = "./output/text/GenBank_tree_year_tab.csv", col_names = TRUE)

### same table but for accumulation curve

plant_year_tab_acc <- GB_accumulate_over_years(year_tab = plant_year_tab)
fungi_year_tab_acc <- GB_accumulate_over_years(year_tab = fungi_year_tab)
amph_year_tab_acc <- GB_accumulate_over_years(year_tab = amph_year_tab)
rept_year_tab_acc <- GB_accumulate_over_years(year_tab = rept_year_tab)
bird_year_tab_acc <- GB_accumulate_over_years(year_tab = bird_year_tab)
mammal_year_tab_acc <- GB_accumulate_over_years(year_tab = mammal_year_tab)
coleo_year_tab_acc <- GB_accumulate_over_years(year_tab = coleo_year_tab)
lumbri_year_tab_acc <- GB_accumulate_over_years(year_tab = lumbri_year_tab)
papilio_year_tab_acc <- GB_accumulate_over_years(year_tab = papilio_year_tab)
sponge_year_tab_acc <- GB_accumulate_over_years(year_tab = sponge_year_tab)
crusta_year_tab_acc <- GB_accumulate_over_years(year_tab = crusta_year_tab)
fish_year_tab_acc <- GB_accumulate_over_years(year_tab = fish_year_tab)
tree_year_tab_acc <- GB_accumulate_over_years(year_tab = tree_year_tab)

# save tables
write_csv2(plant_year_tab_acc, path = "./output/text/GenBank_plant_year_tab_acc.csv", col_names = TRUE)
write_csv2(fungi_year_tab_acc, path = "./output/text/GenBank_fungi_year_tab_acc.csv", col_names = TRUE)
write_csv2(amph_year_tab_acc, path = "./output/text/GenBank_amph_year_tab_acc.csv", col_names = TRUE)
write_csv2(rept_year_tab_acc, path = "./output/text/GenBank_rept_year_tab_acc.csv", col_names = TRUE)
write_csv2(bird_year_tab_acc, path = "./output/text/GenBank_bird_year_tab_acc.csv", col_names = TRUE)
write_csv2(mammal_year_tab_acc, path = "./output/text/GenBank_mammal_year_tab_acc.csv", col_names = TRUE)
write_csv2(coleo_year_tab_acc, path = "./output/text/GenBank_coleo_year_tab_acc.csv", col_names = TRUE)
write_csv2(lumbri_year_tab_acc, path = "./output/text/GenBank_lumbri_year_tab_acc.csv", col_names = TRUE)
write_csv2(papilio_year_tab_acc, path = "./output/text/GenBank_papilio_year_tab_acc.csv", col_names = TRUE)
write_csv2(sponge_year_tab_acc, path = "./output/text/GenBank_sponge_year_tab_acc.csv", col_names = TRUE)
write_csv2(crusta_year_tab_acc, path = "./output/text/GenBank_crusta_year_tab_acc.csv", col_names = TRUE)
write_csv2(fish_year_tab_acc, path = "./output/text/GenBank_fish_year_tab_acc.csv", col_names = TRUE)
write_csv2(tree_year_tab_acc, path = "./output/text/GenBank_tree_year_tab_acc.csv", col_names = TRUE)

