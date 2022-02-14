#################################################
#
# Biodivmex project
#
# GenBank_02_data_formating.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### load data
fungi_data <- read_delim("./data/SIMPLE_v11_RESULT_GENBANK_v10_fungi_references.csv", delim = ",", col_names = TRUE,
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
                           porifera = col_character(),
                           crustacea = col_character(),
                           tree = col_character()
                         ))

plant_data <- read_delim("./data/SIMPLE_v11_RESULT_GENBANK_v10_plant_references.csv", delim = ",", col_names = TRUE,
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
                           porifera = col_character(),
                           crustacea = col_character(),
                           tree = col_character()
                         ))

metazoa_data <- read_delim("./data/SIMPLE_v11_RESULT_GENBANK_v10_animal_references_mitochondrial_nuclear_seq.csv", delim = ",", col_names = TRUE,
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
                             porifera = col_character(),
                             crustacea = col_character(),
                             tree = col_character()
                           ))

# load list of mediterranean countries and islands
med_countries_list <- read_delim("./data/name_list_med_countries_and_islands_GenBank.csv", delim = ",", col_names = FALSE)
med_countries_list <- med_countries_list$X1

### correct minor data errors
# drop marine taxa columns for plants and fungi
fungi_data <- fungi_data %>%
  select(-fish, -porifera, -crustacea, -tree)
plant_data <- plant_data %>%
  select(-fish, -porifera, -crustacea)
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

# replace species_levels values which contain digits by NA
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

# remove sequences from 2021
fungi_data <- subset(fungi_data, fungi_data$year != 2021)
plant_data <- subset(plant_data, plant_data$year != 2021)
metazoa_data <- subset(metazoa_data, metazoa_data$year != 2021)

# remove sequences for wich no gene could be assigned
fungi_data <- subset(fungi_data, !is.na(fungi_data$gene))
plant_data <- subset(plant_data, !is.na(plant_data$gene))
metazoa_data <- subset(metazoa_data, !is.na(metazoa_data$gene))


### number of sequences that passed "content filtering":
number_seq_passing_content_filtering <- nrow(fungi_data) + nrow(plant_data) + nrow(metazoa_data)
# 7,050,591

### create sub-data frame without nuclear genes (only used for marine taxa)
# save data frame with nuclear sequences for marine taxa
metazoa_data_with_nuc <- metazoa_data

# list of nuclear markers
nuc_gene_names <- c("18S rRNA|28S rRNA|5.8S rRNA|ITS|Rag 2")

# remove from the table sequences of these markers
metazoa_data <- metazoa_data %>%
  filter(!grepl(pattern = nuc_gene_names, x = gene))

# sample_origin fill rate through the years for METAZOA
metazoa_sample_origin_fill_rate <- metazoa_data_with_nuc %>%
                                  group_by(year) %>% 
                                  count()

n_empty <- metazoa_data_with_nuc %>%
                filter(is.na(sample_origin)) %>% 
                group_by(year) %>% 
                count()

metazoa_sample_origin_fill_rate <- cbind(metazoa_sample_origin_fill_rate, empty_origin = n_empty$n)
colnames(metazoa_sample_origin_fill_rate) <- c("year", "n_seq", "empty_origin")
metazoa_sample_origin_fill_rate <- cbind(metazoa_sample_origin_fill_rate, 
                                     fill_rate = round(1 - metazoa_sample_origin_fill_rate$empty_origin/metazoa_sample_origin_fill_rate$n_seq, digits = 2))

# sample_origin fill rate through the years for PLANTS
plant_sample_origin_fill_rate <- plant_data %>%
  group_by(year) %>% 
  count()

n_empty <- plant_data %>%
  filter(is.na(sample_origin)) %>% 
  group_by(year) %>% 
  count()

plant_sample_origin_fill_rate <- cbind(plant_sample_origin_fill_rate, empty_origin = n_empty$n)
colnames(plant_sample_origin_fill_rate) <- c("year", "n_seq", "empty_origin")
plant_sample_origin_fill_rate <- cbind(plant_sample_origin_fill_rate, 
                                         fill_rate = round(1 - plant_sample_origin_fill_rate$empty_origin/plant_sample_origin_fill_rate$n_seq, digits = 2))

# sample_origin fill rate through the years for FUNGI
fungi_sample_origin_fill_rate <- fungi_data %>%
  group_by(year) %>% 
  count()

n_empty <- fungi_data %>%
  filter(is.na(sample_origin)) %>% 
  group_by(year) %>% 
  count()

fungi_sample_origin_fill_rate <- cbind(fungi_sample_origin_fill_rate, empty_origin = n_empty$n)
colnames(fungi_sample_origin_fill_rate) <- c("year", "n_seq", "empty_origin")
fungi_sample_origin_fill_rate <- cbind(fungi_sample_origin_fill_rate, 
                                         fill_rate = round(1 - fungi_sample_origin_fill_rate$empty_origin/fungi_sample_origin_fill_rate$n_seq, digits = 2))

# save the tables
write_csv2(metazoa_sample_origin_fill_rate, file = "./output/text/GenBank_metazoa_sample_origin_fill_rate.csv", col_names = TRUE)
write_csv2(plant_sample_origin_fill_rate, file = "./output/text/GenBank_plant_sample_origin_fill_rate.csv", col_names = TRUE)
write_csv2(fungi_sample_origin_fill_rate, file = "./output/text/GenBank_fungi_sample_origin_fill_rate.csv", col_names = TRUE)


### make sub-data frames for the mediterranean basin and each animal taxa
# make a sub-data frame for each animal taxa in the WORLD
amph_data <- metazoa_data %>% filter(taxa == "Amphibian")
rept_data <- metazoa_data %>% filter(taxa == "Reptile")
bird_data <- metazoa_data %>% filter(taxa == "Bird")
mammal_data <- metazoa_data %>% filter(taxa == "Mammal")
coleo_data <- metazoa_data %>% filter(taxa == "Coleoptera")
papilio_data <- metazoa_data %>% filter(taxa == "Papilionoidea")
lumbri_data <- metazoa_data %>% filter(taxa == "Lumbricina")
fish_data <- metazoa_data_with_nuc %>% filter(taxa == "Fish")
porifera_data <- metazoa_data_with_nuc %>% filter(taxa == "Porifera")
crusta_data <- metazoa_data_with_nuc %>% filter(taxa == "Crustacea")

# make a sub-data frame for each animal taxa in the MEDITERRANEAN BASIN
# filter dataframes with med countries
plant_data_med <- plant_data %>% filter(sample_origin %in% med_countries_list)
fungi_data_med <- fungi_data %>% filter(sample_origin %in% med_countries_list)
metazoa_data_med <- metazoa_data %>% filter(sample_origin %in% med_countries_list)
metazoa_data_with_nuc_med <- metazoa_data_with_nuc %>% filter(sample_origin %in% med_countries_list)


### get number of sequences at different steps of the filtering to make the filtering recap
terrestrial_taxa_list <- c("Amphibian", "Reptile", "Bird", "Mammal", "Coleoptera", "Papilionoidea") # no "Lumbricina"

# number of sequences assigned to a taxa of interest
n_seq_marine_taxa <- nrow(metazoa_data_with_nuc %>% filter(fish == "fish" | porifera == "porifera" | crustacea == "crustacea"))
n_seq_terrestrial_taxa <- nrow(metazoa_data %>% filter(taxa %in% terrestrial_taxa_list))
n_seq_taxa_of_interest <- nrow(fungi_data) + nrow(plant_data) + n_seq_marine_taxa + n_seq_terrestrial_taxa
# 2,860,073

# number of sequences assigned to a study area
n_seq_study_area <- nrow(fungi_data_med) + nrow(plant_data_med) + nrow(metazoa_data_with_nuc_med)
# 326,010

# number of sequences with at least one geographic and one taxonomic assignation
terrestrial_taxa_med_df <- metazoa_data_med %>% filter(taxa %in% terrestrial_taxa_list)
marine_taxa_med_df <- metazoa_data_with_nuc_med %>% filter(fish == "fish" | porifera == "porifera" | crustacea == "crustacea")

n_valid_seq <- nrow(fungi_data_med) + nrow(plant_data_med) + nrow(terrestrial_taxa_med_df) + nrow(marine_taxa_med_df)
# 175,828

# make recap table of the filtering steps
recap_titles <- c("Number of sequences that passed content filtering",
                  "Number of sequences assigned to a taxa of interest",
                  "Number of sequences assigned to a study area",
                  "Number of sequences assigned to both a taxa of interest and a study area")
num_seq <- c(number_seq_passing_content_filtering, n_seq_taxa_of_interest, n_seq_study_area, n_valid_seq) 
GenBank_filtering_recap_table <- data.frame(recap_titles, num_seq)
write_csv2(GenBank_filtering_recap_table, file = "./output/text/GenBank_filtering_recap.csv", col_names = TRUE)

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
papilio_data_med <- metazoa_data_med %>% filter(taxa == "Papilionoidea")
lumbri_data_med <- metazoa_data_med %>% filter(taxa == "Lumbricina")

# for marine taxa we take sequences assignated to taxonomic group with mediterranean sea species list and not GenBank's taxonomical hierarchy
fish_data_med <- metazoa_data_with_nuc_med %>% filter(fish == "fish")
porifera_data_med <- metazoa_data_with_nuc_med %>% filter(porifera == "porifera")
crusta_data_med <- metazoa_data_with_nuc_med %>% filter(crustacea == "crustacea")

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
papilio_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = papilio_data, taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
lumbri_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data, taxa_data = lumbri_data, taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
fish_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data_with_nuc, taxa_data = fish_data, taxa_data_med = fish_data_med, taxa_name = "fish")
porifera_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data_with_nuc, taxa_data = porifera_data, taxa_data_med = porifera_data_med, taxa_name = "porifera")
crusta_desc_tab <- GB_extract_general_info(kingdom_data = metazoa_data_with_nuc, taxa_data = crusta_data, taxa_data_med = crusta_data_med, taxa_name = "crustacea")
tree_desc_tab <- GB_extract_general_info(kingdom_data = plant_data, taxa_data = plant_data, taxa_data_med = tree_data_med, taxa_name = "tree")
tree_desc_tab[, 3:4] <- NA # removing irrelevant informations : here we have no way to know taxa number of sequences outside of med region or the loc_rate

# make recap table
all_taxa_desc_tab <- rbind(plant_desc_tab, fungi_desc_tab, amph_desc_tab, rept_desc_tab, bird_desc_tab, mammal_desc_tab,
                           coleo_desc_tab, papilio_desc_tab, lumbri_desc_tab, fish_desc_tab, porifera_desc_tab, crusta_desc_tab,
                           tree_desc_tab)
# save table
write_csv2(all_taxa_desc_tab, file = "./output/text/GenBank_all_taxa_desc_tab.csv", col_names = TRUE)


### extract these general information per year
plant_desc_year_tab <- GB_loop_over_years(kingdom_data = plant_data, taxa_data = plant_data, taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_desc_year_tab <- GB_loop_over_years(kingdom_data = fungi_data, taxa_data = fungi_data, taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = amph_data, taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = rept_data, taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = bird_data, taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = mammal_data, taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = coleo_data, taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
papilio_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = papilio_data, taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
lumbri_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data, taxa_data = lumbri_data, taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
fish_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data_with_nuc, taxa_data = fish_data, taxa_data_med = fish_data_med, taxa_name = "fish")
porifera_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data_with_nuc, taxa_data = porifera_data, taxa_data_med = porifera_data_med, taxa_name = "porifera")
crusta_desc_year_tab <- GB_loop_over_years(kingdom_data = metazoa_data_with_nuc, taxa_data = crusta_data, taxa_data_med = crusta_data_med, taxa_name = "crustacea")
tree_desc_year_tab <- GB_loop_over_years(kingdom_data = plant_data, taxa_data = plant_data, taxa_data_med = tree_data_med, taxa_name = "tree")
tree_desc_year_tab[, 3:4] <- NA # removing irrelevant informations : here we have no way to know taxa number of sequences outside of med region or the loc_rate

# save tables
write_csv2(plant_desc_year_tab, file = "./output/text/GenBank_plant_desc_year_tab.csv", col_names = TRUE)
write_csv2(fungi_desc_year_tab, file = "./output/text/GenBank_fungi_desc_year_tab.csv", col_names = TRUE)
write_csv2(amph_desc_year_tab, file = "./output/text/GenBank_amph_desc_year_tab.csv", col_names = TRUE)
write_csv2(rept_desc_year_tab, file = "./output/text/GenBank_rept_desc_year_tab.csv", col_names = TRUE)
write_csv2(bird_desc_year_tab, file = "./output/text/GenBank_bird_desc_year_tab.csv", col_names = TRUE)
write_csv2(mammal_desc_year_tab, file = "./output/text/GenBank_mammal_desc_year_tab.csv", col_names = TRUE)
write_csv2(coleo_desc_year_tab, file = "./output/text/GenBank_coleo_desc_year_tab.csv", col_names = TRUE)
write_csv2(papilio_desc_year_tab, file = "./output/text/GenBank_papilio_desc_year_tab.csv", col_names = TRUE)
write_csv2(lumbri_desc_year_tab, file = "./output/text/GenBank_lumbri_desc_year_tab.csv", col_names = TRUE)
write_csv2(fish_desc_year_tab, file = "./output/text/GenBank_fish_desc_year_tab.csv", col_names = TRUE)
write_csv2(porifera_desc_year_tab, file = "./output/text/GenBank_porifera_desc_year_tab.csv", col_names = TRUE)
write_csv2(crusta_desc_year_tab, file = "./output/text/GenBank_crusta_desc_year_tab.csv", col_names = TRUE)
write_csv2(tree_desc_year_tab, file = "./output/text/GenBank_tree_desc_year_tab.csv", col_names = TRUE)


### recap table with number of sequences through time for each taxa
# initiaze table
year_list <- c(1987:2020)
all_taxa_year_tab <- setNames(data.frame(matrix(ncol = length(year_list), nrow = 0)), c(paste0(year_list)))

# number of sequences in med basin per year
all_taxa_year_tab <- rbind(plant_desc_year_tab$n_seq_med,
                           fungi_desc_year_tab$n_seq_med,
                           amph_desc_year_tab$n_seq_med,
                           rept_desc_year_tab$n_seq_med,
                           bird_desc_year_tab$n_seq_med,
                           mammal_desc_year_tab$n_seq_med,
                           coleo_desc_year_tab$n_seq_med,
                           papilio_desc_year_tab$n_seq_med,
                           lumbri_desc_year_tab$n_seq_med,
                           fish_desc_year_tab$n_seq_med,
                           porifera_desc_year_tab$n_seq_med,
                           crusta_desc_year_tab$n_seq_med,
                           tree_desc_year_tab$n_seq_med
                           )

colnames(all_taxa_year_tab) <- c(as.character(paste0(year_list)))

# pass table to accumulation
all_taxa_year_tab_acc <- table_accumulate(year_tab = all_taxa_year_tab, first_column = 2)

# bind taxa names column to the tables
taxa_vect <- c("plant", "fungi", "amphibian", "reptile", "bird", "mammal", "coleoptera", "papilionoidea", "lumbricina", "fish", "porifera", "crustacea", "tree")
all_taxa_year_tab <- cbind(taxa = taxa_vect, all_taxa_year_tab)
all_taxa_year_tab <- as.data.frame(all_taxa_year_tab)
all_taxa_year_tab_acc <- cbind(taxa = taxa_vect, all_taxa_year_tab_acc)
all_taxa_year_tab_acc <- as.data.frame(all_taxa_year_tab_acc)

# save tables
write_csv2(all_taxa_year_tab, file = "./output/text/GenBank_all_taxa_year_tab.csv", col_names = TRUE)
write_csv2(all_taxa_year_tab_acc, file = "./output/text/GenBank_all_taxa_year_tab_acc.csv", col_names = TRUE)


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
papilio_gene_tab <- GB_extract_gene(taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
lumbri_gene_tab <- GB_extract_gene(taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
fish_gene_tab <- GB_extract_gene(taxa_data_med = fish_data_med, taxa_name = "fish")
porifera_gene_tab <- GB_extract_gene(taxa_data_med = porifera_data_med, taxa_name = "porifera")
crusta_gene_tab <- GB_extract_gene(taxa_data_med = crusta_data_med, taxa_name = "crustacea")
tree_gene_tab <- GB_extract_gene(taxa_data_med = tree_data_med, taxa_name = "tree")

# make recap table in effectives
all_taxa_gene_tab <- rbind(plant_gene_tab, fungi_gene_tab, amph_gene_tab, rept_gene_tab, bird_gene_tab, mammal_gene_tab,
                           coleo_gene_tab, papilio_gene_tab, lumbri_gene_tab, fish_gene_tab, porifera_gene_tab, crusta_gene_tab,
                           tree_gene_tab)

# recap table in percentage
all_taxa_gene_tab_percentage <- all_taxa_gene_tab[, -c(1, 27)] %>% sapply(`/`, all_taxa_gene_tab[, 27])
all_taxa_gene_tab_percentage <- round(all_taxa_gene_tab_percentage, digits = 2)
all_taxa_gene_tab_percentage <- as.data.frame(cbind(taxa = all_taxa_gene_tab$taxa, all_taxa_gene_tab_percentage, tot_n_seq = all_taxa_gene_tab$tot_n_seq))

# save tables
write_csv2(all_taxa_gene_tab, file = "./output/text/GenBank_all_taxa_gene_tab.csv", col_names = TRUE)
write_csv2(all_taxa_gene_tab_percentage, file = "./output/text/GenBank_all_taxa_gene_tab_percentage.csv", col_names = TRUE)


# loop over years to have the number of sequences containing each gene for each year
plant_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
papilio_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
lumbri_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
fish_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = fish_data_med, taxa_name = "fish")
porifera_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = porifera_data_med, taxa_name = "porifera")
crusta_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = crusta_data_med, taxa_name = "crustacea")
tree_gene_year_tab <- GB_gene_recap_loop_over_years(taxa_data_med = tree_data_med, taxa_name = "tree")

# save tables
write_csv2(plant_gene_year_tab, file = "./output/text/GenBank_plant_gene_year_tab.csv", col_names = TRUE)
write_csv2(fungi_gene_year_tab, file = "./output/text/GenBank_fungi_gene_year_tab.csv", col_names = TRUE)
write_csv2(amph_gene_year_tab, file = "./output/text/GenBank_amph_gene_year_tab.csv", col_names = TRUE)
write_csv2(rept_gene_year_tab, file = "./output/text/GenBank_rept_gene_year_tab.csv", col_names = TRUE)
write_csv2(bird_gene_year_tab, file = "./output/text/GenBank_bird_gene_year_tab.csv", col_names = TRUE)
write_csv2(mammal_gene_year_tab, file = "./output/text/GenBank_mammal_gene_year_tab.csv", col_names = TRUE)
write_csv2(coleo_gene_year_tab, file = "./output/text/GenBank_coleo_gene_year_tab.csv", col_names = TRUE)
write_csv2(papilio_gene_year_tab, file = "./output/text/GenBank_papilio_gene_year_tab.csv", col_names = TRUE)
write_csv2(lumbri_gene_year_tab, file = "./output/text/GenBank_lumbri_gene_year_tab.csv", col_names = TRUE)
write_csv2(fish_gene_year_tab, file = "./output/text/GenBank_fish_gene_year_tab.csv", col_names = TRUE)
write_csv2(porifera_gene_year_tab, file = "./output/text/GenBank_porifera_gene_year_tab.csv", col_names = TRUE)
write_csv2(crusta_gene_year_tab, file = "./output/text/GenBank_crusta_gene_year_tab.csv", col_names = TRUE)
write_csv2(tree_gene_year_tab, file = "./output/text/GenBank_tree_gene_year_tab.csv", col_names = TRUE)


### recap tables with sequencer localisation per taxa per country
# for marine taxa replace islands by their sovereign country
fish_data_med$sample_origin[fish_data_med$sample_origin == "Balearic Islands"] <- "Spain"
fish_data_med$sample_origin[fish_data_med$sample_origin == "Corsica"] <- "France"
fish_data_med$sample_origin[fish_data_med$sample_origin == "Crete"] <- "Greece"
fish_data_med$sample_origin[fish_data_med$sample_origin == "Sardinia"] <- "Italy"
fish_data_med$sample_origin[fish_data_med$sample_origin == "Sicily"] <- "Italy"

porifera_data_med$sample_origin[porifera_data_med$sample_origin == "Balearic Islands"] <- "Spain"
porifera_data_med$sample_origin[porifera_data_med$sample_origin == "Corsica"] <- "France"
porifera_data_med$sample_origin[porifera_data_med$sample_origin == "Crete"] <- "Greece"
porifera_data_med$sample_origin[porifera_data_med$sample_origin == "Sardinia"] <- "Italy"
porifera_data_med$sample_origin[porifera_data_med$sample_origin == "Sicily"] <- "Italy"

crusta_data_med$sample_origin[crusta_data_med$sample_origin == "Balearic Islands"] <- "Spain"
crusta_data_med$sample_origin[crusta_data_med$sample_origin == "Corsica"] <- "France"
crusta_data_med$sample_origin[crusta_data_med$sample_origin == "Crete"] <- "Greece"
crusta_data_med$sample_origin[crusta_data_med$sample_origin == "Sardinia"] <- "Italy"
crusta_data_med$sample_origin[crusta_data_med$sample_origin == "Sicily"] <- "Italy"

# for marine taxa remove countries without any part of their ZEE in the med sea and the islands
countries_to_remove <- c("Bosnia and Herzegovina", "Palestine", "Portugal")
fish_data_med <- fish_data_med %>% filter(!sample_origin %in% countries_to_remove)
porifera_data_med <- porifera_data_med %>% filter(!sample_origin %in% countries_to_remove)
crusta_data_med <- crusta_data_med %>% filter(!sample_origin %in% countries_to_remove)

# fix sample_origin factor levels for all taxa and give an explicit factor level to missing values to ensure that they appear in summaries and plots
sample_origin_med_levels <- c("Albania", "Algeria", "Balearic Islands", "Bosnia and Herzegovina", "Corsica", "Crete", "Croatia", "Cyprus", "Egypt", "France", 
                              "Greece", "Israel", "Italy", "Lebanon", "Libya", "Malta", "Montenegro", "Morocco", "Palestine", "Portugal", 
                              "Sardinia", "Sicily", "Slovenia", "Spain", "Syria", "Tunisia", "Turkey")

sample_origin_med_levels_marine_taxa <- c("Albania", "Algeria", "Croatia", "Cyprus", "Egypt", "France",
                                          "Greece", "Israel", "Italy", "Lebanon", "Libya", "Malta", "Montenegro",
                                          "Morocco", "Slovenia", "Spain", "Syria", "Tunisia", "Turkey")

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
papilio_data_med$sample_origin <- factor(papilio_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
papilio_data_med$sequencer_loc <- fct_explicit_na(papilio_data_med$sequencer_loc)
lumbri_data_med$sample_origin <- factor(lumbri_data_med$sample_origin, levels = sample_origin_med_levels, ordered = FALSE)
lumbri_data_med$sequencer_loc <- fct_explicit_na(lumbri_data_med$sequencer_loc)
fish_data_med$sample_origin <- factor(fish_data_med$sample_origin, levels = sample_origin_med_levels_marine_taxa, ordered = FALSE)
fish_data_med$sequencer_loc <- fct_explicit_na(fish_data_med$sequencer_loc)
porifera_data_med$sample_origin <- factor(porifera_data_med$sample_origin, levels = sample_origin_med_levels_marine_taxa, ordered = FALSE)
porifera_data_med$sequencer_loc <- fct_explicit_na(porifera_data_med$sequencer_loc)
crusta_data_med$sample_origin <- factor(crusta_data_med$sample_origin, levels = sample_origin_med_levels_marine_taxa, ordered = FALSE)
crusta_data_med$sequencer_loc <- fct_explicit_na(crusta_data_med$sequencer_loc)
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
papilio_seq_loc_tab <- papilio_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
lumbri_seq_loc_tab <- lumbri_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
fish_seq_loc_tab <- fish_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
porifera_seq_loc_tab <- porifera_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
crusta_seq_loc_tab <- crusta_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
tree_seq_loc_tab <- tree_data_med %>% group_by(sample_origin, sequencer_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = sequencer_loc, values_from = n)
# Nb : missing values in sequencer_loc are the consequence of a NA in sequencer_nationality, eg resulting from a typing error in the sequencer address

# add column with total number of sequences
plant_seq_loc_tab <- cbind(plant_seq_loc_tab, n_seq = plant_seq_loc_tab$from_country + plant_seq_loc_tab$inside_med + plant_seq_loc_tab$outside_med + plant_seq_loc_tab$`(Missing)`)
fungi_seq_loc_tab <- cbind(fungi_seq_loc_tab, n_seq = fungi_seq_loc_tab$from_country + fungi_seq_loc_tab$inside_med + fungi_seq_loc_tab$outside_med + fungi_seq_loc_tab$`(Missing)`)
amph_seq_loc_tab <- cbind(amph_seq_loc_tab, n_seq = amph_seq_loc_tab$from_country + amph_seq_loc_tab$inside_med + amph_seq_loc_tab$outside_med + amph_seq_loc_tab$`(Missing)`)
rept_seq_loc_tab <- cbind(rept_seq_loc_tab, n_seq = rept_seq_loc_tab$from_country + rept_seq_loc_tab$inside_med + rept_seq_loc_tab$outside_med + rept_seq_loc_tab$`(Missing)`)
bird_seq_loc_tab <- cbind(bird_seq_loc_tab, n_seq = bird_seq_loc_tab$from_country + bird_seq_loc_tab$inside_med + bird_seq_loc_tab$outside_med + bird_seq_loc_tab$`(Missing)`)
mammal_seq_loc_tab <- cbind(mammal_seq_loc_tab, n_seq = mammal_seq_loc_tab$from_country + mammal_seq_loc_tab$inside_med + mammal_seq_loc_tab$outside_med + mammal_seq_loc_tab$`(Missing)`)
coleo_seq_loc_tab <- cbind(coleo_seq_loc_tab, n_seq = coleo_seq_loc_tab$from_country + coleo_seq_loc_tab$inside_med + coleo_seq_loc_tab$outside_med + coleo_seq_loc_tab$`(Missing)`)
papilio_seq_loc_tab <- cbind(papilio_seq_loc_tab, n_seq = papilio_seq_loc_tab$from_country + papilio_seq_loc_tab$inside_med + papilio_seq_loc_tab$outside_med + papilio_seq_loc_tab$`(Missing)`)
lumbri_seq_loc_tab <- cbind(lumbri_seq_loc_tab, n_seq = lumbri_seq_loc_tab$from_country + lumbri_seq_loc_tab$inside_med + lumbri_seq_loc_tab$outside_med + lumbri_seq_loc_tab$`(Missing)`)
fish_seq_loc_tab <- cbind(fish_seq_loc_tab, n_seq = fish_seq_loc_tab$from_country + fish_seq_loc_tab$inside_med + fish_seq_loc_tab$outside_med + fish_seq_loc_tab$`(Missing)`)
porifera_seq_loc_tab <- cbind(porifera_seq_loc_tab, n_seq = porifera_seq_loc_tab$from_country + porifera_seq_loc_tab$inside_med + porifera_seq_loc_tab$outside_med + porifera_seq_loc_tab$`(Missing)`)
crusta_seq_loc_tab <- cbind(crusta_seq_loc_tab, n_seq = crusta_seq_loc_tab$from_country + crusta_seq_loc_tab$inside_med + crusta_seq_loc_tab$outside_med + crusta_seq_loc_tab$`(Missing)`)
tree_seq_loc_tab <- cbind(tree_seq_loc_tab, n_seq = tree_seq_loc_tab$from_country + tree_seq_loc_tab$inside_med + tree_seq_loc_tab$outside_med + tree_seq_loc_tab$`(Missing)`)

# save tables
write_csv2(plant_seq_loc_tab, file = "./output/text/GenBank_plant_seq_loc_tab.csv", col_names = TRUE)
write_csv2(fungi_seq_loc_tab, file = "./output/text/GenBank_fungi_seq_loc_tab.csv", col_names = TRUE)
write_csv2(amph_seq_loc_tab, file = "./output/text/GenBank_amph_seq_loc_tab.csv", col_names = TRUE)
write_csv2(rept_seq_loc_tab, file = "./output/text/GenBank_rept_seq_loc_tab.csv", col_names = TRUE)
write_csv2(bird_seq_loc_tab, file = "./output/text/GenBank_bird_seq_loc_tab.csv", col_names = TRUE)
write_csv2(mammal_seq_loc_tab, file = "./output/text/GenBank_mammal_seq_loc_tab.csv", col_names = TRUE)
write_csv2(coleo_seq_loc_tab, file = "./output/text/GenBank_coleo_seq_loc_tab.csv", col_names = TRUE)
write_csv2(papilio_seq_loc_tab, file = "./output/text/GenBank_papilio_seq_loc_tab.csv", col_names = TRUE)
write_csv2(lumbri_seq_loc_tab, file = "./output/text/GenBank_lumbri_seq_loc_tab.csv", col_names = TRUE)
write_csv2(fish_seq_loc_tab, file = "./output/text/GenBank_fish_seq_loc_tab.csv", col_names = TRUE)
write_csv2(porifera_seq_loc_tab, file = "./output/text/GenBank_porifera_seq_loc_tab.csv", col_names = TRUE)
write_csv2(crusta_seq_loc_tab, file = "./output/text/GenBank_crusta_seq_loc_tab.csv", col_names = TRUE)
write_csv2(tree_seq_loc_tab, file = "./output/text/GenBank_tree_seq_loc_tab.csv", col_names = TRUE)


### recap tables with proportion of sequences assigned at species level per country for each taxa
plant_species_level_tab <- GB_recap_species_level(taxa_data_med = plant_data_med, taxa_name = "plant")
fungi_species_level_tab <- GB_recap_species_level(taxa_data_med = fungi_data_med, taxa_name = "fungi")
amph_species_level_tab <- GB_recap_species_level(taxa_data_med = amph_data_med, taxa_name = "amphibian")
rept_species_level_tab <- GB_recap_species_level(taxa_data_med = rept_data_med, taxa_name = "reptile")
bird_species_level_tab <- GB_recap_species_level(taxa_data_med = bird_data_med, taxa_name = "bird")
mammal_species_level_tab <- GB_recap_species_level(taxa_data_med = mammal_data_med, taxa_name = "mammal")
coleo_species_level_tab <- GB_recap_species_level(taxa_data_med = coleo_data_med, taxa_name = "coleoptera")
papilio_species_level_tab <- GB_recap_species_level(taxa_data_med = papilio_data_med, taxa_name = "papilionoidea")
lumbri_species_level_tab <- GB_recap_species_level(taxa_data_med = lumbri_data_med, taxa_name = "lumbricina")
fish_species_level_tab <- GB_recap_species_level(taxa_data_med = fish_data_med, taxa_name = "fish")
porifera_species_level_tab <- GB_recap_species_level(taxa_data_med = porifera_data_med, taxa_name = "porifera")
crusta_species_level_tab <- GB_recap_species_level(taxa_data_med = crusta_data_med, taxa_name = "crustacea")
tree_species_level_tab <- GB_recap_species_level(taxa_data_med = tree_data_med, taxa_name = "tree")

# for marine taxa remove countries without any part of their ZEE in the med sea and islands (empty rows anyway)
sample_origin_to_remove <- c("Bosnia and Herzegovina", "Palestine", "Portugal",
                             "Balearic Islands", "Corsica", "Crete", "Sardinia", "Sicily")
fish_species_level_tab <- fish_species_level_tab %>% filter(!country %in% sample_origin_to_remove)
porifera_species_level_tab <- porifera_species_level_tab %>% filter(!country %in% sample_origin_to_remove)
crusta_species_level_tab <- crusta_species_level_tab %>% filter(!country %in% sample_origin_to_remove)

# save tables
write_csv2(plant_species_level_tab, file = "./output/text/GenBank_plant_species_level_tab.csv", col_names = TRUE)
write_csv2(fungi_species_level_tab, file = "./output/text/GenBank_fungi_species_level_tab.csv", col_names = TRUE)
write_csv2(amph_species_level_tab, file = "./output/text/GenBank_amph_species_level_tab.csv", col_names = TRUE)
write_csv2(rept_species_level_tab, file = "./output/text/GenBank_rept_species_level_tab.csv", col_names = TRUE)
write_csv2(bird_species_level_tab, file = "./output/text/GenBank_bird_species_level_tab.csv", col_names = TRUE)
write_csv2(mammal_species_level_tab, file = "./output/text/GenBank_mammal_species_level_tab.csv", col_names = TRUE)
write_csv2(coleo_species_level_tab, file = "./output/text/GenBank_coleo_species_level_tab.csv", col_names = TRUE)
write_csv2(papilio_species_level_tab, file = "./output/text/GenBank_papilio_species_level_tab.csv", col_names = TRUE)
write_csv2(lumbri_species_level_tab, file = "./output/text/GenBank_lumbri_species_level_tab.csv", col_names = TRUE)
write_csv2(fish_species_level_tab, file = "./output/text/GenBank_fish_species_level_tab.csv", col_names = TRUE)
write_csv2(porifera_species_level_tab, file = "./output/text/GenBank_porifera_species_level_tab.csv", col_names = TRUE)
write_csv2(crusta_species_level_tab, file = "./output/text/GenBank_crusta_species_level_tab.csv", col_names = TRUE)
write_csv2(tree_species_level_tab, file = "./output/text/GenBank_tree_species_level_tab.csv", col_names = TRUE)


### recap table with the number of species with at least one sequence in GenBank for the islands
island_sp_tab <- rbind(plant_species_level_tab, fungi_species_level_tab, amph_species_level_tab,
                       rept_species_level_tab, bird_species_level_tab, mammal_species_level_tab,
                       coleo_species_level_tab, papilio_species_level_tab, lumbri_species_level_tab,
                       tree_species_level_tab)

island_list <- c("Balearic Islands", "Corsica", "Crete", "Cyprus", "Sardinia", "Sicily", "Malta")
island_sp_tab <- island_sp_tab %>% filter(country %in% island_list)

table_islands_n_sp <- island_sp_tab[, c(1, 2, 5)] %>% 
  pivot_wider(names_from = taxa, values_from = n_sp)

table_islands_n_seq <- island_sp_tab[, c(1, 2, 4)] %>% 
  pivot_wider(names_from = taxa, values_from = n_seq)

# save tables
write_csv2(island_sp_tab, file = "./output/text/GenBank_island_sp_tab.csv", col_names = TRUE)
write_csv2(table_islands_n_sp, file = "./output/text/GenBank_islands_n_sp_tab.csv", col_names = TRUE)
write_csv2(table_islands_n_seq, file = "./output/text/GenBank_islands_n_seq_tab.csv", col_names = TRUE)

### get the corresponding species names
plant_island_sp_list <- GB_island_species_list(plant_data_med)
fungi_island_sp_list <- GB_island_species_list(fungi_data_med)
amph_island_sp_list <- GB_island_species_list(amph_data_med)
rept_island_sp_list <- GB_island_species_list(rept_data_med)
bird_island_sp_list <- GB_island_species_list(bird_data_med)
mammal_island_sp_list <- GB_island_species_list(mammal_data_med)
coleo_island_sp_list <- GB_island_species_list(coleo_data_med)
papilio_island_sp_list <- GB_island_species_list(papilio_data_med)
lumbri_island_sp_list <- GB_island_species_list(lumbri_data_med)
tree_island_sp_list <- GB_island_species_list(tree_data_med)

# save tables
write_csv2(plant_island_sp_list, file = "./output/text/GenBank_island_sp_list_plant.csv", col_names = TRUE)
write_csv2(fungi_island_sp_list, file = "./output/text/GenBank_island_sp_list_fungi.csv", col_names = TRUE)
write_csv2(amph_island_sp_list, file = "./output/text/GenBank_island_sp_list_amph.csv", col_names = TRUE)
write_csv2(rept_island_sp_list, file = "./output/text/GenBank_island_sp_list_rept.csv", col_names = TRUE)
write_csv2(bird_island_sp_list, file = "./output/text/GenBank_island_sp_list_bird.csv", col_names = TRUE)
write_csv2(mammal_island_sp_list, file = "./output/text/GenBank_island_sp_list_mammal.csv", col_names = TRUE)
write_csv2(coleo_island_sp_list, file = "./output/text/GenBank_island_sp_list_coleo.csv", col_names = TRUE)
write_csv2(papilio_island_sp_list, file = "./output/text/GenBank_island_sp_list_papilio.csv", col_names = TRUE)
write_csv2(lumbri_island_sp_list, file = "./output/text/GenBank_island_sp_list_lumbri.csv", col_names = TRUE)
write_csv2(tree_island_sp_list, file = "./output/text/GenBank_island_sp_list_tree.csv", col_names = TRUE)

### recap tables with number of sequences per year per country for each taxa
# fix year factor levels for all taxa
year_list <- c(1987:2020)

plant_data_med$year <- factor(plant_data_med$year, levels = year_list, ordered = TRUE)
fungi_data_med$year <- factor(fungi_data_med$year, levels = year_list, ordered = TRUE)
amph_data_med$year <- factor(amph_data_med$year, levels = year_list, ordered = TRUE)
rept_data_med$year <- factor(rept_data_med$year, levels = year_list, ordered = TRUE)
bird_data_med$year <- factor(bird_data_med$year, levels = year_list, ordered = TRUE)
mammal_data_med$year <- factor(mammal_data_med$year, levels = year_list, ordered = TRUE)
coleo_data_med$year <- factor(coleo_data_med$year, levels = year_list, ordered = TRUE)
papilio_data_med$year <- factor(papilio_data_med$year, levels = year_list, ordered = TRUE)
lumbri_data_med$year <- factor(lumbri_data_med$year, levels = year_list, ordered = TRUE)
fish_data_med$year <- factor(fish_data_med$year, levels = year_list, ordered = TRUE)
porifera_data_med$year <- factor(porifera_data_med$year, levels = year_list, ordered = TRUE)
crusta_data_med$year <- factor(crusta_data_med$year, levels = year_list, ordered = TRUE)
tree_data_med$year <- factor(tree_data_med$year, levels = year_list, ordered = TRUE)

plant_country_year_tab <- plant_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
fungi_country_year_tab <- fungi_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
amph_country_year_tab <- amph_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
rept_country_year_tab <- rept_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
bird_country_year_tab <- bird_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
mammal_country_year_tab <- mammal_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
coleo_country_year_tab <- coleo_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
papilio_country_year_tab <- papilio_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
lumbri_country_year_tab <- lumbri_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
fish_country_year_tab <- fish_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
porifera_country_year_tab <- porifera_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
crusta_country_year_tab <- crusta_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
tree_country_year_tab <- tree_data_med %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)

# save tables
write_csv2(plant_country_year_tab, file = "./output/text/GenBank_plant_country_year_tab.csv", col_names = TRUE)
write_csv2(fungi_country_year_tab, file = "./output/text/GenBank_fungi_country_year_tab.csv", col_names = TRUE)
write_csv2(amph_country_year_tab, file = "./output/text/GenBank_amph_country_year_tab.csv", col_names = TRUE)
write_csv2(rept_country_year_tab, file = "./output/text/GenBank_rept_country_year_tab.csv", col_names = TRUE)
write_csv2(bird_country_year_tab, file = "./output/text/GenBank_bird_country_year_tab.csv", col_names = TRUE)
write_csv2(mammal_country_year_tab, file = "./output/text/GenBank_mammal_country_year_tab.csv", col_names = TRUE)
write_csv2(coleo_country_year_tab, file = "./output/text/GenBank_coleo_country_year_tab.csv", col_names = TRUE)
write_csv2(papilio_country_year_tab, file = "./output/text/GenBank_papilio_country_year_tab.csv", col_names = TRUE)
write_csv2(lumbri_country_year_tab, file = "./output/text/GenBank_lumbri_country_year_tab.csv", col_names = TRUE)
write_csv2(fish_country_year_tab, file = "./output/text/GenBank_fish_country_year_tab.csv", col_names = TRUE)
write_csv2(porifera_country_year_tab, file = "./output/text/GenBank_porifera_country_year_tab.csv", col_names = TRUE)
write_csv2(crusta_country_year_tab, file = "./output/text/GenBank_crusta_country_year_tab.csv", col_names = TRUE)
write_csv2(tree_country_year_tab, file = "./output/text/GenBank_tree_country_year_tab.csv", col_names = TRUE)

### same table but for accumulation curve
plant_country_year_tab_acc <- table_accumulate(year_tab = plant_country_year_tab, first_column = 2)
fungi_country_year_tab_acc <- table_accumulate(year_tab = fungi_country_year_tab, first_column = 2)
amph_country_year_tab_acc <- table_accumulate(year_tab = amph_country_year_tab, first_column = 2)
rept_country_year_tab_acc <- table_accumulate(year_tab = rept_country_year_tab, first_column = 2)
bird_country_year_tab_acc <- table_accumulate(year_tab = bird_country_year_tab, first_column = 2)
mammal_country_year_tab_acc <- table_accumulate(year_tab = mammal_country_year_tab, first_column = 2)
coleo_country_year_tab_acc <- table_accumulate(year_tab = coleo_country_year_tab, first_column = 2)
papilio_country_year_tab_acc <- table_accumulate(year_tab = papilio_country_year_tab, first_column = 2)
lumbri_country_year_tab_acc <- table_accumulate(year_tab = lumbri_country_year_tab, first_column = 2)
fish_country_year_tab_acc <- table_accumulate(year_tab = fish_country_year_tab, first_column = 2)
porifera_country_year_tab_acc <- table_accumulate(year_tab = porifera_country_year_tab, first_column = 2)
crusta_country_year_tab_acc <- table_accumulate(year_tab = crusta_country_year_tab, first_column = 2)
tree_country_year_tab_acc <- table_accumulate(year_tab = tree_country_year_tab, first_column = 2)

# save tables
write_csv2(plant_country_year_tab_acc, file = "./output/text/GenBank_plant_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(fungi_country_year_tab_acc, file = "./output/text/GenBank_fungi_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(amph_country_year_tab_acc, file = "./output/text/GenBank_amph_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(rept_country_year_tab_acc, file = "./output/text/GenBank_rept_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(bird_country_year_tab_acc, file = "./output/text/GenBank_bird_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(mammal_country_year_tab_acc, file = "./output/text/GenBank_mammal_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(coleo_country_year_tab_acc, file = "./output/text/GenBank_coleo_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(papilio_country_year_tab_acc, file = "./output/text/GenBank_papilio_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(lumbri_country_year_tab_acc, file = "./output/text/GenBank_lumbri_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(fish_country_year_tab_acc, file = "./output/text/GenBank_fish_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(porifera_country_year_tab_acc, file = "./output/text/GenBank_porifera_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(crusta_country_year_tab_acc, file = "./output/text/GenBank_crusta_country_year_tab_acc.csv", col_names = TRUE)
write_csv2(tree_country_year_tab_acc, file = "./output/text/GenBank_tree_country_year_tab_acc.csv", col_names = TRUE)

