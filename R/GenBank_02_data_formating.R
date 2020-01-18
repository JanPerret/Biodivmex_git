#################################################
#
# Biodivmex project
#
# GenBank_02_data_formating.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### load data
fungi_data <- read_delim("./data/TEST_v10_SIMPLE_RESULT_GENBANK_v10_fungi_references.csv", delim = ",", col_names = TRUE,
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
                           crustacea = col_character()
                         ))

plant_data <- read_delim("./data/TEST_v10_SIMPLE_RESULT_GENBANK_v10_plant_references.csv", delim = ",", col_names = TRUE,
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
                           crustacea = col_character()
                         ))

metazoa_data <- read_delim("./data/TEST_v10_SIMPLE_RESULT_GENBANK_v10_animal_references_mitochondrial_nuclear_seq.csv", delim = ",", col_names = TRUE,
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
                             crustacea = col_character()
                           ))


### correct minor data errors
# drop marine taxa columns for plants and fungi
fungi_data <- fungi_data %>%
  select(-fish, -sponge, -crustacea)
plant_data <- plant_data %>%
  select(-fish, -sponge, -crustacea)

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


### create sub-data frame without nuclear genes (only used for marine taxa)
nuc_gene_names <- c("18S rRNA", "28S rRNA", "5.8S rRNA", "ITS", "Rag 2")

# save data frame with nuclear sequences for marine taxa
metazoa_data_with_nuc <- metazoa_data

# data frame for terrestrial taxa
metazoa_data <- metazoa_data %>% 
  filter(!str_detect(gene, paste(nuc_gene_names, sep = "|")))


### make sub-data frames for the mediterranean basin and each animal taxa
# load list of mediterranean countries and island
med_countries_list <- read_delim("./data/name_list_med_countries_and_islands_GenBank.csv", delim = ",", col_names = FALSE)
med_countries_list <- med_countries_list$X1

# make a sub-data frame for each animal taxa in the WORLD
amph_data <- metazoa_data %>% filter(taxa == "Amphibian")
rept_data <- metazoa_data %>% filter(taxa == "Reptile")
bird_data <- metazoa_data %>% filter(taxa == "Bird")
mammal_data <- metazoa_data %>% filter(taxa == "Mammal")
coleo_data <- metazoa_data %>% filter(taxa == "Coleoptera")
lumbri_data <- metazoa_data %>% filter(taxa == "Lumbricina")
papilio_data <- metazoa_data %>% filter(taxa == "Papilionoidea")
sponge_data <- metazoa_data_with_nuc %>% filter(taxa == "Sponge") #########################
crusta_data <- metazoa_data_with_nuc %>% filter(taxa == "Crustacea") ###################### A VOIR SI JE PRENDS LA COLONNE TAXA OU LES COLONNES CREES AVEC LES NOMS D'ESPECES
fish_data <- metazoa_data_with_nuc %>% filter(taxa == "Fish") #############################

# make a sub-data frame for each animal taxa in the MEDITERRANEAN BASIN
# filter dataframes with med countries
plant_data_med <- plant_data %>% filter(sample_origin %in% med_countries_list)
fungi_data_med <- fungi_data %>% filter(sample_origin %in% med_countries_list)
metazoa_data_med <- metazoa_data %>% filter(sample_origin %in% med_countries_list)
metazoa_data_with_nuc_med <- metazoa_data_with_nuc %>% filter(sample_origin %in% med_countries_list)

# make sub-data frames
amph_data_med <- metazoa_data_med %>% filter(taxa == "Amphibian")
rept_data_med <- metazoa_data_med %>% filter(taxa == "Reptile")
bird_data_med <- metazoa_data_med %>% filter(taxa == "Bird")
mammal_data_med <- metazoa_data_med %>% filter(taxa == "Mammal")
coleo_data_med <- metazoa_data_med %>% filter(taxa == "Coleoptera")
lumbri_data_med <- metazoa_data_med %>% filter(taxa == "Lumbricina")
papilio_data_med <- metazoa_data_med %>% filter(taxa == "Papilionoidea")
sponge_data_med <- metazoa_data_with_nuc_med %>% filter(taxa == "Sponge") #########################
crusta_data_med <- metazoa_data_with_nuc_med %>% filter(taxa == "Crustacea") ###################### A VOIR SI JE PRENDS LA COLONNE TAXA OU LES COLONNES CREES AVEC LES NOMS D'ESPECES
fish_data_med <- metazoa_data_with_nuc_med %>% filter(taxa == "Fish") #############################

# function to extract general informations for a diven taxa
GB_extract_general_info <- function(kingdom_data, taxa_data, taxa_data_med, taxa_name) {
  
  # initiaze table to store the 6 general general descriptive informations
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
  descritive_table$n_sp_med <- length(unique(taxa_data_med$species_level))
  
  return(descritive_table)
}


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

# make recap table
all_taxa_desc_tab <- rbind(plant_desc_tab, fungi_desc_tab, amph_desc_tab, rept_desc_tab, bird_desc_tab, mammal_desc_tab,
                           coleo_desc_tab, lumbri_desc_tab, papilio_desc_tab, sponge_desc_tab, crusta_desc_tab, fish_desc_tab)
# save table
write_csv2(all_taxa_desc_tab, path = "./output/text/GenBank_all_taxa_descriptive_mesures.csv", col_names = TRUE)


#############################
############################# # ICI SORTIR L'INFO DU NOMBRE DE SEQUENCES POUR CHAQUE GENE (dont "NA") + une colonne avec le nombre total de sequences pour faire des pourcentages
############################# et sortir les deux tableaux : un avec les effectifs et un avec ls pourcentages et les sauver en CSV
