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


### make sub data frames for the mediterranean basin and each animal taxa
# load list of mediterranean countries and island
med_countries_list <- read_delim("./data/name_list_med_countries_and_islands_GenBank.csv", delim = ",", col_names = FALSE)
med_countries_list <- med_countries_list$X1

# make a sub-dataframe for each animal taxa in the WORLD
amph_data <- metazoa_data %>% filter(taxa == "Amphibian")
rept_data <- metazoa_data %>% filter(taxa == "Reptile")
bird_data <- metazoa_data %>% filter(taxa == "Bird")
mammal_data <- metazoa_data %>% filter(taxa == "Mammal")
coleo_data <- metazoa_data %>% filter(taxa == "Coleoptera")
lumbri_data <- metazoa_data %>% filter(taxa == "Lumbricina")
papilio_data <- metazoa_data %>% filter(taxa == "Papilionoidea")
sponge_data <- metazoa_data %>% filter(taxa == "Sponge") #########################
crusta_data <- metazoa_data %>% filter(taxa == "Crustacea") ###################### A VOIR SI JE PRENDS LA COLONNE TAXA OU LES COLONNES CREES AVEC LES NOMS D'ESPECES
fish_data <- metazoa_data %>% filter(taxa == "Fish") #############################

# make a sub-dataframe for each animal taxa in the MEDITERRANEAN BASIN
# filter dataframes with med countries
plant_data_med <- plant_data %>% filter(sample_origin %in% med_countries_list)
fungi_data_med <- fungi_data %>% filter(sample_origin %in% med_countries_list)
metazoa_data_med <- metazoa_data %>% filter(sample_origin %in% med_countries_list)

# make sub-dataframes
amph_data_med <- metazoa_data_med %>% filter(taxa == "Amphibian")
rept_data_med <- metazoa_data_med %>% filter(taxa == "Reptile")
bird_data_med <- metazoa_data_med %>% filter(taxa == "Bird")
mammal_data_med <- metazoa_data_med %>% filter(taxa == "Mammal")
coleo_data_med <- metazoa_data_med %>% filter(taxa == "Coleoptera")
lumbri_data_med <- metazoa_data_med %>% filter(taxa == "Lumbricina")
papilio_data_med <- metazoa_data_med %>% filter(taxa == "Papilionoidea")
sponge_data_med <- metazoa_data_med %>% filter(taxa == "Sponge") #########################
crusta_data_med <- metazoa_data_med %>% filter(taxa == "Crustacea") ###################### A VOIR SI JE PRENDS LA COLONNE TAXA OU LES COLONNES CREES AVEC LES NOMS D'ESPECES
fish_data_med <- metazoa_data_med %>% filter(taxa == "Fish") #############################

#############################
############################# ICI IL Y A UN FILTRAGE DES GENES A FAIRE AUSSI POUR GARDER QUE LES MITOC POUR LES TAXONS DE METAZOAIRES TERRESTRES
############################# utiliser filter() et faire une liste des genes nucleaires avec une condition gene != nuc_gene_list 

# 3 arguments necessaires pour la future fonction

kingdom_data = plant_data
taxa_data = plant_data
taxa_data_med = plant_data_med

kingdom_data = metazoa_data
taxa_data = amph_data
taxa_data_med = amph_data_med

kingdom_data = metazoa_data
taxa_data = lumbri_data
taxa_data_med = lumbri_data_med

GB_extract_general_info <- function(kingdom_data, taxa_data, taxa_data_med) {
  
  # initiaze table to store the 6 general general descriptive informations 
  descritive_table <- setNames(data.frame(matrix(ncol = 6, nrow = 1)),
                                     c("n_seq", "taxa_n_seq", "loc_rate", "n_seq_med", "n_seq_sp_level_med", "n_sp_med"))
  
  # 1. number of sequences in GenBank for the reign (metazoa, thracheophyta or fungi)
  descritive_table$n_seq <- length(kingdom_data$access_num)
  
  # 2. number of sequences for the chosen taxonomic group (identical for fungi and plants)
  descritive_table$taxa_n_seq <- length(taxa_data$access_num)
  
  # 3. percentage of sequences with a sample origin information
  descritive_table$loc_rate <- round((sum(!is.na(taxa_data$sample_origin))/length(taxa_data$access_num))*100, 2)

  # 4. number of sequences affiliated to a country of the mediterranean basin for the taxa
  descritive_table$n_seq_med <- length(taxa_data_med$access_num)
  
  # 5. percentage of sequences affiliated at least to species level (can be more precise) for the taxa in the mediterranean basin
  # descritive_table$n_seq_sp_level_med <- round((sum(!is.na(taxa_data_med$species_level))/length(taxa_data_med$species_level))*100, 2)
  descritive_table$n_seq_sp_level_med <- round((sum(!is.na(taxa_data_med$species_level | taxa_data_med$species_level != "hyrid"))/length(taxa_data_med$species_level))*100, 2)
  ######################################################################### ICI IL Y A LES HYBRIDES A GERER !!!!
  
  # 6. number of different species names for the taxa in the mediterranean basin
  descritive_table$n_sp_med <- length(unique(taxa_data_med$species_level))
  
  return(descritive_table)
}

GB_extract_general_info(kingdom_data = metazoa_data,
                        taxa_data = lumbri_data,
                        taxa_data_med = lumbri_data_med)
