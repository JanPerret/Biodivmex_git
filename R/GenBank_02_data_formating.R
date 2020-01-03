#################################################
#
# Biodivmex project
#
# GenBank_02_data_formating.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### Load data
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


### Correct minor data errors

# drop marine taxa columns for plants and fungi
fungi_data <- fungi_data %>%
  select(-fish, -sponge, -crustacea)
plant_data <- plant_data %>%
  select(-fish, -sponge, -crustacea)

# Replace species_levels values "BUG" by NAs
# "BUG" indicates that the field that contains the species name 
# is in an unusual format and that the species name couldn't be extracted
fungi_data <- fungi_data %>% 
  mutate(species_level = replace(species_level, species_level == "BUG", NA))
plant_data <- plant_data %>% 
  mutate(species_level = replace(species_level, species_level == "BUG", NA))
metazoa_data <- metazoa_data %>% 
  mutate(species_level = replace(species_level, species_level == "BUG", NA))

# Replace species_levels values which contain digits by NAs
fungi_data <- fungi_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[:digit:]"), NA))
plant_data <- plant_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[:digit:]"), NA))
metazoa_data <- metazoa_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[:digit:]"), NA))

# Remove the single quotes of the field species_level
fungi_data$species_level <- str_replace_all(fungi_data$species_level, "'", "")
plant_data$species_level <- str_replace_all(plant_data$species_level, "'", "")
metazoa_data$species_level <- str_replace_all(metazoa_data$species_level, "'", "")

# Replace species_levels values which contain punctuation signs other than "-" by NAs
fungi_data <- fungi_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[^-[:^punct:]]"), NA)) # you can exclude "-" sign from the POSIX class punct using a double negative like this : [^-[:^punct:]]
plant_data <- plant_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[^-[:^punct:]]"), NA))
metazoa_data <- metazoa_data %>% 
  mutate(species_level = replace(species_level, str_detect(species_level,"[^-[:^punct:]]"), NA))

# Reassign country names for Palestine
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

