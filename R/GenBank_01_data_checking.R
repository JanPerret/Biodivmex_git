#################################################
#
# Biodivmex project
#
# GenBank_01_data_checking.R
#
# jan.perret@cefe.cnrs.fr
#################################################

# commit test after synchronizing folder with SyncBackFree

# load data
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

# drop marine taxa columns for plants and fungi
fungi_data <- fungi_data %>%
  select(-fish, -sponge, -crustacea)

plant_data <- plant_data %>%
  select(-fish, -sponge, -crustacea)


# check molecule type
levels(as.factor(fungi_data$mol_type))
levels(as.factor(plant_data$mol_type))
levels(as.factor(metazoa_data$mol_type))
# only genomic DNA as expected

# check species level names
levels(as.factor(plant_data$species_level))
# [1] "'Oreofraga morrisiana'"         "'Rughidia cordata'"             "'Rughidia milleri'"  
# some species names contain single quotes

levels(as.factor(fungi_data$species_level))
# [1] "'Anthracinomyces petraeus'"          "'Anthracinomyces ramosus'"           "'Diplorhynchus bilobus'"            
# same for fungi

levels(as.factor(metazoa_data$species_level))
# [1] "'AntiopalaX' zalosara"             "'BareaX' ectadia"                  "'CeruraX' melanoglypta"           
# [4] "'CoesyraX' hemiphragma"            "'CoesyraX' stipulata"              "'CoesyraX' violacea"              
# here there are single quotes and upper case X at the end of some genus names

# inspect rows with one of the species names with single quotes
test <- plant_data %>%
  filter(str_detect(species_level,"'Oreofraga morrisiana'"))
test

# inspect rows with one of the levels of species_level with the X problem
test <- metazoa_data %>%
  filter(str_detect(species_level,"'AntiopalaX' zalosara"))
test
# GenBank treats that as a normal species rank, so I'm going to simply remove the single quotes

test <- metazoa_data %>%
  filter(str_detect(species_level,"AntiopalaX zalosara"))
test
# no sequence with this species name without the quotes

test <- metazoa_data %>%
  filter(str_detect(species_level,"'"))
unique(test[10])
# 36 different species names contain single quotes for metazoa sequences

test <- metazoa_data %>%
  filter(str_detect(species_level,"[:punct:]")) %>% print(n=10)
unique(test[10]) %>% print(n=200)
# 3077 sequences have a species_level value containing punctuation, most of them have only a "-" in the genus or species name

test <- fungi_data %>%
  filter(str_detect(species_level,"[:punct:]")) %>% print(n=10)
unique(test[10]) %>% print(n=200)
# 3532 sequences for fungi

test <- plant_data %>%
  filter(str_detect(species_level,"[:punct:]")) %>% print(n=10)
unique(test[10]) %>% print(n=200)
# 3845 for plants

# check if species_level contain digits
test <- metazoa_data %>%
  filter(str_detect(species_level,"[:digit:]")) %>% print(n=10)
unique(test[10]) %>% print(n=200)
# 1382 sequences for metazoa

test <- fungi_data %>%
  filter(str_detect(species_level,"[:digit:]")) %>% print(n=10)
unique(test[10]) %>% print(n=200)
# 43 sequences for fungi

test <- plant_data %>%
  filter(str_detect(species_level,"[:digit:]")) %>% print(n=10)
unique(test[10]) %>% print(n=200)
# 4 sequences for plants
# for these sequences the species_level field will be replaced by NAs

# proportion of hybrids :
table(plant_data$species_level == "hybrid")
#  FALSE   TRUE 
# 690436   5476

table(fungi_data$species_level == "hybrid")
#  FALSE   TRUE 
# 438890     99

table(metazoa_data$species_level == "hybrid")
#   FALSE    TRUE 
# 3891985    4802 

# check sample origin
levels(as.factor(plant_data$sample_origin))
levels(as.factor(fungi_data$sample_origin))
levels(as.factor(metazoa_data$sample_origin))
# seems perfect

plant_data %>% count(sample_origin, sort = TRUE) %>% print(n=40)
fungi_data %>% count(sample_origin, sort = TRUE) %>% print(n=40)
metazoa_data %>% count(sample_origin, sort = TRUE) %>% print(n=40)
# good

# check number of sequences per year
plant_data %>% count(year, sort = FALSE) %>% print(n=40)
fungi_data %>% count(year, sort = FALSE) %>% print(n=40)
metazoa_data %>% count(year, sort = FALSE) %>% print(n=40)
# good

# check numer of sequences per taxa
plant_data %>% count(taxa, sort = TRUE) %>% print(n=40)
fungi_data %>% count(taxa, sort = TRUE) %>% print(n=40)
metazoa_data %>% count(taxa, sort = TRUE) %>% print(n=40)
# interresting : fish and mammals have the most sequences

# check genes
plant_data %>% count(gene, sort = TRUE) %>% print(n=40)
fungi_data %>% count(gene, sort = TRUE) %>% print(n=40)
metazoa_data %>% count(gene, sort = TRUE) %>% print(n=40)
# # A tibble: 433 x 2
# gene                                             n
# <chr>                                        <int>
# 1 COI                                      3347277
# 2 NA                                        511025 ### these are the sequences selected with the condition "mitochondrial DNA" but who couldn't be assigned to a particular gene
# 3 CytB                                      468325
# 4 16S rRNA                                  280068
# 5 ND2                                       102885
# 6 12S rRNA                                  101445
# 7 complete genome                            73938
# 8 tRNA                                       58389
# 9 COX2                                       56368

test <- metazoa_data %>%
  filter(is.na(metazoa_data$gene))
test %>% print(n=1000)

# select random rows from the metazoa dataframe to check why there is so much NAs in the gene column
test <- metazoa_data %>%
  filter(is.na(metazoa_data$gene)) %>% 
  sample_n(size = 30) %>%
  print(n=30)
# after checking a fiew of these samples in GenBank, it seems that there is no major error explaining the 9% of NAs
# they correspond to a variety of sequences (diverse control regions, D-loop, etc.), or typing errors of the gene names
# so nothing to do about that in my opinion

# check species_level
plant_data %>% count(species_level, sort = TRUE) %>% print(n=40)
fungi_data %>% count(species_level, sort = TRUE) %>% print(n=40)
# no particular problems for plants and fungi

metazoa_data %>% count(species_level, sort = TRUE) %>% print(n=40)
# For the metazoa dataframe there are 2 particular things to manage : the hybrids and the "BUG" in species_level
# # A tibble: 200,304 x 2
# species_level                      n
# <chr>                          <int>
# 1 NA                         1550418
# 2 Homo sapiens                180133
# 3 Ovis aries                   13114
# 4 Delia platura                12880
# 5 Capra hircus                 11986
# 6 Sus scrofa                   11982
# 7 Equus caballus               11192
# 8 Bos taurus                   10604
# 9 Gallus gallus                 9611
# 10 Bemisia tabaci                9442
# 11 Canis lupus                   7628
# 12 BUG                           6412 ###
# 13 Entomobrya nivalis            6323
# 14 Ixodes ricinus                4911
# 15 hybrid                        4802 ### 
# 16 Bubalus bubalis               4768
# 17 Echinococcus granulosus       4755
# 18 Lymantria dispar              4634
# 19 Mus musculus                  4576


# check references with "BUG" in species_level column
test <- metazoa_data %>%
  filter(str_detect(species_level,"BUG"))
unique(test[4]) %>% print(n=200)
# 6402 sequences

test <- fungi_data %>%
  filter(str_detect(species_level,"BUG"))
unique(test[4]) %>% print(n=200)
# 1611 sequences

test <- plant_data %>%
  filter(str_detect(species_level,"BUG"))
unique(test[4]) %>% print(n=200)
# 18 sequences


# checking supplementary geographic informations
levels(as.factor(plant_data$supp_origin_infos))
levels(as.factor(fungi_data$supp_origin_infos))
levels(as.factor(metazoa_data$supp_origin_infos))
# ok

