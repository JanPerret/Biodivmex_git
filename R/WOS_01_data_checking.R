#################################################
#
# Biodivmex project
#
# WOS_01_data_checking.R
#
# jan.perret@cefe.cnrs.fr
#################################################


# load data
wos_data <- read_delim("./data/TEST_SIMPLE_v15_RESULT_WOS_files_merged.csv", delim = ",", col_names = TRUE,
                          col_types = cols(
                            access_num = col_character(),
                            language = col_character(),
                            doc_type = col_character(),
                            publisher = col_character(),
                            year = col_integer(),
                            author_nationality = col_character(),
                            fieldwork_country = col_character(),
                            marine_region = col_character(),
                            outside_med = col_character(),
                            plant = col_character(),
                            fungi = col_character(),
                            amphibian = col_character(),
                            reptile = col_character(),
                            bird = col_character(),
                            mammal = col_character(),
                            fish = col_character(),
                            sponge = col_character(),
                            crustacea = col_character(),
                            coleoptera = col_character(),
                            papilionoidea = col_character(),
                            lumbricina = col_character(),
                            tree = col_character()
                          ))

# inspect outside_med articles before dropping them
wos_data %>% count(outside_med, author_nationality, sort = TRUE) %>% print(n=100)
# seems correct : articles identified with outside_med are often written by authors from regions of the world with mediterranean climate

wos_data %>% count(outside_med, fieldwork_country, sort = TRUE) %>% print(n=300)
# perfect all outside_med articles have a NA in the fieldwork_country field !

# drop articles about mediterranean regions outside the mediterranean basin
wos_data <- wos_data %>%
  filter(is.na(outside_med))

# drop the outside_med column
wos_data <- wos_data %>%
  select(-outside_med)

# test if there are errors in access_num column
length(wos_data$access_num) == length(unique(wos_data$access_num))
# no error here

# check language
table(wos_data$language)
# 2028 articles not written in english and 63216 in english

subset(wos_data, (wos_data$language == "French"))$access_num
# after cheking in WOS directly for a few articles, some have an abstract in english but not all of them

# check fieldwork country rate
wos_data %>% count(language, fieldwork_country, sort = TRUE) %>% print(n=100)
# removing articles not in english will make us loose 244 articles with France as fieldwork country,
# 106 articles with Spain, 61 articles for Algeria, 50 articles for Turkey, 49 for Morocco, 45 for Tunisia, 24 for Corsica.
# seems ok to do so, as we don't know how many of these articles have an abstract in english in WOS, so it may biais our results.

# drop articles not written in english
wos_data <- wos_data %>%
  filter(language == "English")

# count doc_type
wos_data %>% count(doc_type, sort = TRUE) %>% print(n=100)
# seems not very standardised, but we will keep everything here.

# count publisher
wos_data %>% count(publisher, sort = TRUE) %>% print(n=100)
# # A tibble: 3,647 x 2
# publisher                                                                 n
# <chr>                                                                 <int>
# 1 SCIENCE OF THE TOTAL ENVIRONMENT                                      900
# 2 MARINE ECOLOGY PROGRESS SERIES                                        719
# 3 MARINE POLLUTION BULLETIN                                             697
# 4 HYDROBIOLOGIA                                                         603
# 5 SCIENTIA MARINA                                                       580
# 6 ESTUARINE COASTAL AND SHELF SCIENCE                                   552
# 7 PALAEOGEOGRAPHY PALAEOCLIMATOLOGY PALAEOECOLOGY                       539
# 8 MARINE GEOLOGY                                                        524
# 9 JOURNAL OF THE MARINE BIOLOGICAL ASSOCIATION OF THE UNITED KINGDOM    515
# 10 MARINE BIOLOGY                                                       506
# 11 ZOOTAXA                                                              438
# 12 MEDITERRANEAN MARINE SCIENCE                                         433
# 13 FOREST ECOLOGY AND MANAGEMENT                                        419
# 14 ATMOSPHERIC ENVIRONMENT                                              406
# 15 FRESENIUS ENVIRONMENTAL BULLETIN                                     382
# 16 JOURNAL OF MARINE SYSTEMS                                            372
# 17 JOURNAL OF GEOPHYSICAL RESEARCH-OCEANS                               368
# 18 DEEP-SEA RESEARCH PART I-OCEANOGRAPHIC RESEARCH PAPERS               328
# 19 JOURNAL OF ECONOMIC ENTOMOLOGY                                       320
# 20 CATENA                                                               317
# 21 JOURNAL OF BIOGEOGRAPHY                                              315
# 22 PLANT BIOSYSTEMS                                                     313
# 23 ATMOSPHERIC CHEMISTRY AND PHYSICS                                    302
# 24 CAHIERS DE BIOLOGIE MARINE                                           290
# 25 FISHERIES RESEARCH                                                   285
# 26 MARINE ENVIRONMENTAL RESEARCH                                        284
# 27 PROGRESS IN OCEANOGRAPHY                                             279
# 28 JOURNAL OF HYDROLOGY                                                 277

# marine journals dominate the ranking, even if we have filtered the papers by research area at the WOS export step and excluded research areas not concerning biology


# count years
wos_data %>% count(year, sort = FALSE) %>% print(n=100)

# drop articles from 2020
wos_data <- wos_data %>%
  filter(year != 2020)

# count author_nationality
wos_data %>% count(author_nationality, sort = TRUE) %>% print(n=50)
# # A tibble: 560 x 2
# author_nationality        n
# <chr>                 <int>
# 1 Spain               14951
# 2 Italy               11893
# 3 France               5812
# 4 Greece               3444
# 5 Turkey               3004
# 6 NA                   2900
# 7 USA                  2510
# 8 Israel               2008
# 9 Portugal             1848
# 10 Germany             1843
# 11 United Kingdom      1560
# 12 Tunisia              876
# 13 Netherlands          634

# count fieldwork country
wos_data %>% count(fieldwork_country, sort = TRUE) %>% print(n=100)
# # A tibble: 1,331 x 2
# fieldwork_country                                     n
# <chr>                                             <int>
# 1 NA                                              32149
# 2 Spain                                            6769
# 3 Italy                                            3936
# 4 Turkey                                           2389
# 5 France                                           2027
# 6 Greece                                           1657
# 7 Israel                                           1227
# 8 Portugal                                         1014
# 9 Tunisia                                           922
# 10 Sicily                                           661 ####
# 11 Balearic Islands                                 590 ####
# 12 Egypt                                            558
# 13 Italy // Sicily                                  484 ####
# 14 Morocco                                          484
# 15 Italy // Sardinia                                433 ####
# 16 Sardinia                                         396 ####
# 17 Algeria                                          388
# 18 Crete                                            300 ####
# 19 Cyprus                                           300 ####
# 20 Croatia                                          278
# 21 Spain // Balearic Islands                        254 ####
# 22 Corsica                                          239 ####
# 23 Greece // Crete                                  223 ####
# 24 Lebanon                                          209
# 25 Syria                                            202
# 26 Portugal // Spain                                170
# 27 France // Spain                                  168
# 28 Malta                                            162 ####
# 29 France // Corsica                                161 ####

# the 5 islands are high in the ranking !


# count marine_region
wos_data %>% count(marine_region, sort = TRUE) %>% print(n=50)
# # A tibble: 281 x 2
# marine_region                                                                      n
# <chr>                                                                          <int>
# 1 NA                                                                           55133
# 2 Western Mediterranean Sea                                                     1116
# 3 Adriatic Sea                                                                   861
# 4 Aegean Sea                                                                     787
# 5 Levantine Sea                                                                  767
# 6 Northwestern Mediterranean Sea                                                 612
# 7 Tyrrhenian Sea                                                                 399
# 8 Strait of Gibraltar                                                            342
# 9 Ionian Sea                                                                     321
# 10 Ligurian Sea                                                                  300
# 11 Alboran Sea                                                                   255
# 12 Gulf of lion                                                                  255
# 13 Balearic Sea                                                                  176
# 14 Gulf of Gabes                                                                 172
# 15 Strait of Sicily                                                              147
# 16 Levantine Sea // Aegean Sea                                                   142


# seems good ! Lots of double or triple assignations after the 15 first simple assignations in the ranking

# cross marine_region and fieldwork country
wos_data %>% count(marine_region, fieldwork_country, sort = TRUE) %>% print(n=200)

# seems good ! most articles assigned to a marine region don't have an identified fieldwork country
# and the first crossings are logical (Aegean Sea + Greece, Gulf of Gabes + Tunisia, Western Mediterranean Sea + Spain, Adriatic Sea + Italy, ...)


### counting number of articles for the 13 taxonomic groups
# plant
wos_data %>% count(plant, sort = TRUE) %>% print(n=50)
# 1 NA    47351
# 2 plant 15730

# tree
wos_data %>% count(tree, sort = TRUE) %>% print(n=50)
# 1 NA    62652
# 2 tree    429

# tree + plant
wos_data %>% count(plant, tree, sort = TRUE) %>% print(n=50)
# # A tibble: 4 x 3
#   plant   tree       n
#   <chr>   <chr>  <int>
# 1 NA      NA     47176
# 2 plant   NA     15476
# 3 plant   tree     254
# 4 NA      tree     175 ### 175/429 -> 40% of articles assigned to "tree" are not assigned to "plant"

# fungi
wos_data %>% count(fungi, sort = TRUE) %>% print(n=50)
# 1 NA    61695
# 2 fungi  1386

# amphibian
wos_data %>% count(amphibian, sort = TRUE) %>% print(n=50)
# 1 NA        62751
# 2 amphibian   330

# reptile
wos_data %>% count(reptile, sort = TRUE) %>% print(n=50)
# 1 NA      61920
# 2 reptile  1161

# bird
wos_data %>% count(bird, sort = TRUE) %>% print(n=50)
# 1 NA    61144
# 2 bird   1937

# mammal
wos_data %>% count(mammal, sort = TRUE) %>% print(n=50)
# 1 NA     61366
# 2 mammal  1715

# coleoptera
wos_data %>% count(coleoptera, sort = TRUE) %>% print(n=50)
# 1 NA         62397
# 2 coleoptera   684

# papilionoidea
wos_data %>% count(papilionoidea, sort = TRUE) %>% print(n=50)
# 1 NA            62903
# 2 papilionoidea   178

# lumbricina
wos_data %>% count(lumbricina, sort = TRUE) %>% print(n=50)
# 1 NA         62986
# 2 lumbricina    95

# fish
wos_data %>% count(fish, sort = TRUE) %>% print(n=50)
# 1 NA    59147
# 2 fish   3934

# sponge
wos_data %>% count(sponge, sort = TRUE) %>% print(n=50)
# 1 NA     62462
# 2 sponge   619

# crustacea
wos_data %>% count(crustacea, sort = TRUE) %>% print(n=50)
# 1 NA        61471
# 2 crustacea  1610

### there seem to be a shift toward marine taxa, probably due to the strong presence of marine journals in the corpus of selected articles.
### is the reason that the keyword "mediterran*" is more often used in articles about marine systems than terrestrial system ? 
### Or that the publication production on marine systems if very important in the mediterranean basin ?


# count marine taxa and marine regions
wos_data %>% count(marine_region, fish, sort = TRUE) %>% print(n=50)
wos_data %>% count(marine_region, sponge, sort = TRUE) %>% print(n=50)
wos_data %>% count(marine_region, crustacea, sort = TRUE) %>% print(n=50)
# seems good


# cross tree and field_work country
wos_data %>% count(fieldwork_country, tree, sort = TRUE) %>% print(n=100)
# NA > Spain > Turkey > Italy ...


