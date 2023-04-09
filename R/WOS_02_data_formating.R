#################################################
#
# Biodivmex project
#
# WOS_02_data_formating.R
#
# jan.perret@cefe.cnrs.fr
#################################################


# load data
wos_data <- read_delim("./data/SIMPLE_v15_RESULT_WOS_files_merged_2022-02-14.csv", delim = ",", col_names = TRUE,
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
                         porifera = col_character(),
                         crustacea = col_character(),
                         coleoptera = col_character(),
                         papilionoidea = col_character(),
                         lumbricina = col_character(),
                         tree = col_character()
                       ))

# drop articles not written in english
wos_data <- wos_data %>%
  filter(language == "English")

# drop articles from 2021
wos_data <- wos_data %>%
  filter(year != 2021)

### corpus time truncation condition
truncation_year = 1989

wos_data <- wos_data %>%
  filter(year > truncation_year) # all articles published from 1990 to 2020
# NB : there was no article with year = NA anyway in the dataframe

# fix year factor levels for all taxa
year_list <- c((truncation_year + 1):2020)
wos_data$year <- factor(wos_data$year, levels = year_list, ordered = TRUE)

# drop articles from irrelevant journals 
journal_filtering <- read_delim("./data/WOS_journal_filtering_v2.csv", delim = ";", col_names = TRUE, 
                                col_types = cols(
                                  publisher = col_character()))

# nrow(wos_data[rowSums(is.na(wos_data[, c(9:21)])) != 13, ])/nrow(wos_data) # rate of articles assigned to at least one taxa
wos_data <- wos_data %>% 
  filter(!(publisher %in% journal_filtering$publisher))
# nrow(wos_data[rowSums(is.na(wos_data[, c(9:21)])) != 13, ])/nrow(wos_data) # same rate after filtering irrelevant journals

# drop articles about mediterranean regions outside the mediterranean basin
wos_data <- wos_data %>%
  filter(is.na(outside_med))

# drop the outside_med column
wos_data <- wos_data %>%
  select(-outside_med)

# remove duplicated rows
wos_data <- wos_data %>%
  distinct()
         

# ### random sample of 500 articles for classification error rates estimation
# set.seed(2020)
# wos_data_sample_error_rates <- wos_data[sample(nrow(wos_data), size = 500, replace = FALSE), ]
# 
# # load corpus file with complete metadata (e.g. titles, keywords and abstracts) and join to the sample table
# wos_data_complete <- read_tsv("./data/RESULT_WOS_files_merged.csv", col_names = TRUE, col_types = cols(.default = "c"))
# wos_data_complete <- wos_data_complete[, c(1:13)] # remove junk columns
# wos_data_sample_error_rates <- dplyr::inner_join(wos_data_sample_error_rates, wos_data_complete, by = c("access_num" = "accession_number"))
# # dim(wos_data_sample_error_rates)
# 
# # save table
# write_csv2(wos_data_sample_error_rates, file = "./output/text/WOS_sample_error_rates.csv", col_names = TRUE)


### get number of aticles at different steps of the filtering to make the filtering recap

# number of articles that passed "content filtering"
number_art_passing_content_filtering <- nrow(wos_data)
# 58,676

# number of articles assigned to a taxa of interest
number_art_taxa_of_interest <- 0

for (i in 1:length(wos_data$access_num)) {
  
  # test if the article is assigned to any taxa
  if (any(!is.na(wos_data[i , c(9:19, 21)]))) { # excluding lumbricina
    number_art_taxa_of_interest <- number_art_taxa_of_interest + 1
  }
}
# 27,055

# number of sequences assigned to a study area
number_art_study_area <- 0

for (i in 1:length(wos_data$access_num)) {
  
  # test if the article is assigned to any fieldwork_country
  if (!is.na(wos_data$fieldwork_country[i])) {
    number_art_study_area <- number_art_study_area + 1
  
  } else {
      
  # test if the article is assigned to any marine region
  if (!is.na(wos_data$marine_region[i])) {
    number_art_study_area <- number_art_study_area + 1
  }
 }
}
# 33,486


# number of articles with both (at least one geographic AND one taxonomic assignation)
number_art_both <- 0

for (i in 1:length(wos_data$access_num)) {
  
  # test if the article is assigned to any terrestrial taxa
  if (any(!is.na(wos_data[i , c(9:14, 18, 19, 21)]))) {
    
    # test if the article is assigned to any fieldwork_country
    if (!is.na(wos_data$fieldwork_country[i])) {
      number_art_both <- number_art_both + 1
    }
  } else {
    
    # test if the article is assigned to any marine taxa
    if (any(!is.na(wos_data[i , c(15, 16, 17)]))) {
      
      # test if the article is assigned to any marine region
      if (!is.na(wos_data$marine_region[i])) {
        number_art_both <- number_art_both + 1
      }
    }
  }
}
# 13,001 

# make recap table of the filtering steps
recap_titles <- c("Number of articles that passed content filtering",
                  "Number of articles assigned to a taxa of interest",
                  "Number of articles assigned to a study area",
                  "Number of articles assigned to both a taxa of interest and a study area")
num_art <- c(number_art_passing_content_filtering, number_art_taxa_of_interest, number_art_study_area, number_art_both) 
WOS_filtering_recap_table <- data.frame(recap_titles, num_art)
write_csv2(WOS_filtering_recap_table, file = "./output/text/WOS_filtering_recap.csv", col_names = TRUE)

### number of articles per journal
WOS_journal_table <- as.data.frame(table(wos_data$publisher))
colnames(WOS_journal_table) <- c("publisher","n_articles")
WOS_journal_table <- WOS_journal_table[with(WOS_journal_table, order(-n_articles)), ]

# save table
write_csv2(WOS_journal_table, file = "./output/text/WOS_recap_table_per_journal.csv", col_names = TRUE)


### number of articles on each taxonomic group
taxa_vect <- c("plant", "fungi", "amphibian", "reptile", "bird", "mammal", "fish", "porifera", "crustacea", "coleoptera", "papilionoidea", "lumbricina", "tree")

# initiaze table
WOS_taxa_table <- setNames(data.frame(matrix(ncol = 3, nrow = 13)), c("taxa", "n_articles", "loc_rate"))
WOS_taxa_table$taxa <- taxa_vect

# total number of articles in the corpus assigned to each taxonomic group
WOS_taxa_table$n_articles <- c(sum(wos_data$plant == "plant", na.rm = TRUE), 
                           sum(wos_data$fungi == "fungi", na.rm = TRUE),
                           sum(wos_data$amphibian == "amphibian", na.rm = TRUE),
                           sum(wos_data$reptile == "reptile", na.rm = TRUE),
                           sum(wos_data$bird == "bird", na.rm = TRUE),
                           sum(wos_data$mammal == "mammal", na.rm = TRUE),
                           sum(wos_data$fish == "fish", na.rm = TRUE),
                           sum(wos_data$porifera == "porifera", na.rm = TRUE),
                           sum(wos_data$crustacea == "crustacea", na.rm = TRUE),
                           sum(wos_data$coleoptera == "coleoptera", na.rm = TRUE),
                           sum(wos_data$papilionoidea == "papilionoidea", na.rm = TRUE),
                           sum(wos_data$lumbricina == "lumbricina", na.rm = TRUE),
                           sum(wos_data$tree == "tree", na.rm = TRUE)
)

# sub data frames to mesure fieldwork country assignation rate
plant_df <- subset(wos_data, wos_data$plant == "plant")
fungi_df <- subset(wos_data, wos_data$fungi == "fungi")
amph_df <- subset(wos_data, wos_data$amphibian == "amphibian")
rept_df <- subset(wos_data, wos_data$reptile == "reptile")
bird_df <- subset(wos_data, wos_data$bird == "bird")
mammal_df <- subset(wos_data, wos_data$mammal == "mammal")
fish_df <- subset(wos_data, wos_data$fish == "fish")
porifera_df <- subset(wos_data, wos_data$porifera == "porifera")
crusta_df <- subset(wos_data, wos_data$crustacea == "crustacea")
coleo_df <- subset(wos_data, wos_data$coleoptera == "coleoptera")
papilio_df <- subset(wos_data, wos_data$papilionoidea == "papilionoidea")
lumbri_df <- subset(wos_data, wos_data$lumbricina == "lumbricina")
tree_df <- subset(wos_data, wos_data$tree == "tree")

# fieldwork_country assignation rate (or same for marine_region for marine taxa)
WOS_taxa_table$loc_rate <- c(round((sum(!is.na(plant_df$fieldwork_country))/length(plant_df$access_num))*100, 2),
                         round((sum(!is.na(fungi_df$fieldwork_country))/length(fungi_df$access_num))*100, 2),
                         round((sum(!is.na(amph_df$fieldwork_country))/length(amph_df$access_num))*100, 2),
                         round((sum(!is.na(rept_df$fieldwork_country))/length(rept_df$access_num))*100, 2),
                         round((sum(!is.na(bird_df$fieldwork_country))/length(bird_df$access_num))*100, 2),
                         round((sum(!is.na(mammal_df$fieldwork_country))/length(mammal_df$access_num))*100, 2),
                         round((sum(!is.na(fish_df$marine_region))/length(fish_df$access_num))*100, 2),
                         round((sum(!is.na(porifera_df$marine_region))/length(porifera_df$access_num))*100, 2),
                         round((sum(!is.na(crusta_df$marine_region))/length(crusta_df$access_num))*100, 2),
                         round((sum(!is.na(coleo_df$fieldwork_country))/length(coleo_df$access_num))*100, 2),
                         round((sum(!is.na(papilio_df$fieldwork_country))/length(papilio_df$access_num))*100, 2),
                         round((sum(!is.na(lumbri_df$fieldwork_country))/length(lumbri_df$access_num))*100, 2),
                         round((sum(!is.na(tree_df$fieldwork_country))/length(tree_df$access_num))*100, 2)
                         )

# save table
write_csv2(WOS_taxa_table, file = "./output/text/WOS_recap_table_per_taxa.csv", col_names = TRUE)


### number of articles per year for each taxa
# initiaze table
taxa_year_table <- setNames(data.frame(matrix(ncol = length(year_list), nrow = 13)), c(paste0(year_list)))

# number of articles per year
taxa_year_table[1,] <- as.data.frame(table(plant_df$year))$Freq
taxa_year_table[2,] <- as.data.frame(table(fungi_df$year))$Freq
taxa_year_table[3,] <- as.data.frame(table(amph_df$year))$Freq
taxa_year_table[4,] <- as.data.frame(table(rept_df$year))$Freq
taxa_year_table[5,] <- as.data.frame(table(bird_df$year))$Freq
taxa_year_table[6,] <- as.data.frame(table(mammal_df$year))$Freq
taxa_year_table[7,] <- as.data.frame(table(fish_df$year))$Freq
taxa_year_table[8,] <- as.data.frame(table(porifera_df$year))$Freq
taxa_year_table[9,] <- as.data.frame(table(crusta_df$year))$Freq
taxa_year_table[10,] <- as.data.frame(table(coleo_df$year))$Freq
taxa_year_table[11,] <- as.data.frame(table(papilio_df$year))$Freq
taxa_year_table[12,] <- as.data.frame(table(lumbri_df$year))$Freq
taxa_year_table[13,] <- as.data.frame(table(tree_df$year))$Freq

# bind taxa name column
taxa_year_table <- cbind(taxa = taxa_vect, taxa_year_table)

# save table
write_csv2(taxa_year_table, file = "./output/text/WOS_recap_table_articles_per_year.csv", col_names = TRUE)

# passing from raw counts to accumulation
taxa_year_table_acc <- table_accumulate(year_tab = taxa_year_table, first_column = 2)

# save table
write_csv2(taxa_year_table_acc, file = "./output/text/WOS_recap_table_articles_per_year_acc.csv", col_names = TRUE)


### number of articles per country for each taxa
# first multiply rows with multiple assignations in fieldwork_country
# work on separate dataframe 
wos_data_div <- wos_data

# initiate variables
times = rep(NA, length(wos_data_div$access_num))
coun = list()
country = c()

# loop through the dataframe
for (i in 1:length(wos_data_div$access_num)){
  
  country <- as.vector(strsplit(wos_data_div$fieldwork_country[i], split = " // ")[[1]]) # extract multiple countries stored in fieldwork_country as a vector
  times[i] <- length(country) # number of different countries for given article
  coun <- rlist::list.append(coun, country) # append coun list with the countries stored in country object
  
}

times[times == 0] <- 1 # if there is no country name in fieldwork_country reference still count for one row

# replace positions of length 0 by NA
coun <- lapply(coun, function(x) if(identical(x, character(0))) NA_character_ else x)

# pass "coun" from list to vector
coun <- unlist(coun)

# repeat rows the number of times indicated in the vector "times"
wos_data_div <- wos_data_div[rep(seq_len(nrow(wos_data_div)), times),]
# so if there was zero or one value in fieldwork_country, the row won't be repeated, if there were 2 countries it will be repeated twice, etc.

# assign back unlisted fieldwork countries
wos_data_div$fieldwork_country <- coun

# fix fieldwork_country factor levels
country_list <- sort(unique(wos_data_div$fieldwork_country))
wos_data_div$fieldwork_country <- factor(wos_data_div$fieldwork_country, levels = country_list, ordered = FALSE)
wos_data_div$fieldwork_country <- fct_explicit_na(wos_data_div$fieldwork_country) # give fieldwork_country NAs an explicit value

# sub data frames per taxa
plant_country_df <- subset(wos_data_div, wos_data_div$plant == "plant")
fungi_country_df <- subset(wos_data_div, wos_data_div$fungi == "fungi")
amph_country_df <- subset(wos_data_div, wos_data_div$amphibian == "amphibian")
rept_country_df <- subset(wos_data_div, wos_data_div$reptile == "reptile")
bird_country_df <- subset(wos_data_div, wos_data_div$bird == "bird")
mammal_country_df <- subset(wos_data_div, wos_data_div$mammal == "mammal")
fish_country_df <- subset(wos_data_div, wos_data_div$fish == "fish")
porifera_country_df <- subset(wos_data_div, wos_data_div$porifera == "porifera")
crusta_country_df <- subset(wos_data_div, wos_data_div$crustacea == "crustacea")
coleo_country_df <- subset(wos_data_div, wos_data_div$coleoptera == "coleoptera")
papilio_country_df <- subset(wos_data_div, wos_data_div$papilionoidea == "papilionoidea")
lumbri_country_df <- subset(wos_data_div, wos_data_div$lumbricina == "lumbricina")
tree_country_df <- subset(wos_data_div, wos_data_div$tree == "tree")

# number of articles per country
# initiaze table
taxa_country_table <- setNames(data.frame(matrix(ncol = 13, nrow = length(country_list)+1)), c(paste0(taxa_vect)))

# number of articles per country
taxa_country_table[,1] <- as.data.frame(table(plant_country_df$fieldwork_country))$Freq
taxa_country_table[,2] <- as.data.frame(table(fungi_country_df$fieldwork_country))$Freq
taxa_country_table[,3] <- as.data.frame(table(amph_country_df$fieldwork_country))$Freq
taxa_country_table[,4] <- as.data.frame(table(rept_country_df$fieldwork_country))$Freq
taxa_country_table[,5] <- as.data.frame(table(bird_country_df$fieldwork_country))$Freq
taxa_country_table[,6] <- as.data.frame(table(mammal_country_df$fieldwork_country))$Freq
taxa_country_table[,7] <- as.data.frame(table(fish_country_df$fieldwork_country))$Freq
taxa_country_table[,8] <- as.data.frame(table(porifera_country_df$fieldwork_country))$Freq
taxa_country_table[,9] <- as.data.frame(table(crusta_country_df$fieldwork_country))$Freq
taxa_country_table[,10] <- as.data.frame(table(coleo_country_df$fieldwork_country))$Freq
taxa_country_table[,11] <- as.data.frame(table(papilio_country_df$fieldwork_country))$Freq
taxa_country_table[,12] <- as.data.frame(table(lumbri_country_df$fieldwork_country))$Freq
taxa_country_table[,13] <- as.data.frame(table(tree_country_df$fieldwork_country))$Freq

# bind country name column
taxa_country_table <- cbind(fieldwork_country = c(paste0(country_list), "NA"), taxa_country_table)

# save table
write_csv2(taxa_country_table, file = "./output/text/WOS_recap_table_articles_per_country.csv", col_names = TRUE)


### number of articles per year per country for each taxa
# country/year contingency table
plant_year_tab_per_country <- plant_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
fungi_year_tab_per_country <- fungi_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
amph_year_tab_per_country <- amph_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
rept_year_tab_per_country <- rept_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
bird_year_tab_per_country <- bird_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
mammal_year_tab_per_country <- mammal_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
coleo_year_tab_per_country <- coleo_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
lumbri_year_tab_per_country <- lumbri_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
papilio_year_tab_per_country <- papilio_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
porifera_year_tab_per_country <- porifera_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
crusta_year_tab_per_country <- crusta_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
fish_year_tab_per_country <- fish_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
tree_year_tab_per_country <- tree_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)

# save tables
write_csv2(plant_year_tab_per_country, file = "./output/text/WOS_plant_year_tab_per_country.csv", col_names = TRUE)
write_csv2(fungi_year_tab_per_country, file = "./output/text/WOS_fungi_year_tab_per_country.csv", col_names = TRUE)
write_csv2(amph_year_tab_per_country, file = "./output/text/WOS_amph_year_tab_per_country.csv", col_names = TRUE)
write_csv2(rept_year_tab_per_country, file = "./output/text/WOS_rept_year_tab_per_country.csv", col_names = TRUE)
write_csv2(bird_year_tab_per_country, file = "./output/text/WOS_bird_year_tab_per_country.csv", col_names = TRUE)
write_csv2(mammal_year_tab_per_country, file = "./output/text/WOS_mammal_year_tab_per_country.csv", col_names = TRUE)
write_csv2(coleo_year_tab_per_country, file = "./output/text/WOS_coleo_year_tab_per_country.csv", col_names = TRUE)
write_csv2(lumbri_year_tab_per_country, file = "./output/text/WOS_lumbri_year_tab_per_country.csv", col_names = TRUE)
write_csv2(papilio_year_tab_per_country, file = "./output/text/WOS_papilio_year_tab_per_country.csv", col_names = TRUE)
write_csv2(porifera_year_tab_per_country, file = "./output/text/WOS_porifera_year_tab_per_country.csv", col_names = TRUE)
write_csv2(crusta_year_tab_per_country, file = "./output/text/WOS_crusta_year_tab_per_country.csv", col_names = TRUE)
write_csv2(fish_year_tab_per_country, file = "./output/text/WOS_fish_year_tab_per_country.csv", col_names = TRUE)
write_csv2(tree_year_tab_per_country, file = "./output/text/WOS_tree_year_tab_per_country.csv", col_names = TRUE)

# passing from raw counts to accumulation tables
plant_year_tab_per_country_acc <- table_accumulate(year_tab = plant_year_tab_per_country, first_column = 2)
fungi_year_tab_per_country_acc <- table_accumulate(year_tab = fungi_year_tab_per_country, first_column = 2)
amph_year_tab_per_country_acc <- table_accumulate(year_tab = amph_year_tab_per_country, first_column = 2)
rept_year_tab_per_country_acc <- table_accumulate(year_tab = rept_year_tab_per_country, first_column = 2)
bird_year_tab_per_country_acc <- table_accumulate(year_tab = bird_year_tab_per_country, first_column = 2)
mammal_year_tab_per_country_acc <- table_accumulate(year_tab = mammal_year_tab_per_country, first_column = 2)
coleo_year_tab_per_country_acc <- table_accumulate(year_tab = coleo_year_tab_per_country, first_column = 2)
lumbri_year_tab_per_country_acc <- table_accumulate(year_tab = lumbri_year_tab_per_country, first_column = 2)
papilio_year_tab_per_country_acc <- table_accumulate(year_tab = papilio_year_tab_per_country, first_column = 2)
porifera_year_tab_per_country_acc <- table_accumulate(year_tab = porifera_year_tab_per_country, first_column = 2)
crusta_year_tab_per_country_acc <- table_accumulate(year_tab = crusta_year_tab_per_country, first_column = 2)
fish_year_tab_per_country_acc <- table_accumulate(year_tab = fish_year_tab_per_country, first_column = 2)
tree_year_tab_per_country_acc <- table_accumulate(year_tab = tree_year_tab_per_country, first_column = 2)

# save tables
write_csv2(plant_year_tab_per_country_acc, file = "./output/text/WOS_plant_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(fungi_year_tab_per_country_acc, file = "./output/text/WOS_fungi_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(amph_year_tab_per_country_acc, file = "./output/text/WOS_amph_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(rept_year_tab_per_country_acc, file = "./output/text/WOS_rept_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(bird_year_tab_per_country_acc, file = "./output/text/WOS_bird_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(mammal_year_tab_per_country_acc, file = "./output/text/WOS_mammal_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(coleo_year_tab_per_country_acc, file = "./output/text/WOS_coleo_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(lumbri_year_tab_per_country_acc, file = "./output/text/WOS_lumbri_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(papilio_year_tab_per_country_acc, file = "./output/text/WOS_papilio_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(porifera_year_tab_per_country_acc, file = "./output/text/WOS_porifera_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(crusta_year_tab_per_country_acc, file = "./output/text/WOS_crusta_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(fish_year_tab_per_country_acc, file = "./output/text/WOS_fish_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(tree_year_tab_per_country_acc, file = "./output/text/WOS_tree_year_tab_per_country_acc.csv", col_names = TRUE)


### number of articles per country with from_country / inside_med / outside_med corresponding author for each taxa
# add empty column "author_loc"
wos_data_div["author_loc"] <- NA
wos_data_div <- wos_data_div %>% mutate(author_loc = as.character(author_loc))

# fix factor levels for author_nationality
wos_data_div$author_nationality <- factor(wos_data_div$author_nationality, levels = unique(wos_data_div$author_nationality), ordered = FALSE)

# give missing values an explicit factor level to ensure that they appear in summaries and plots
wos_data_div$author_nationality <- fct_explicit_na(wos_data_div$author_nationality)

# loop to fill the column
# initialize variables
fdwk_country = ""
author_natio = ""
country_detection = FALSE

for (i in 1:length(wos_data_div$access_num)) {
  fdwk_country = as.character(wos_data_div$fieldwork_country[i])
  author_natio = as.character(wos_data_div$author_nationality[i])

  # detect if there is mediterranean country in author_natio
  for (coun_name in country_list) {
    if (str_detect(author_natio, coun_name)) {country_detection <- TRUE}
  }
  
  # to avoid bug in case of missing value
  if (author_natio == "(Missing)") {author_natio <- "XXXXXXXXXXXXXXXX"}
  if (fdwk_country == "(Missing)") {fdwk_country <- "ZZZZZZZZZZZZZZZZ"} # as NAs have an explicit value in wos_data_div$fieldwork_country we can't use is.na() 

  # to fill author loc for from_country authors
  if (str_detect(author_natio, fdwk_country)) {wos_data_div$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Balearic Islands" & str_detect(author_natio, "Spain")) {wos_data_div$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Corsica" & str_detect(author_natio, "France")) {wos_data_div$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Crete" & str_detect(author_natio, "Greece")) {wos_data_div$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Sardinia" & str_detect(author_natio, "Italy")) {wos_data_div$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Sicily" & str_detect(author_natio, "Italy")) {wos_data_div$author_loc[i] <- "from_country"}
  
  # for inside_med authors
  else if (country_detection == TRUE) {wos_data_div$author_loc[i] <- "inside_med"}
  
  # for outside_med authors
  else if (author_natio != "XXXXXXXXXXXXXXXX") {wos_data_div$author_loc[i] <- "outside_med"}
  
  # at the end of the loop set country_detection to FALSE again before next iteration
  country_detection <- FALSE
}

# fix factor levels
wos_data_div$author_loc <- factor(wos_data_div$author_loc, levels = c("from_country", "outside_med", "inside_med"), ordered = FALSE)

# give missing values an explicit factor level to ensure that they appear in summaries and plots
wos_data_div$author_loc <- fct_explicit_na(wos_data_div$author_loc)

# # test if missing values are still missing
# table(wos_data_div$author_nationality, useNA = "always")
# table(wos_data_div$author_loc, useNA = "always")

# sub data frames per taxa
plant_loc_df <- subset(wos_data_div, wos_data_div$plant == "plant")
fungi_loc_df <- subset(wos_data_div, wos_data_div$fungi == "fungi")
amph_loc_df <- subset(wos_data_div, wos_data_div$amphibian == "amphibian")
rept_loc_df <- subset(wos_data_div, wos_data_div$reptile == "reptile")
bird_loc_df <- subset(wos_data_div, wos_data_div$bird == "bird")
mammal_loc_df <- subset(wos_data_div, wos_data_div$mammal == "mammal")
fish_loc_df <- subset(wos_data_div, wos_data_div$fish == "fish")
porifera_loc_df <- subset(wos_data_div, wos_data_div$porifera == "porifera")
crusta_loc_df <- subset(wos_data_div, wos_data_div$crustacea == "crustacea")
coleo_loc_df <- subset(wos_data_div, wos_data_div$coleoptera == "coleoptera")
papilio_loc_df <- subset(wos_data_div, wos_data_div$papilionoidea == "papilionoidea")
lumbri_loc_df <- subset(wos_data_div, wos_data_div$lumbricina == "lumbricina")
tree_loc_df <- subset(wos_data_div, wos_data_div$tree == "tree")

# make recap tables
plant_article_loc_tab <- plant_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
fungi_article_loc_tab <- fungi_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
amph_article_loc_tab <- amph_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
rept_article_loc_tab <- rept_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
bird_article_loc_tab <- bird_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
mammal_article_loc_tab <- mammal_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
coleo_article_loc_tab <- coleo_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
lumbri_article_loc_tab <- lumbri_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
papilio_article_loc_tab <- papilio_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
porifera_article_loc_tab <- porifera_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
crusta_article_loc_tab <- crusta_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
fish_article_loc_tab <- fish_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
tree_article_loc_tab <- tree_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)

# add column with total number of sequences
plant_article_loc_tab <- cbind(plant_article_loc_tab, n_articles = plant_article_loc_tab$from_country + plant_article_loc_tab$inside_med + plant_article_loc_tab$outside_med + plant_article_loc_tab$`(Missing)`)
fungi_article_loc_tab <- cbind(fungi_article_loc_tab, n_articles = fungi_article_loc_tab$from_country + fungi_article_loc_tab$inside_med + fungi_article_loc_tab$outside_med + fungi_article_loc_tab$`(Missing)`)
amph_article_loc_tab <- cbind(amph_article_loc_tab, n_articles = amph_article_loc_tab$from_country + amph_article_loc_tab$inside_med + amph_article_loc_tab$outside_med + amph_article_loc_tab$`(Missing)`)
rept_article_loc_tab <- cbind(rept_article_loc_tab, n_articles = rept_article_loc_tab$from_country + rept_article_loc_tab$inside_med + rept_article_loc_tab$outside_med + rept_article_loc_tab$`(Missing)`)
bird_article_loc_tab <- cbind(bird_article_loc_tab, n_articles = bird_article_loc_tab$from_country + bird_article_loc_tab$inside_med + bird_article_loc_tab$outside_med + bird_article_loc_tab$`(Missing)`)
mammal_article_loc_tab <- cbind(mammal_article_loc_tab, n_articles = mammal_article_loc_tab$from_country + mammal_article_loc_tab$inside_med + mammal_article_loc_tab$outside_med + mammal_article_loc_tab$`(Missing)`)
coleo_article_loc_tab <- cbind(coleo_article_loc_tab, n_articles = coleo_article_loc_tab$from_country + coleo_article_loc_tab$inside_med + coleo_article_loc_tab$outside_med + coleo_article_loc_tab$`(Missing)`)
papilio_article_loc_tab <- cbind(papilio_article_loc_tab, n_articles = papilio_article_loc_tab$from_country + papilio_article_loc_tab$inside_med + papilio_article_loc_tab$outside_med + papilio_article_loc_tab$`(Missing)`)
lumbri_article_loc_tab <- cbind(lumbri_article_loc_tab, n_articles = lumbri_article_loc_tab$from_country + lumbri_article_loc_tab$inside_med + lumbri_article_loc_tab$outside_med + lumbri_article_loc_tab$`(Missing)`)
fish_article_loc_tab <- cbind(fish_article_loc_tab, n_articles = fish_article_loc_tab$from_country + fish_article_loc_tab$inside_med + fish_article_loc_tab$outside_med + fish_article_loc_tab$`(Missing)`)
porifera_article_loc_tab <- cbind(porifera_article_loc_tab, n_articles = porifera_article_loc_tab$from_country + porifera_article_loc_tab$inside_med + porifera_article_loc_tab$outside_med + porifera_article_loc_tab$`(Missing)`)
crusta_article_loc_tab <- cbind(crusta_article_loc_tab, n_articles = crusta_article_loc_tab$from_country + crusta_article_loc_tab$inside_med + crusta_article_loc_tab$outside_med + crusta_article_loc_tab$`(Missing)`)
tree_article_loc_tab <- cbind(tree_article_loc_tab, n_articles = tree_article_loc_tab$from_country + tree_article_loc_tab$inside_med + tree_article_loc_tab$outside_med + tree_article_loc_tab$`(Missing)`)

# remove Macedonia
plant_article_loc_tab <- subset(plant_article_loc_tab, plant_article_loc_tab$fieldwork_country != "Macedonia")
fungi_article_loc_tab <- subset(fungi_article_loc_tab, fungi_article_loc_tab$fieldwork_country != "Macedonia")
amph_article_loc_tab <- subset(amph_article_loc_tab,amph_article_loc_tab $fieldwork_country != "Macedonia")
rept_article_loc_tab <- subset(rept_article_loc_tab, rept_article_loc_tab$fieldwork_country != "Macedonia")
bird_article_loc_tab <- subset(bird_article_loc_tab, bird_article_loc_tab$fieldwork_country != "Macedonia")
mammal_article_loc_tab <- subset(mammal_article_loc_tab, mammal_article_loc_tab$fieldwork_country != "Macedonia")
coleo_article_loc_tab <- subset(coleo_article_loc_tab, coleo_article_loc_tab$fieldwork_country != "Macedonia")
papilio_article_loc_tab <- subset(papilio_article_loc_tab, papilio_article_loc_tab$fieldwork_country != "Macedonia")
lumbri_article_loc_tab <- subset(lumbri_article_loc_tab, lumbri_article_loc_tab$fieldwork_country != "Macedonia")
fish_article_loc_tab <- subset(fish_article_loc_tab, fish_article_loc_tab$fieldwork_country != "Macedonia")
porifera_article_loc_tab <- subset(porifera_article_loc_tab, porifera_article_loc_tab$fieldwork_country != "Macedonia")
crusta_article_loc_tab <- subset(crusta_article_loc_tab, crusta_article_loc_tab$fieldwork_country != "Macedonia")
tree_article_loc_tab <- subset(tree_article_loc_tab, tree_article_loc_tab$fieldwork_country != "Macedonia")

# save tables
write_csv2(plant_article_loc_tab, file = "./output/text/WOS_plant_article_loc_tab.csv", col_names = TRUE)
write_csv2(fungi_article_loc_tab, file = "./output/text/WOS_fungi_article_loc_tab.csv", col_names = TRUE)
write_csv2(amph_article_loc_tab, file = "./output/text/WOS_amph_article_loc_tab.csv", col_names = TRUE)
write_csv2(rept_article_loc_tab, file = "./output/text/WOS_rept_article_loc_tab.csv", col_names = TRUE)
write_csv2(bird_article_loc_tab, file = "./output/text/WOS_bird_article_loc_tab.csv", col_names = TRUE)
write_csv2(mammal_article_loc_tab, file = "./output/text/WOS_mammal_article_loc_tab.csv", col_names = TRUE)
write_csv2(coleo_article_loc_tab, file = "./output/text/WOS_coleo_article_loc_tab.csv", col_names = TRUE)
write_csv2(lumbri_article_loc_tab, file = "./output/text/WOS_lumbri_article_loc_tab.csv", col_names = TRUE)
write_csv2(papilio_article_loc_tab, file = "./output/text/WOS_papilio_article_loc_tab.csv", col_names = TRUE)
write_csv2(porifera_article_loc_tab, file = "./output/text/WOS_porifera_article_loc_tab.csv", col_names = TRUE)
write_csv2(crusta_article_loc_tab, file = "./output/text/WOS_crusta_article_loc_tab.csv", col_names = TRUE)
write_csv2(fish_article_loc_tab, file = "./output/text/WOS_fish_article_loc_tab.csv", col_names = TRUE)
write_csv2(tree_article_loc_tab, file = "./output/text/WOS_tree_article_loc_tab.csv", col_names = TRUE)

### number of different journals through the years for each taxa
# keep only document types "Article", "Correction" and "Review" because journal names are the most standardized
wos_data_journals <- wos_data %>% 
  filter(doc_type %in% c("Article", "Correction", "Review"))

# sub data frames per taxa
plant_journals_df <- subset(wos_data_journals, wos_data_journals$plant == "plant")
fungi_journals_df <- subset(wos_data_journals, wos_data_journals$fungi == "fungi")
amph_journals_df <- subset(wos_data_journals, wos_data_journals$amphibian == "amphibian")
rept_journals_df <- subset(wos_data_journals, wos_data_journals$reptile == "reptile")
bird_journals_df <- subset(wos_data_journals, wos_data_journals$bird == "bird")
mammal_journals_df <- subset(wos_data_journals, wos_data_journals$mammal == "mammal")
fish_journals_df <- subset(wos_data_journals, wos_data_journals$fish == "fish")
porifera_journals_df <- subset(wos_data_journals, wos_data_journals$porifera == "porifera")
crusta_journals_df <- subset(wos_data_journals, wos_data_journals$crustacea == "crustacea")
coleo_journals_df <- subset(wos_data_journals, wos_data_journals$coleoptera == "coleoptera")
papilio_journals_df <- subset(wos_data_journals, wos_data_journals$papilionoidea == "papilionoidea")
lumbri_journals_df <- subset(wos_data_journals, wos_data_journals$lumbricina == "lumbricina")
tree_journals_df <- subset(wos_data_journals, wos_data_journals$tree == "tree")

# initiaze table
taxa_journals_table <- setNames(data.frame(matrix(ncol = 4+length(year_list), nrow = 13)), nm = c("taxa", "tot_n_journals", "tot_articles", "mean_art_per_journal", paste0(year_list)))
taxa_journals_table$taxa <- taxa_vect

# total number of journals
taxa_journals_table$tot_n_journals <- c(length(unique(plant_journals_df$publisher)),
                                        length(unique(fungi_journals_df$publisher)),
                                        length(unique(amph_journals_df$publisher)),
                                        length(unique(rept_journals_df$publisher)),
                                        length(unique(bird_journals_df$publisher)),
                                        length(unique(mammal_journals_df$publisher)),
                                        length(unique(fish_journals_df$publisher)),
                                        length(unique(porifera_journals_df$publisher)),
                                        length(unique(crusta_journals_df$publisher)),
                                        length(unique(coleo_journals_df$publisher)),
                                        length(unique(papilio_journals_df$publisher)),
                                        length(unique(lumbri_journals_df$publisher)),
                                        length(unique(tree_journals_df$publisher))
                                        )

# total number of articles
taxa_journals_table$tot_articles <- c(length(plant_journals_df$access_num),
                                        length(fungi_journals_df$access_num),
                                        length(amph_journals_df$access_num),
                                        length(rept_journals_df$access_num),
                                        length(bird_journals_df$access_num),
                                        length(mammal_journals_df$access_num),
                                        length(fish_journals_df$access_num),
                                        length(porifera_journals_df$access_num),
                                        length(crusta_journals_df$access_num),
                                        length(coleo_journals_df$access_num),
                                        length(papilio_journals_df$access_num),
                                        length(lumbri_journals_df$access_num),
                                        length(tree_journals_df$access_num)
                                        )
# mean number of articles per journal
taxa_journals_table$mean_art_per_journal <- round(taxa_journals_table$tot_articles / taxa_journals_table$tot_n_journals, digits = 2)

# fill taxa_journals_table by indexing rows with taxa name and columns with year_list length
taxa_journals_table[which(taxa_journals_table$taxa == "plant"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = plant_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "fungi"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = fungi_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "amphibian"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = amph_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "reptile"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = rept_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "bird"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = bird_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "mammal"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = mammal_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "fish"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = fish_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "porifera"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = porifera_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "crustacea"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = crusta_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "coleoptera"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = coleo_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "papilionoidea"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = papilio_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "lumbricina"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = lumbri_journals_df)
taxa_journals_table[which(taxa_journals_table$taxa == "tree"), 5:(4+length(year_list))] <- WOS_count_journals(taxa_journals_data = tree_journals_df)

# save table
write_csv2(taxa_journals_table, file = "./output/text/WOS_taxa_journals_table.csv", col_names = TRUE)


### USE OF MARINE REGIONS INSTEAD OF COUNTRIES FOR MARINE TAXA ###

### number of articles per marine region for each taxa
# first multiply rows with multiple assignations in marine_region
# work on separate dataframe
wos_data_div_marine <- wos_data

# initiate variables
times = rep(NA, length(wos_data_div_marine$access_num))
reg = list()
region = c()

# loop through the dataframe
for (i in 1:length(wos_data_div_marine$access_num)){
  
  region <- as.vector(strsplit(wos_data_div_marine$marine_region[i], split = " // ")[[1]]) # extract multiple regions stored in marine_region as a vector
  times[i] <- length(region) # number of different regions for given article
  reg <- rlist::list.append(reg, region) # append reg list with the regions stored in region object
  
}

times[times == 0] <- 1 # if there is no region name in marine_region reference still count for one row

# replace positions of length 0 by NA
reg <- lapply(reg, function(x) if(identical(x, character(0))) NA_character_ else x)

# pass "reg" from list to vector
reg <- unlist(reg)

# repeat rows the number of times indicated in the vector "times"
wos_data_div_marine <- wos_data_div_marine[rep(seq_len(nrow(wos_data_div_marine)), times),]
# so if there was zero or one value in marine_region, the row won't be repeated, if there were 2 regions it will be repeated twice, etc.

# assign back unlisted regions
wos_data_div_marine$marine_region <- reg

# fix marine_region factor levels
region_list <- sort(unique(wos_data_div_marine$marine_region))
wos_data_div_marine$marine_region <- factor(wos_data_div_marine$marine_region, levels = region_list, ordered = FALSE)
wos_data_div_marine$marine_region <- fct_explicit_na(wos_data_div_marine$marine_region) # give marine_region NAs an explicit value

# sub data frames per taxa
fish_region_df <- subset(wos_data_div_marine, wos_data_div_marine$fish == "fish")
porifera_region_df <- subset(wos_data_div_marine, wos_data_div_marine$porifera == "porifera")
crusta_region_df <- subset(wos_data_div_marine, wos_data_div_marine$crustacea == "crustacea")

# number of articles per region
# initiate table
taxa_region_table <- setNames(data.frame(matrix(ncol = 3, nrow = length(region_list)+1)), c("fish", "porifera", "crustacea"))

# number of articles per region
taxa_region_table[,1] <- as.data.frame(table(fish_region_df$marine_region))$Freq
taxa_region_table[,2] <- as.data.frame(table(porifera_region_df$marine_region))$Freq
taxa_region_table[,3] <- as.data.frame(table(crusta_region_df$marine_region))$Freq

# bind region name column
taxa_region_table <- cbind(marine_region = c(paste0(region_list), "NA"), taxa_region_table)

# save table
write_csv2(taxa_region_table, file = "./output/text/WOS_recap_articles_per_marine_region.csv", col_names = TRUE)


### number of articles per year per marine region for each taxa (l.236)
# region/year contingency table
fish_year_tab_per_region <- fish_region_df %>% group_by(marine_region, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
porifera_year_tab_per_region <- porifera_region_df %>% group_by(marine_region, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
crusta_year_tab_per_region <- crusta_region_df %>% group_by(marine_region, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)

# save tables
write_csv2(fish_year_tab_per_region, file = "./output/text/WOS_fish_year_tab_per_region.csv", col_names = TRUE)
write_csv2(porifera_year_tab_per_region, file = "./output/text/WOS_porifera_year_tab_per_region.csv", col_names = TRUE)
write_csv2(crusta_year_tab_per_region, file = "./output/text/WOS_crusta_year_tab_per_region.csv", col_names = TRUE)

# passing from raw counts to accumulation tables
fish_year_tab_per_region_acc <- table_accumulate(year_tab = fish_year_tab_per_region, first_column = 2)
porifera_year_tab_per_region_acc <- table_accumulate(year_tab = porifera_year_tab_per_region, first_column = 2)
crusta_year_tab_per_region_acc <- table_accumulate(year_tab = crusta_year_tab_per_region, first_column = 2)

# save tables
write_csv2(fish_year_tab_per_region_acc, file = "./output/text/WOS_fish_year_tab_per_region_acc.csv", col_names = TRUE)
write_csv2(porifera_year_tab_per_region_acc, file = "./output/text/WOS_porifera_year_tab_per_region_acc.csv", col_names = TRUE)
write_csv2(crusta_year_tab_per_region_acc, file = "./output/text/WOS_crusta_year_tab_per_region_acc.csv", col_names = TRUE)


### number of articles per marine region with inside_med / outside_med corresponding author information (l.298)
# add empty column "author_loc"
wos_data_div_marine["author_loc"] <- NA
wos_data_div_marine <- wos_data_div_marine %>% mutate(author_loc = as.character(author_loc))

# fix factor levels for author_nationality
wos_data_div_marine$author_nationality <- factor(wos_data_div_marine$author_nationality, levels = unique(wos_data_div_marine$author_nationality), ordered = FALSE)

# give missing values an explicit factor level to ensure that they appear in summaries and plots
wos_data_div_marine$author_nationality <- fct_explicit_na(wos_data_div_marine$author_nationality)

# loop to fill the column
# initialize variables
mar_region = ""
author_natio = ""
region_detection = FALSE

for (i in 1:length(wos_data_div_marine$access_num)) {
  mar_region = as.character(wos_data_div_marine$marine_region[i])
  author_natio = as.character(wos_data_div_marine$author_nationality[i])
  
  # detect if there is mediterranean country in author_natio
  for (coun_name in country_list) {
    if (str_detect(author_natio, coun_name)) {region_detection <- TRUE}
  }
  
  # to avoid bug in case of missing value
  if (author_natio == "(Missing)") {author_natio <- "XXXXXXXXXXXXXXXX"}
  if (mar_region == "(Missing)") {mar_region <- "ZZZZZZZZZZZZZZZZ"} # as NAs have an explicit value in wos_data_div_marine$marine_region we can't use is.na() 
  
  # for inside_med authors
  if (region_detection == TRUE) {wos_data_div_marine$author_loc[i] <- "inside_med"}
  
  # for outside_med authors
  else if (author_natio != "XXXXXXXXXXXXXXXX") {wos_data_div_marine$author_loc[i] <- "outside_med"}
  
  # at the end of the loop set region_detection to FALSE again before next iteration
  region_detection <- FALSE
}

# fix factor levels
wos_data_div_marine$author_loc <- factor(wos_data_div_marine$author_loc, levels = c("outside_med", "inside_med"), ordered = FALSE)

# give missing values an explicit factor level to ensure that they appear in summaries and plots
wos_data_div_marine$author_loc <- fct_explicit_na(wos_data_div_marine$author_loc)

# # test if missing values are still missing
# table(wos_data_div_marine$author_nationality, useNA = "always")
# table(wos_data_div_marine$author_loc, useNA = "always")

# sub data frames per taxa
fish_loc_df <- subset(wos_data_div_marine, wos_data_div_marine$fish == "fish")
porifera_loc_df <- subset(wos_data_div_marine, wos_data_div_marine$porifera == "porifera")
crusta_loc_df <- subset(wos_data_div_marine, wos_data_div_marine$crustacea == "crustacea")

# make recap tables
porifera_article_loc_tab <- porifera_loc_df %>% group_by(marine_region, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
crusta_article_loc_tab <- crusta_loc_df %>% group_by(marine_region, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
fish_article_loc_tab <- fish_loc_df %>% group_by(marine_region, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)

# add column with total number of sequences
fish_article_loc_tab <- cbind(fish_article_loc_tab, n_articles = fish_article_loc_tab$inside_med + fish_article_loc_tab$outside_med + fish_article_loc_tab$`(Missing)`)
porifera_article_loc_tab <- cbind(porifera_article_loc_tab, n_articles = porifera_article_loc_tab$inside_med + porifera_article_loc_tab$outside_med + porifera_article_loc_tab$`(Missing)`)
crusta_article_loc_tab <- cbind(crusta_article_loc_tab, n_articles = crusta_article_loc_tab$inside_med + crusta_article_loc_tab$outside_med + crusta_article_loc_tab$`(Missing)`)

# save tables
write_csv2(porifera_article_loc_tab, file = "./output/text/WOS_porifera_article_loc_tab.csv", col_names = TRUE)
write_csv2(crusta_article_loc_tab, file = "./output/text/WOS_crusta_article_loc_tab.csv", col_names = TRUE)
write_csv2(fish_article_loc_tab, file = "./output/text/WOS_fish_article_loc_tab.csv", col_names = TRUE)


#### Values for the Table 1 of the main text ####
# View(wos_data)
vect_myers_taxa <- c("plant", "amphibian", "reptile", "bird", "mammal")
vect_other_taxa <- c("fungi", "coleoptera", "papilionoidea")

# portugal
df_portugal <- subset(wos_data, grepl("Portugal", wos_data$fieldwork_country))
df_portugal_myers <- subset(df_portugal, !is.na(df_portugal$plant) | !is.na(df_portugal$amphibian) | !is.na(df_portugal$reptile) | !is.na(df_portugal$bird) | !is.na(df_portugal$mammal))
df_portugal_other <- subset(df_portugal, !is.na(df_portugal$fungi) | !is.na(df_portugal$coleoptera) | !is.na(df_portugal$papilionoidea))
nrow(df_portugal_myers); (nrow(df_portugal_myers)/13001)*100
nrow(df_portugal_other); (nrow(df_portugal_other)/13001)*100
df_portugal_all_taxa <- subset(df_portugal, !is.na(df_portugal$plant) | !is.na(df_portugal$amphibian) | !is.na(df_portugal$reptile) | !is.na(df_portugal$bird) | !is.na(df_portugal$mammal) | !is.na(df_portugal$fungi) | !is.na(df_portugal$coleoptera) | !is.na(df_portugal$papilionoidea))
nrow(df_portugal_all_taxa)

# spain
df_spain <- subset(wos_data, grepl("Spain", wos_data$fieldwork_country) | grepl("Balearic Islands", wos_data$fieldwork_country))
df_spain_myers <- subset(df_spain, !is.na(df_spain$plant) | !is.na(df_spain$amphibian) | !is.na(df_spain$reptile) | !is.na(df_spain$bird) | !is.na(df_spain$mammal))
df_spain_other <- subset(df_spain, !is.na(df_spain$fungi) | !is.na(df_spain$coleoptera) | !is.na(df_spain$papilionoidea))
nrow(df_spain_myers); (nrow(df_spain_myers)/13001)*100
nrow(df_spain_other); (nrow(df_spain_other)/13001)*100
df_spain_all_taxa <- subset(df_spain, !is.na(df_spain$plant) | !is.na(df_spain$amphibian) | !is.na(df_spain$reptile) | !is.na(df_spain$bird) | !is.na(df_spain$mammal) | !is.na(df_spain$fungi) | !is.na(df_spain$coleoptera) | !is.na(df_spain$papilionoidea))
nrow(df_spain_all_taxa)

# france
df_france <- subset(wos_data, grepl("France", wos_data$fieldwork_country) | grepl("Corsica", wos_data$fieldwork_country))
df_france_myers <- subset(df_france, !is.na(df_france$plant) | !is.na(df_france$amphibian) | !is.na(df_france$reptile) | !is.na(df_france$bird) | !is.na(df_france$mammal))
df_france_other <- subset(df_france, !is.na(df_france$fungi) | !is.na(df_france$coleoptera) | !is.na(df_france$papilionoidea))
nrow(df_france_myers); (nrow(df_france_myers)/13001)*100
nrow(df_france_other); (nrow(df_france_other)/13001)*100
df_france_all_taxa <- subset(df_france, !is.na(df_france$plant) | !is.na(df_france$amphibian) | !is.na(df_france$reptile) | !is.na(df_france$bird) | !is.na(df_france$mammal) | !is.na(df_france$fungi) | !is.na(df_france$coleoptera) | !is.na(df_france$papilionoidea))
nrow(df_france_all_taxa)

# italy
df_italy <- subset(wos_data, grepl("Italy", wos_data$fieldwork_country) | grepl("Sicily", wos_data$fieldwork_country) | grepl("Sardinia", wos_data$fieldwork_country))
df_italy_myers <- subset(df_italy, !is.na(df_italy$plant) | !is.na(df_italy$amphibian) | !is.na(df_italy$reptile) | !is.na(df_italy$bird) | !is.na(df_italy$mammal))
df_italy_other <- subset(df_italy, !is.na(df_italy$fungi) | !is.na(df_italy$coleoptera) | !is.na(df_italy$papilionoidea))
nrow(df_italy_myers); (nrow(df_italy_myers)/13001)*100
nrow(df_italy_other); (nrow(df_italy_other)/13001)*100
df_italy_all_taxa <- subset(df_italy, !is.na(df_italy$plant) | !is.na(df_italy$amphibian) | !is.na(df_italy$reptile) | !is.na(df_italy$bird) | !is.na(df_italy$mammal) | !is.na(df_italy$fungi) | !is.na(df_italy$coleoptera) | !is.na(df_italy$papilionoidea))
nrow(df_italy_all_taxa)

# North Western
df_north_western <- subset(wos_data, grepl("Portugal", wos_data$fieldwork_country) | grepl("Spain", wos_data$fieldwork_country) | grepl("Balearic Islands", wos_data$fieldwork_country) | grepl("France", wos_data$fieldwork_country) | grepl("Corsica", wos_data$fieldwork_country) | grepl("Italy", wos_data$fieldwork_country) | grepl("Sicily", wos_data$fieldwork_country) | grepl("Sardinia", wos_data$fieldwork_country))
df_north_western_myers <- subset(df_north_western, !is.na(df_north_western$plant) | !is.na(df_north_western$amphibian) | !is.na(df_north_western$reptile) | !is.na(df_north_western$bird) | !is.na(df_north_western$mammal))
df_north_western_other <- subset(df_north_western, !is.na(df_north_western$fungi) | !is.na(df_north_western$coleoptera) | !is.na(df_north_western$papilionoidea))
nrow(df_north_western_myers); (nrow(df_north_western_myers)/13001)*100
nrow(df_north_western_other); (nrow(df_north_western_other)/13001)*100

# Croatia
df_croatia <- subset(wos_data, grepl("Croatia", wos_data$fieldwork_country))
df_croatia_myers <- subset(df_croatia, !is.na(df_croatia$plant) | !is.na(df_croatia$amphibian) | !is.na(df_croatia$reptile) | !is.na(df_croatia$bird) | !is.na(df_croatia$mammal))
df_croatia_other <- subset(df_croatia, !is.na(df_croatia$fungi) | !is.na(df_croatia$coleoptera) | !is.na(df_croatia$papilionoidea))
nrow(df_croatia_myers); (nrow(df_croatia_myers)/13001)*100
nrow(df_croatia_other); (nrow(df_croatia_other)/13001)*100
df_croatia_all_taxa <- subset(df_croatia, !is.na(df_croatia$plant) | !is.na(df_croatia$amphibian) | !is.na(df_croatia$reptile) | !is.na(df_croatia$bird) | !is.na(df_croatia$mammal) | !is.na(df_croatia$fungi) | !is.na(df_croatia$coleoptera) | !is.na(df_croatia$papilionoidea))
nrow(df_croatia_all_taxa)

# Slovenia
df_slovenia <- subset(wos_data, grepl("Slovenia", wos_data$fieldwork_country))
df_slovenia_myers <- subset(df_slovenia, !is.na(df_slovenia$plant) | !is.na(df_slovenia$amphibian) | !is.na(df_slovenia$reptile) | !is.na(df_slovenia$bird) | !is.na(df_slovenia$mammal))
df_slovenia_other <- subset(df_slovenia, !is.na(df_slovenia$fungi) | !is.na(df_slovenia$coleoptera) | !is.na(df_slovenia$papilionoidea))
nrow(df_slovenia_myers); (nrow(df_slovenia_myers)/13001)*100
nrow(df_slovenia_other); (nrow(df_slovenia_other)/13001)*100
df_slovenia_all_taxa <- subset(df_slovenia, !is.na(df_slovenia$plant) | !is.na(df_slovenia$amphibian) | !is.na(df_slovenia$reptile) | !is.na(df_slovenia$bird) | !is.na(df_slovenia$mammal) | !is.na(df_slovenia$fungi) | !is.na(df_slovenia$coleoptera) | !is.na(df_slovenia$papilionoidea))
nrow(df_slovenia_all_taxa)

# Montenegro
df_montenegro <- subset(wos_data, grepl("Montenegro", wos_data$fieldwork_country))
df_montenegro_myers <- subset(df_montenegro, !is.na(df_montenegro$plant) | !is.na(df_montenegro$amphibian) | !is.na(df_montenegro$reptile) | !is.na(df_montenegro$bird) | !is.na(df_montenegro$mammal))
df_montenegro_other <- subset(df_montenegro, !is.na(df_montenegro$fungi) | !is.na(df_montenegro$coleoptera) | !is.na(df_montenegro$papilionoidea))
nrow(df_montenegro_myers); (nrow(df_montenegro_myers)/13001)*100
nrow(df_montenegro_other); (nrow(df_montenegro_other)/13001)*100
df_montenegro_all_taxa <- subset(df_montenegro, !is.na(df_montenegro$plant) | !is.na(df_montenegro$amphibian) | !is.na(df_montenegro$reptile) | !is.na(df_montenegro$bird) | !is.na(df_montenegro$mammal) | !is.na(df_montenegro$fungi) | !is.na(df_montenegro$coleoptera) | !is.na(df_montenegro$papilionoidea))
nrow(df_montenegro_all_taxa)

# Albania
df_albania <- subset(wos_data, grepl("Albania", wos_data$fieldwork_country))
df_albania_myers <- subset(df_albania, !is.na(df_albania$plant) | !is.na(df_albania$amphibian) | !is.na(df_albania$reptile) | !is.na(df_albania$bird) | !is.na(df_albania$mammal))
df_albania_other <- subset(df_albania, !is.na(df_albania$fungi) | !is.na(df_albania$coleoptera) | !is.na(df_albania$papilionoidea))
nrow(df_albania_myers); (nrow(df_albania_myers)/13001)*100
nrow(df_albania_other); (nrow(df_albania_other)/13001)*100
df_albania_all_taxa <- subset(df_albania, !is.na(df_albania$plant) | !is.na(df_albania$amphibian) | !is.na(df_albania$reptile) | !is.na(df_albania$bird) | !is.na(df_albania$mammal) | !is.na(df_albania$fungi) | !is.na(df_albania$coleoptera) | !is.na(df_albania$papilionoidea))
nrow(df_albania_all_taxa)

# Bosnia and Herzegovina
df_bosnia <- subset(wos_data, grepl("Bosnia", wos_data$fieldwork_country))
df_bosnia_myers <- subset(df_bosnia, !is.na(df_bosnia$plant) | !is.na(df_bosnia$amphibian) | !is.na(df_bosnia$reptile) | !is.na(df_bosnia$bird) | !is.na(df_bosnia$mammal))
df_bosnia_other <- subset(df_bosnia, !is.na(df_bosnia$fungi) | !is.na(df_bosnia$coleoptera) | !is.na(df_bosnia$papilionoidea))
nrow(df_bosnia_myers); (nrow(df_bosnia_myers)/13001)*100
nrow(df_bosnia_other); (nrow(df_bosnia_other)/13001)*100
df_bosnia_all_taxa <- subset(df_bosnia, !is.na(df_bosnia$plant) | !is.na(df_bosnia$amphibian) | !is.na(df_bosnia$reptile) | !is.na(df_bosnia$bird) | !is.na(df_bosnia$mammal) | !is.na(df_bosnia$fungi) | !is.na(df_bosnia$coleoptera) | !is.na(df_bosnia$papilionoidea))
nrow(df_bosnia_all_taxa)

# Balkans
df_balkans <- subset(wos_data, grepl("Croatia", wos_data$fieldwork_country) | grepl("Slovenia", wos_data$fieldwork_country) | grepl("Montenegro", wos_data$fieldwork_country) | grepl("Albania", wos_data$fieldwork_country) | grepl("Bosnia", wos_data$fieldwork_country))
df_balkans_myers <- subset(df_balkans, !is.na(df_balkans$plant) | !is.na(df_balkans$amphibian) | !is.na(df_balkans$reptile) | !is.na(df_balkans$bird) | !is.na(df_balkans$mammal))
df_balkans_other <- subset(df_balkans, !is.na(df_balkans$fungi) | !is.na(df_balkans$coleoptera) | !is.na(df_balkans$papilionoidea))
nrow(df_balkans_myers); (nrow(df_balkans_myers)/13001)*100
nrow(df_balkans_other); (nrow(df_balkans_other)/13001)*100

# Algeria
df_algeria <- subset(wos_data, grepl("Algeria", wos_data$fieldwork_country))
df_algeria_myers <- subset(df_algeria, !is.na(df_algeria$plant) | !is.na(df_algeria$amphibian) | !is.na(df_algeria$reptile) | !is.na(df_algeria$bird) | !is.na(df_algeria$mammal))
df_algeria_other <- subset(df_algeria, !is.na(df_algeria$fungi) | !is.na(df_algeria$coleoptera) | !is.na(df_algeria$papilionoidea))
nrow(df_algeria_myers); (nrow(df_algeria_myers)/13001)*100
nrow(df_algeria_other); (nrow(df_algeria_other)/13001)*100
df_algeria_all_taxa <- subset(df_algeria, !is.na(df_algeria$plant) | !is.na(df_algeria$amphibian) | !is.na(df_algeria$reptile) | !is.na(df_algeria$bird) | !is.na(df_algeria$mammal) | !is.na(df_algeria$fungi) | !is.na(df_algeria$coleoptera) | !is.na(df_algeria$papilionoidea))
nrow(df_algeria_all_taxa)

# Tunisia
df_tunisia <- subset(wos_data, grepl("Tunisia", wos_data$fieldwork_country))
df_tunisia_myers <- subset(df_tunisia, !is.na(df_tunisia$plant) | !is.na(df_tunisia$amphibian) | !is.na(df_tunisia$reptile) | !is.na(df_tunisia$bird) | !is.na(df_tunisia$mammal))
df_tunisia_other <- subset(df_tunisia, !is.na(df_tunisia$fungi) | !is.na(df_tunisia$coleoptera) | !is.na(df_tunisia$papilionoidea))
nrow(df_tunisia_myers); (nrow(df_tunisia_myers)/13001)*100
nrow(df_tunisia_other); (nrow(df_tunisia_other)/13001)*100
df_tunisia_all_taxa <- subset(df_tunisia, !is.na(df_tunisia$plant) | !is.na(df_tunisia$amphibian) | !is.na(df_tunisia$reptile) | !is.na(df_tunisia$bird) | !is.na(df_tunisia$mammal) | !is.na(df_tunisia$fungi) | !is.na(df_tunisia$coleoptera) | !is.na(df_tunisia$papilionoidea))
nrow(df_tunisia_all_taxa)

# Malta
df_malta <- subset(wos_data, grepl("Malta", wos_data$fieldwork_country))
df_malta_myers <- subset(df_malta, !is.na(df_malta$plant) | !is.na(df_malta$amphibian) | !is.na(df_malta$reptile) | !is.na(df_malta$bird) | !is.na(df_malta$mammal))
df_malta_other <- subset(df_malta, !is.na(df_malta$fungi) | !is.na(df_malta$coleoptera) | !is.na(df_malta$papilionoidea))
nrow(df_malta_myers); (nrow(df_malta_myers)/13001)*100
nrow(df_malta_other); (nrow(df_malta_other)/13001)*100
df_malta_all_taxa <- subset(df_malta, !is.na(df_malta$plant) | !is.na(df_malta$amphibian) | !is.na(df_malta$reptile) | !is.na(df_malta$bird) | !is.na(df_malta$mammal) | !is.na(df_malta$fungi) | !is.na(df_malta$coleoptera) | !is.na(df_malta$papilionoidea))
nrow(df_malta_all_taxa)

# Egypt
df_egypt <- subset(wos_data, grepl("Egypt", wos_data$fieldwork_country))
df_egypt_myers <- subset(df_egypt, !is.na(df_egypt$plant) | !is.na(df_egypt$amphibian) | !is.na(df_egypt$reptile) | !is.na(df_egypt$bird) | !is.na(df_egypt$mammal))
df_egypt_other <- subset(df_egypt, !is.na(df_egypt$fungi) | !is.na(df_egypt$coleoptera) | !is.na(df_egypt$papilionoidea))
nrow(df_egypt_myers); (nrow(df_egypt_myers)/13001)*100
nrow(df_egypt_other); (nrow(df_egypt_other)/13001)*100
df_egypt_all_taxa <- subset(df_egypt, !is.na(df_egypt$plant) | !is.na(df_egypt$amphibian) | !is.na(df_egypt$reptile) | !is.na(df_egypt$bird) | !is.na(df_egypt$mammal) | !is.na(df_egypt$fungi) | !is.na(df_egypt$coleoptera) | !is.na(df_egypt$papilionoidea))
nrow(df_egypt_all_taxa)

# Libya
df_libya <- subset(wos_data, grepl("Libya", wos_data$fieldwork_country))
df_libya_myers <- subset(df_libya, !is.na(df_libya$plant) | !is.na(df_libya$amphibian) | !is.na(df_libya$reptile) | !is.na(df_libya$bird) | !is.na(df_libya$mammal))
df_libya_other <- subset(df_libya, !is.na(df_libya$fungi) | !is.na(df_libya$coleoptera) | !is.na(df_libya$papilionoidea))
nrow(df_libya_myers); (nrow(df_libya_myers)/13001)*100
nrow(df_libya_other); (nrow(df_libya_other)/13001)*100
df_libya_all_taxa <- subset(df_libya, !is.na(df_libya$plant) | !is.na(df_libya$amphibian) | !is.na(df_libya$reptile) | !is.na(df_libya$bird) | !is.na(df_libya$mammal) | !is.na(df_libya$fungi) | !is.na(df_libya$coleoptera) | !is.na(df_libya$papilionoidea))
nrow(df_libya_all_taxa)

# Morocco
df_morocco <- subset(wos_data, grepl("Morocco", wos_data$fieldwork_country))
df_morocco_myers <- subset(df_morocco, !is.na(df_morocco$plant) | !is.na(df_morocco$amphibian) | !is.na(df_morocco$reptile) | !is.na(df_morocco$bird) | !is.na(df_morocco$mammal))
df_morocco_other <- subset(df_morocco, !is.na(df_morocco$fungi) | !is.na(df_morocco$coleoptera) | !is.na(df_morocco$papilionoidea))
nrow(df_morocco_myers); (nrow(df_morocco_myers)/13001)*100
nrow(df_morocco_other); (nrow(df_morocco_other)/13001)*100
df_morocco_all_taxa <- subset(df_morocco, !is.na(df_morocco$plant) | !is.na(df_morocco$amphibian) | !is.na(df_morocco$reptile) | !is.na(df_morocco$bird) | !is.na(df_morocco$mammal) | !is.na(df_morocco$fungi) | !is.na(df_morocco$coleoptera) | !is.na(df_morocco$papilionoidea))
nrow(df_morocco_all_taxa)

# Southern
df_southern <- subset(wos_data, grepl("Algeria", wos_data$fieldwork_country) | grepl("Tunisia", wos_data$fieldwork_country) | grepl("Malta", wos_data$fieldwork_country) | grepl("Egypt", wos_data$fieldwork_country) | grepl("Libya", wos_data$fieldwork_country) | grepl("Morocco", wos_data$fieldwork_country))
df_southern_myers <- subset(df_southern, !is.na(df_southern$plant) | !is.na(df_southern$amphibian) | !is.na(df_southern$reptile) | !is.na(df_southern$bird) | !is.na(df_southern$mammal))
df_southern_other <- subset(df_southern, !is.na(df_southern$fungi) | !is.na(df_southern$coleoptera) | !is.na(df_southern$papilionoidea))
nrow(df_southern_myers); (nrow(df_southern_myers)/13001)*100
nrow(df_southern_other); (nrow(df_southern_other)/13001)*100

# Greece
df_greece <- subset(wos_data, grepl("Greece", wos_data$fieldwork_country) | grepl("Crete", wos_data$fieldwork_country))
df_greece_myers <- subset(df_greece, !is.na(df_greece$plant) | !is.na(df_greece$amphibian) | !is.na(df_greece$reptile) | !is.na(df_greece$bird) | !is.na(df_greece$mammal))
df_greece_other <- subset(df_greece, !is.na(df_greece$fungi) | !is.na(df_greece$coleoptera) | !is.na(df_greece$papilionoidea))
nrow(df_greece_myers); (nrow(df_greece_myers)/13001)*100
nrow(df_greece_other); (nrow(df_greece_other)/13001)*100
df_greece_all_taxa <- subset(df_greece, !is.na(df_greece$plant) | !is.na(df_greece$amphibian) | !is.na(df_greece$reptile) | !is.na(df_greece$bird) | !is.na(df_greece$mammal) | !is.na(df_greece$fungi) | !is.na(df_greece$coleoptera) | !is.na(df_greece$papilionoidea))
nrow(df_greece_all_taxa)

# Turkey
df_turkey <- subset(wos_data, grepl("Turkey", wos_data$fieldwork_country))
df_turkey_myers <- subset(df_turkey, !is.na(df_turkey$plant) | !is.na(df_turkey$amphibian) | !is.na(df_turkey$reptile) | !is.na(df_turkey$bird) | !is.na(df_turkey$mammal))
df_turkey_other <- subset(df_turkey, !is.na(df_turkey$fungi) | !is.na(df_turkey$coleoptera) | !is.na(df_turkey$papilionoidea))
nrow(df_turkey_myers); (nrow(df_turkey_myers)/13001)*100
nrow(df_turkey_other); (nrow(df_turkey_other)/13001)*100
df_turkey_all_taxa <- subset(df_turkey, !is.na(df_turkey$plant) | !is.na(df_turkey$amphibian) | !is.na(df_turkey$reptile) | !is.na(df_turkey$bird) | !is.na(df_turkey$mammal) | !is.na(df_turkey$fungi) | !is.na(df_turkey$coleoptera) | !is.na(df_turkey$papilionoidea))
nrow(df_turkey_all_taxa)

# North Eastern
df_north_eastern <- subset(wos_data, grepl("Greece", wos_data$fieldwork_country) | grepl("Crete", wos_data$fieldwork_country) | grepl("Turkey", wos_data$fieldwork_country))
df_north_eastern_myers <- subset(df_north_eastern, !is.na(df_north_eastern$plant) | !is.na(df_north_eastern$amphibian) | !is.na(df_north_eastern$reptile) | !is.na(df_north_eastern$bird) | !is.na(df_north_eastern$mammal))
df_north_eastern_other <- subset(df_north_eastern, !is.na(df_north_eastern$fungi) | !is.na(df_north_eastern$coleoptera) | !is.na(df_north_eastern$papilionoidea))
nrow(df_north_eastern_myers); (nrow(df_north_eastern_myers)/13001)*100
nrow(df_north_eastern_other); (nrow(df_north_eastern_other)/13001)*100

# Israel
df_israel <- subset(wos_data, grepl("Israel", wos_data$fieldwork_country))
df_israel_myers <- subset(df_israel, !is.na(df_israel$plant) | !is.na(df_israel$amphibian) | !is.na(df_israel$reptile) | !is.na(df_israel$bird) | !is.na(df_israel$mammal))
df_israel_other <- subset(df_israel, !is.na(df_israel$fungi) | !is.na(df_israel$coleoptera) | !is.na(df_israel$papilionoidea))
nrow(df_israel_myers); (nrow(df_israel_myers)/13001)*100
nrow(df_israel_other); (nrow(df_israel_other)/13001)*100
df_israel_all_taxa <- subset(df_israel, !is.na(df_israel$plant) | !is.na(df_israel$amphibian) | !is.na(df_israel$reptile) | !is.na(df_israel$bird) | !is.na(df_israel$mammal) | !is.na(df_israel$fungi) | !is.na(df_israel$coleoptera) | !is.na(df_israel$papilionoidea))
nrow(df_israel_all_taxa)

# Cyprus
df_cyprus <- subset(wos_data, grepl("Cyprus", wos_data$fieldwork_country))
df_cyprus_myers <- subset(df_cyprus, !is.na(df_cyprus$plant) | !is.na(df_cyprus$amphibian) | !is.na(df_cyprus$reptile) | !is.na(df_cyprus$bird) | !is.na(df_cyprus$mammal))
df_cyprus_other <- subset(df_cyprus, !is.na(df_cyprus$fungi) | !is.na(df_cyprus$coleoptera) | !is.na(df_cyprus$papilionoidea))
nrow(df_cyprus_myers); (nrow(df_cyprus_myers)/13001)*100
nrow(df_cyprus_other); (nrow(df_cyprus_other)/13001)*100
df_cyprus_all_taxa <- subset(df_cyprus, !is.na(df_cyprus$plant) | !is.na(df_cyprus$amphibian) | !is.na(df_cyprus$reptile) | !is.na(df_cyprus$bird) | !is.na(df_cyprus$mammal) | !is.na(df_cyprus$fungi) | !is.na(df_cyprus$coleoptera) | !is.na(df_cyprus$papilionoidea))
nrow(df_cyprus_all_taxa)

# Lebanon
df_lebanon <- subset(wos_data, grepl("Lebanon", wos_data$fieldwork_country))
df_lebanon_myers <- subset(df_lebanon, !is.na(df_lebanon$plant) | !is.na(df_lebanon$amphibian) | !is.na(df_lebanon$reptile) | !is.na(df_lebanon$bird) | !is.na(df_lebanon$mammal))
df_lebanon_other <- subset(df_lebanon, !is.na(df_lebanon$fungi) | !is.na(df_lebanon$coleoptera) | !is.na(df_lebanon$papilionoidea))
nrow(df_lebanon_myers); (nrow(df_lebanon_myers)/13001)*100
nrow(df_lebanon_other); (nrow(df_lebanon_other)/13001)*100
df_lebanon_all_taxa <- subset(df_lebanon, !is.na(df_lebanon$plant) | !is.na(df_lebanon$amphibian) | !is.na(df_lebanon$reptile) | !is.na(df_lebanon$bird) | !is.na(df_lebanon$mammal) | !is.na(df_lebanon$fungi) | !is.na(df_lebanon$coleoptera) | !is.na(df_lebanon$papilionoidea))
nrow(df_lebanon_all_taxa)

# Syria
df_syria <- subset(wos_data, grepl("Syria", wos_data$fieldwork_country))
df_syria_myers <- subset(df_syria, !is.na(df_syria$plant) | !is.na(df_syria$amphibian) | !is.na(df_syria$reptile) | !is.na(df_syria$bird) | !is.na(df_syria$mammal))
df_syria_other <- subset(df_syria, !is.na(df_syria$fungi) | !is.na(df_syria$coleoptera) | !is.na(df_syria$papilionoidea))
nrow(df_syria_myers); (nrow(df_syria_myers)/13001)*100
nrow(df_syria_other); (nrow(df_syria_other)/13001)*100
df_syria_all_taxa <- subset(df_syria, !is.na(df_syria$plant) | !is.na(df_syria$amphibian) | !is.na(df_syria$reptile) | !is.na(df_syria$bird) | !is.na(df_syria$mammal) | !is.na(df_syria$fungi) | !is.na(df_syria$coleoptera) | !is.na(df_syria$papilionoidea))
nrow(df_syria_all_taxa)

# Palestine
df_palestine <- subset(wos_data, grepl("Palestine", wos_data$fieldwork_country))
df_palestine_myers <- subset(df_palestine, !is.na(df_palestine$plant) | !is.na(df_palestine$amphibian) | !is.na(df_palestine$reptile) | !is.na(df_palestine$bird) | !is.na(df_palestine$mammal))
df_palestine_other <- subset(df_palestine, !is.na(df_palestine$fungi) | !is.na(df_palestine$coleoptera) | !is.na(df_palestine$papilionoidea))
nrow(df_palestine_myers); (nrow(df_palestine_myers)/13001)*100
nrow(df_palestine_other); (nrow(df_palestine_other)/13001)*100
df_palestine_all_taxa <- subset(df_palestine, !is.na(df_palestine$plant) | !is.na(df_palestine$amphibian) | !is.na(df_palestine$reptile) | !is.na(df_palestine$bird) | !is.na(df_palestine$mammal) | !is.na(df_palestine$fungi) | !is.na(df_palestine$coleoptera) | !is.na(df_palestine$papilionoidea))
nrow(df_palestine_all_taxa)

# Near East
df_near_east <- subset(wos_data, grepl("Israel", wos_data$fieldwork_country) | grepl("Cyprus", wos_data$fieldwork_country) | grepl("Lebanon", wos_data$fieldwork_country) | grepl("Syria", wos_data$fieldwork_country) | grepl("Palestine", wos_data$fieldwork_country))
df_near_east_myers <- subset(df_near_east, !is.na(df_near_east$plant) | !is.na(df_near_east$amphibian) | !is.na(df_near_east$reptile) | !is.na(df_near_east$bird) | !is.na(df_near_east$mammal))
df_near_east_other <- subset(df_near_east, !is.na(df_near_east$fungi) | !is.na(df_near_east$coleoptera) | !is.na(df_near_east$papilionoidea))
nrow(df_near_east_myers); (nrow(df_near_east_myers)/13001)*100
nrow(df_near_east_other); (nrow(df_near_east_other)/13001)*100


# publications dealing with the northern side of the Mediterranean Sea
df_north <- rbind(df_north_western, df_balkans, df_north_eastern)
df_north <- df_north[!duplicated(df_north), ]
df_north <- subset(df_north, !is.na(df_north$plant) | !is.na(df_north$amphibian) | !is.na(df_north$reptile) | !is.na(df_north$bird) | !is.na(df_north$mammal) | !is.na(df_north$fungi) | !is.na(df_north$coleoptera) | !is.na(df_north$papilionoidea))
nrow(df_north); (nrow(df_north)/13001)*100
# 72.16% of publications deal with at least one country of the northern side of the Mediterranean

# contribution of the 4 north-western countries
df_4NW <- rbind(df_portugal, df_spain, df_france, df_italy)
df_4NW <- df_4NW[!duplicated(df_4NW), ]
df_4NW <- subset(df_4NW, !is.na(df_4NW$plant) | !is.na(df_4NW$amphibian) | !is.na(df_4NW$reptile) | !is.na(df_4NW$bird) | !is.na(df_4NW$mammal) | !is.na(df_4NW$fungi) | !is.na(df_4NW$coleoptera) | !is.na(df_4NW$papilionoidea))
nrow(df_4NW); (nrow(df_4NW)/13001)*100
# 57.42 of publications deal with at least one of the four countries of NW med

# contribution of 3 main marine regions
df_marine_taxa <- subset(wos_data, !is.na(wos_data$fish) | !is.na(wos_data$porifera) | !is.na(wos_data$crustacea))
df_marine_taxa <- subset(df_marine_taxa, !is.na(df_marine_taxa$marine_region))
df_3_marine <- subset(df_marine_taxa, grepl("Western Mediterranean Sea", df_marine_taxa$marine_region) | grepl("Adriatic Sea", df_marine_taxa$marine_region) | grepl("Aegean Sea", df_marine_taxa$marine_region))
nrow(df_3_marine); (nrow(df_3_marine)/nrow(df_marine_taxa))*100
# 46.25% of publications on marine taxa deal with these 3 marine regions



