#################################################
#
# Biodivmex project
#
# WOS_02_data_formating.R
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

# drop articles about mediterranean regions outside the mediterranean basin
wos_data <- wos_data %>%
  filter(is.na(outside_med))

# drop the outside_med column
wos_data <- wos_data %>%
  select(-outside_med)

# drop articles not written in english
wos_data <- wos_data %>%
  filter(language == "English")

# drop articles from 2020
wos_data <- wos_data %>%
  filter(year != 2020)

### corpus time truncation condition
truncation_year = 1979

wos_data <- wos_data %>%
  filter(year > truncation_year) # all articles published from 1980 to 2019
# NB : there was no article with year = NA anyway in the dataframe

# fix year factor levels for all taxa
year_list <- c((truncation_year+1):2019)
wos_data$year <- factor(wos_data$year, levels = year_list, ordered = TRUE)

### total number of articles on each taxonomic group
taxa_vect <- c("plant", "fungi", "amphibian", "reptile", "bird", "mammal", "fish", "sponge", "crustacea", "coleoptera", "papilionoidea", "lumbricina", "tree")

# initiaze table
taxa_table <- setNames(data.frame(matrix(ncol = 3, nrow = 13)), c("taxa", "n_articles", "loc_rate"))
taxa_table$taxa <- taxa_vect

# total number of articles in the corpus assigned to each taxonomic group
taxa_table$n_articles <- c(sum(wos_data$plant == "plant", na.rm = TRUE), 
                           sum(wos_data$fungi == "fungi", na.rm = TRUE),
                           sum(wos_data$amphibian == "amphibian", na.rm = TRUE),
                           sum(wos_data$reptile == "reptile", na.rm = TRUE),
                           sum(wos_data$bird == "bird", na.rm = TRUE),
                           sum(wos_data$mammal == "mammal", na.rm = TRUE),
                           sum(wos_data$fish == "fish", na.rm = TRUE),
                           sum(wos_data$sponge == "sponge", na.rm = TRUE),
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
sponge_df <- subset(wos_data, wos_data$sponge == "sponge")
crusta_df <- subset(wos_data, wos_data$crustacea == "crustacea")
coleo_df <- subset(wos_data, wos_data$coleoptera == "coleoptera")
papilio_df <- subset(wos_data, wos_data$papilionoidea == "papilionoidea")
lumbri_df <- subset(wos_data, wos_data$lumbricina == "lumbricina")
tree_df <- subset(wos_data, wos_data$tree == "tree")

# fieldwork_country assignation rate (or same for marine_region for marine taxa)
taxa_table$loc_rate <- c(round((sum(!is.na(plant_df$fieldwork_country))/length(plant_df$access_num))*100, 2),
                         round((sum(!is.na(fungi_df$fieldwork_country))/length(fungi_df$access_num))*100, 2),
                         round((sum(!is.na(amph_df$fieldwork_country))/length(amph_df$access_num))*100, 2),
                         round((sum(!is.na(rept_df$fieldwork_country))/length(rept_df$access_num))*100, 2),
                         round((sum(!is.na(bird_df$fieldwork_country))/length(bird_df$access_num))*100, 2),
                         round((sum(!is.na(mammal_df$fieldwork_country))/length(mammal_df$access_num))*100, 2),
                         round((sum(!is.na(fish_df$marine_region))/length(fish_df$access_num))*100, 2),
                         round((sum(!is.na(sponge_df$marine_region))/length(sponge_df$access_num))*100, 2),
                         round((sum(!is.na(crusta_df$marine_region))/length(crusta_df$access_num))*100, 2),
                         round((sum(!is.na(coleo_df$fieldwork_country))/length(coleo_df$access_num))*100, 2),
                         round((sum(!is.na(papilio_df$fieldwork_country))/length(papilio_df$access_num))*100, 2),
                         round((sum(!is.na(lumbri_df$fieldwork_country))/length(lumbri_df$access_num))*100, 2),
                         round((sum(!is.na(tree_df$fieldwork_country))/length(tree_df$access_num))*100, 2)
                         )

# save table
write_csv2(taxa_table, path = "./output/text/WOS_recap_table_per_taxa.csv", col_names = TRUE)


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
taxa_year_table[8,] <- as.data.frame(table(sponge_df$year))$Freq
taxa_year_table[9,] <- as.data.frame(table(crusta_df$year))$Freq
taxa_year_table[10,] <- as.data.frame(table(coleo_df$year))$Freq
taxa_year_table[11,] <- as.data.frame(table(papilio_df$year))$Freq
taxa_year_table[12,] <- as.data.frame(table(lumbri_df$year))$Freq
taxa_year_table[13,] <- as.data.frame(table(tree_df$year))$Freq

# bind taxa name column
taxa_year_table <- cbind(taxa_vect, taxa_year_table)

# save table
write_csv2(taxa_year_table, path = "./output/text/WOS_recap_table_articles_per_year.csv", col_names = TRUE)

# passing from raw counts to accumulation
taxa_year_table_acc <- WOS_accumulate(year_tab = taxa_year_table, first_column = 2)

# save table
write_csv2(taxa_year_table_acc, path = "./output/text/WOS_recap_table_articles_per_year_acc.csv", col_names = TRUE)


### number of articles per country for each taxa
# first multiply rows with multiple assignations in fieldwork_country
# initiate variables
times = rep(NA, length(wos_data$access_num))
coun = list()
country = c()

# loop through the dataframe
for (i in 1:length(wos_data$access_num)){
  
  country <- as.vector(strsplit(wos_data$fieldwork_country[i], split = " // ")[[1]]) # extract multiple countries stored in fieldwork_country as a vector
  times[i] <- length(country) # number of different countries for given article
  coun <- rlist::list.append(coun, country) # append coun list with the countries stored in country object
  
}

times[times==0] <- 1 # if there is no country name in fieldwork_country reference still count for one row

# replace positions of length 0 by NA
coun <- lapply(coun, function(x) if(identical(x, character(0))) NA_character_ else x)

# pass "coun" from list to vector
coun <- unlist(coun)

# repeat rows the number of times indicated in the vector "times"
wos_data <- wos_data[rep(seq_len(nrow(wos_data)), times),]
# so if there was zero or one value in fieldwork_country, the row won't be repeated, if there were 2 countries it will be repeated twice, etc.

# assign back unlisted fieldwork countries
wos_data$fieldwork_country <- coun

# fix fieldwork_country factor levels
country_list <- sort(unique(wos_data$fieldwork_country))
wos_data$fieldwork_country <- factor(wos_data$fieldwork_country, levels = country_list, ordered = FALSE)
wos_data$fieldwork_country <- fct_explicit_na(wos_data$fieldwork_country) # give fieldwork_country NAs an explicit value

# sub data frames per taxa
plant_country_df <- subset(wos_data, wos_data$plant == "plant")
fungi_country_df <- subset(wos_data, wos_data$fungi == "fungi")
amph_country_df <- subset(wos_data, wos_data$amphibian == "amphibian")
rept_country_df <- subset(wos_data, wos_data$reptile == "reptile")
bird_country_df <- subset(wos_data, wos_data$bird == "bird")
mammal_country_df <- subset(wos_data, wos_data$mammal == "mammal")
fish_country_df <- subset(wos_data, wos_data$fish == "fish")
sponge_country_df <- subset(wos_data, wos_data$sponge == "sponge")
crusta_country_df <- subset(wos_data, wos_data$crustacea == "crustacea")
coleo_country_df <- subset(wos_data, wos_data$coleoptera == "coleoptera")
papilio_country_df <- subset(wos_data, wos_data$papilionoidea == "papilionoidea")
lumbri_country_df <- subset(wos_data, wos_data$lumbricina == "lumbricina")
tree_country_df <- subset(wos_data, wos_data$tree == "tree")

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
taxa_country_table[,8] <- as.data.frame(table(sponge_country_df$fieldwork_country))$Freq
taxa_country_table[,9] <- as.data.frame(table(crusta_country_df$fieldwork_country))$Freq
taxa_country_table[,10] <- as.data.frame(table(coleo_country_df$fieldwork_country))$Freq
taxa_country_table[,11] <- as.data.frame(table(papilio_country_df$fieldwork_country))$Freq
taxa_country_table[,12] <- as.data.frame(table(lumbri_country_df$fieldwork_country))$Freq
taxa_country_table[,13] <- as.data.frame(table(tree_country_df$fieldwork_country))$Freq

# bind country name column
taxa_country_table <- cbind(fieldwork_country = c(paste0(country_list), "NA"), taxa_country_table)

# save table
write_csv2(taxa_country_table, path = "./output/text/WOS_recap_table_articles_per_country.csv", col_names = TRUE)


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
sponge_year_tab_per_country <- sponge_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
crusta_year_tab_per_country <- crusta_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
fish_year_tab_per_country <- fish_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
tree_year_tab_per_country <- tree_country_df %>% group_by(fieldwork_country, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)

# save tables
write_csv2(plant_year_tab_per_country, path = "./output/text/WOS_plant_year_tab_per_country.csv", col_names = TRUE)
write_csv2(fungi_year_tab_per_country, path = "./output/text/WOS_fungi_year_tab_per_country.csv", col_names = TRUE)
write_csv2(amph_year_tab_per_country, path = "./output/text/WOS_amph_year_tab_per_country.csv", col_names = TRUE)
write_csv2(rept_year_tab_per_country, path = "./output/text/WOS_rept_year_tab_per_country.csv", col_names = TRUE)
write_csv2(bird_year_tab_per_country, path = "./output/text/WOS_bird_year_tab_per_country.csv", col_names = TRUE)
write_csv2(mammal_year_tab_per_country, path = "./output/text/WOS_mammal_year_tab_per_country.csv", col_names = TRUE)
write_csv2(coleo_year_tab_per_country, path = "./output/text/WOS_coleo_year_tab_per_country.csv", col_names = TRUE)
write_csv2(lumbri_year_tab_per_country, path = "./output/text/WOS_lumbri_year_tab_per_country.csv", col_names = TRUE)
write_csv2(papilio_year_tab_per_country, path = "./output/text/WOS_papilio_year_tab_per_country.csv", col_names = TRUE)
write_csv2(sponge_year_tab_per_country, path = "./output/text/WOS_sponge_year_tab_per_country.csv", col_names = TRUE)
write_csv2(crusta_year_tab_per_country, path = "./output/text/WOS_crusta_year_tab_per_country.csv", col_names = TRUE)
write_csv2(fish_year_tab_per_country, path = "./output/text/WOS_fish_year_tab_per_country.csv", col_names = TRUE)
write_csv2(tree_year_tab_per_country, path = "./output/text/WOS_tree_year_tab_per_country.csv", col_names = TRUE)

# passing from raw counts to accumulation tables
plant_year_tab_per_country_acc <- WOS_accumulate(year_tab = plant_year_tab_per_country, first_column = 2)
fungi_year_tab_per_country_acc <- WOS_accumulate(year_tab = fungi_year_tab_per_country, first_column = 2)
amph_year_tab_per_country_acc <- WOS_accumulate(year_tab = amph_year_tab_per_country, first_column = 2)
rept_year_tab_per_country_acc <- WOS_accumulate(year_tab = rept_year_tab_per_country, first_column = 2)
bird_year_tab_per_country_acc <- WOS_accumulate(year_tab = bird_year_tab_per_country, first_column = 2)
mammal_year_tab_per_country_acc <- WOS_accumulate(year_tab = mammal_year_tab_per_country, first_column = 2)
coleo_year_tab_per_country_acc <- WOS_accumulate(year_tab = coleo_year_tab_per_country, first_column = 2)
lumbri_year_tab_per_country_acc <- WOS_accumulate(year_tab = lumbri_year_tab_per_country, first_column = 2)
papilio_year_tab_per_country_acc <- WOS_accumulate(year_tab = papilio_year_tab_per_country, first_column = 2)
sponge_year_tab_per_country_acc <- WOS_accumulate(year_tab = sponge_year_tab_per_country, first_column = 2)
crusta_year_tab_per_country_acc <- WOS_accumulate(year_tab = crusta_year_tab_per_country, first_column = 2)
fish_year_tab_per_country_acc <- WOS_accumulate(year_tab = fish_year_tab_per_country, first_column = 2)
tree_year_tab_per_country_acc <- WOS_accumulate(year_tab = tree_year_tab_per_country, first_column = 2)

# save tables
write_csv2(plant_year_tab_per_country_acc, path = "./output/text/WOS_plant_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(fungi_year_tab_per_country_acc, path = "./output/text/WOS_fungi_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(amph_year_tab_per_country_acc, path = "./output/text/WOS_amph_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(rept_year_tab_per_country_acc, path = "./output/text/WOS_rept_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(bird_year_tab_per_country_acc, path = "./output/text/WOS_bird_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(mammal_year_tab_per_country_acc, path = "./output/text/WOS_mammal_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(coleo_year_tab_per_country_acc, path = "./output/text/WOS_coleo_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(lumbri_year_tab_per_country_acc, path = "./output/text/WOS_lumbri_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(papilio_year_tab_per_country_acc, path = "./output/text/WOS_papilio_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(sponge_year_tab_per_country_acc, path = "./output/text/WOS_sponge_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(crusta_year_tab_per_country_acc, path = "./output/text/WOS_crusta_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(fish_year_tab_per_country_acc, path = "./output/text/WOS_fish_year_tab_per_country_acc.csv", col_names = TRUE)
write_csv2(tree_year_tab_per_country_acc, path = "./output/text/WOS_tree_year_tab_per_country_acc.csv", col_names = TRUE)


### number of articles per country with from_country / inside_med / outside_med corresponding author for each taxa
# add empty column "author_loc"
wos_data["author_loc"] <- NA
wos_data <- wos_data %>% mutate(author_loc = as.character(author_loc))

# fix factor levels for author_nationality
wos_data$author_nationality <- factor(wos_data$author_nationality, levels = unique(wos_data$author_nationality), ordered = FALSE)

# give missing values an explicit factor level to ensure that they appear in summaries and plots
wos_data$author_nationality <- fct_explicit_na(wos_data$author_nationality)

# loop to fill the column
# initialize variables
fdwk_country = ""
author_natio = ""
country_detection = FALSE
# i=2 # author_nationality = Italy // Israel
# i=5 # author_nationality = Netherlands

for (i in 1:length(wos_data$access_num)) {
  fdwk_country = as.character(wos_data$fieldwork_country[i])
  author_natio = as.character(wos_data$author_nationality[i])

  # detect if there is mediterranean country in author_natio
  for (coun_name in country_list) {
    if (str_detect(author_natio, coun_name)) {country_detection <- TRUE}
  }
  
  # to avoid bug in case of missing value
  if (author_natio == "(Missing)") {author_natio <- "XXXXXXXXXXXXXXXX"}
  if (fdwk_country == "(Missing)") {fdwk_country <- "ZZZZZZZZZZZZZZZZ"} # as NAs have an explicit value in wos_data$fieldwork_country we can't use is.na() 

  # to fill author loc for from_country authors
  if (str_detect(author_natio, fdwk_country)) {wos_data$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Balearic Islands" & str_detect(author_natio, "Spain")) {wos_data$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Corsica" & str_detect(author_natio, "France")) {wos_data$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Crete" & str_detect(author_natio, "Greece")) {wos_data$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Sardinia" & str_detect(author_natio, "Italy")) {wos_data$author_loc[i] <- "from_country"}
  else if (fdwk_country == "Sicily" & str_detect(author_natio, "Italy")) {wos_data$author_loc[i] <- "from_country"}
  
  # for inside_med authors
  else if (country_detection == TRUE) {wos_data$author_loc[i] <- "inside_med"}
  
  # for outside_med authors
  else if (author_natio != "XXXXXXXXXXXXXXXX") {wos_data$author_loc[i] <- "outside_med"}
  
  # at the end of the loop set country_detection to FALSE again before next iteration
  country_detection <- FALSE
}

# fix factor levels
wos_data$author_loc <- factor(wos_data$author_loc, levels = c("from_country", "outside_med", "inside_med"), ordered = FALSE)

# give missing values an explicit factor level to ensure that they appear in summaries and plots
wos_data$author_loc <- fct_explicit_na(wos_data$author_loc)

# test if missing values are still missing
# table(wos_data$author_nationality, useNA = "always")
# table(wos_data$author_loc, useNA = "always")
# -> ok the 2564 '(Missing)' values are still there

# sub data frames per taxa
plant_loc_df <- subset(wos_data, wos_data$plant == "plant")
fungi_loc_df <- subset(wos_data, wos_data$fungi == "fungi")
amph_loc_df <- subset(wos_data, wos_data$amphibian == "amphibian")
rept_loc_df <- subset(wos_data, wos_data$reptile == "reptile")
bird_loc_df <- subset(wos_data, wos_data$bird == "bird")
mammal_loc_df <- subset(wos_data, wos_data$mammal == "mammal")
fish_loc_df <- subset(wos_data, wos_data$fish == "fish")
sponge_loc_df <- subset(wos_data, wos_data$sponge == "sponge")
crusta_loc_df <- subset(wos_data, wos_data$crustacea == "crustacea")
coleo_loc_df <- subset(wos_data, wos_data$coleoptera == "coleoptera")
papilio_loc_df <- subset(wos_data, wos_data$papilionoidea == "papilionoidea")
lumbri_loc_df <- subset(wos_data, wos_data$lumbricina == "lumbricina")
tree_loc_df <- subset(wos_data, wos_data$tree == "tree")

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
sponge_article_loc_tab <- sponge_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
crusta_article_loc_tab <- crusta_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
fish_article_loc_tab <- fish_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)
tree_article_loc_tab <- tree_loc_df %>% group_by(fieldwork_country, author_loc, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = author_loc, values_from = n)

# save tables
write_csv2(plant_article_loc_tab, path = "./output/text/WOS_plant_article_loc_tab.csv", col_names = TRUE)
write_csv2(fungi_article_loc_tab, path = "./output/text/WOS_fungi_article_loc_tab.csv", col_names = TRUE)
write_csv2(amph_article_loc_tab, path = "./output/text/WOS_amph_article_loc_tab.csv", col_names = TRUE)
write_csv2(rept_article_loc_tab, path = "./output/text/WOS_rept_article_loc_tab.csv", col_names = TRUE)
write_csv2(bird_article_loc_tab, path = "./output/text/WOS_bird_article_loc_tab.csv", col_names = TRUE)
write_csv2(mammal_article_loc_tab, path = "./output/text/WOS_mammal_article_loc_tab.csv", col_names = TRUE)
write_csv2(coleo_article_loc_tab, path = "./output/text/WOS_coleo_article_loc_tab.csv", col_names = TRUE)
write_csv2(lumbri_article_loc_tab, path = "./output/text/WOS_lumbri_article_loc_tab.csv", col_names = TRUE)
write_csv2(papilio_article_loc_tab, path = "./output/text/WOS_papilio_article_loc_tab.csv", col_names = TRUE)
write_csv2(sponge_article_loc_tab, path = "./output/text/WOS_sponge_article_loc_tab.csv", col_names = TRUE)
write_csv2(crusta_article_loc_tab, path = "./output/text/WOS_crusta_article_loc_tab.csv", col_names = TRUE)
write_csv2(fish_article_loc_tab, path = "./output/text/WOS_fish_article_loc_tab.csv", col_names = TRUE)
write_csv2(tree_article_loc_tab, path = "./output/text/WOS_tree_article_loc_tab.csv", col_names = TRUE)

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
sponge_journals_df <- subset(wos_data_journals, wos_data_journals$sponge == "sponge")
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
                                        length(unique(sponge_journals_df$publisher)),
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
                                        length(sponge_journals_df$access_num),
                                        length(crusta_journals_df$access_num),
                                        length(coleo_journals_df$access_num),
                                        length(papilio_journals_df$access_num),
                                        length(lumbri_journals_df$access_num),
                                        length(tree_journals_df$access_num)
                                        )
# mean number of articles per journal
taxa_journals_table$mean_art_per_journal <- round(taxa_journals_table$tot_articles / taxa_journals_table$tot_n_journals, digits = 2)

# number of journals through the years

########## question : is the raw number of journals per year interesting or the accumulation number of journals ?
########## -> probably only the accumulation

########## extract this info with a loop that subsets the data frame at each iteration with a "given_year" variable going from 1980 to 2019, 
########## and a subset condition in the form of : subset(plant_journals_df, plant_journals_df$year <= given_year), 
########## and do a length(unique(plant_journals_df$publisher)) at each step.
########## put that in a function, and make the function turn for each taxa


