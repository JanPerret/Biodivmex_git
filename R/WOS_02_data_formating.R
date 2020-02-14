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
# fix year factor levels for all taxa
year_list <- c((truncation_year+1):2019)

plant_df$year <- factor(plant_df$year, levels = year_list, ordered = TRUE)
fungi_df$year <- factor(fungi_df$year, levels = year_list, ordered = TRUE)
amph_df$year <- factor(amph_df$year, levels = year_list, ordered = TRUE)
rept_df$year <- factor(rept_df$year, levels = year_list, ordered = TRUE)
bird_df$year <- factor(bird_df$year, levels = year_list, ordered = TRUE)
mammal_df$year <- factor(mammal_df$year, levels = year_list, ordered = TRUE)
fish_df$year <- factor(fish_df$year, levels = year_list, ordered = TRUE)
sponge_df$year <- factor(sponge_df$year, levels = year_list, ordered = TRUE)
crusta_df$year <- factor(crusta_df$year, levels = year_list, ordered = TRUE)
coleo_df$year <- factor(coleo_df$year, levels = year_list, ordered = TRUE)
papilio_df$year <- factor(papilio_df$year, levels = year_list, ordered = TRUE)
lumbri_df$year <- factor(lumbri_df$year, levels = year_list, ordered = TRUE)
tree_df$year <- factor(tree_df$year, levels = year_list, ordered = TRUE)

# initiaze table
taxa_year_table <- setNames(data.frame(matrix(ncol = length(year_list), nrow = 13)), c(paste0(year_list)))
# taxa_year_table$taxa <- taxa_vect

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
View(taxa_year_table)

# save table
write_csv2(taxa_year_table, path = "./output/text/WOS_recap_table_articles_per_year.csv", col_names = TRUE)

# passing from raw counts to accumulation
taxa_year_table_acc <- WOS_accumulate(year_tab = taxa_year_table, first_column = 2)

# save table
write_csv2(taxa_year_table_acc, path = "./output/text/WOS_recap_table_articles_per_year_acc.csv", col_names = TRUE)


### number of articles per year per country for each taxa



# plant_year_tab <- plant_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# fungi_year_tab <- fungi_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# amph_year_tab <- amph_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# rept_year_tab <- rept_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# bird_year_tab <- bird_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# mammal_year_tab <- mammal_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# coleo_year_tab <- coleo_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# lumbri_year_tab <- lumbri_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# papilio_year_tab <- papilio_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# sponge_year_tab <- sponge_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# crusta_year_tab <- crusta_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# fish_year_tab <- fish_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)
# tree_year_tab <- tree_df %>% group_by(sample_origin, year, .drop = FALSE) %>% summarise(n=n()) %>% pivot_wider(names_from = year, values_from = n)


