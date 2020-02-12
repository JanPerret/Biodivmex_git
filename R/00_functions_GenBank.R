#################################################
#
# Biodivmex project
#
# 00_functions_GenBank.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### extract general informations for a diven taxa
GB_extract_general_info <- function(kingdom_data, taxa_data, taxa_data_med, taxa_name) {
  
  # initiaze table to store the 6 general  descriptive informations
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
  descritive_table$n_sp_med <- length(levels(as.factor(taxa_data_med$species_level))) # here levels() if better than unique() because it don't count NA as a value
  
  if ("hybrid" %in% levels(as.factor(taxa_data_med$species_level))) {
    descritive_table$n_sp_med <- descritive_table$n_sp_med - 1 # minus one if the "hybrid" level is present because it is not a species name
  }
  
  return(descritive_table)
}


### extract the same information than GB_extract_general_info() but for each year
GB_loop_over_years <- function(kingdom_data, taxa_data, taxa_data_med, taxa_name) {
  
  # initiaze table to store the 6 general descriptive informations
  general_table <- setNames(data.frame(matrix(ncol = 8, nrow = 0)),
                            c("taxa", "n_seq", "taxa_n_seq", "loc_rate", "n_seq_med", "sp_level_rate_med", "n_sp_med", "year"))
  
  # list years since the first sequence was deposited in GenBank (for our taxa + genes conditions)
  vect_year <- as.integer(c(1987:2019)) # first sequences is from 1987 in our data
  
  # loop over years
  for (year in vect_year) {
    given_year <- as.integer(year) # because else it doesn't work with filter()
    
    # subset tables for the given year
    kingdom_data_year <- kingdom_data %>% filter(year == given_year)
    taxa_data_year <- taxa_data %>% filter(year == given_year)
    taxa_data_med_year <- taxa_data_med %>% filter(year == given_year)
    
    # extract information of the year tables with GB_extract_general_info() function
    tab_given_year <- GB_extract_general_info(kingdom_data = kingdom_data_year, taxa_data = taxa_data_year, taxa_data_med = taxa_data_med_year, taxa_name = taxa_name)
    
    # binding extracted informations to the output table
    tab_given_year <- cbind(tab_given_year, year = given_year)
    general_table <- rbind(general_table, tab_given_year)
  }
  
  return(general_table)
}


### extract number of sequences containing each gene for a given taxa
GB_extract_gene <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  gene_table <- setNames(data.frame(matrix(ncol = 27, nrow = 1)), c("taxa", paste0(gene_list), "n_NA", "tot_n_seq"))
  
  # loop through gene names and fill gene_table
  gene = ""
  for (gene in gene_list) {
    gene_table$taxa <- taxa_name
    
    # count number of sequences containing the given gene
    gene_table[gene] <- sum(grepl(gene, taxa_data_med$gene)) # grepl is like str_detect but it doesn't return NA when used on NA
    
    # number of sequences without a gene assignation
    gene_table$n_NA <- sum(is.na(taxa_data_med$gene))
    
    # total number of sequences
    gene_table$tot_n_seq <- length(taxa_data_med$access_num)
  }
  
  return(gene_table)
}


### extract the same information than GB_extract_gene() but for each year
GB_gene_recap_loop_over_years <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  gene_table <- setNames(data.frame(matrix(ncol = 28, nrow = 0)), c("taxa", "year", paste0(gene_list), "n_NA", "tot_n_seq"))
  
  # list years since the first sequence was deposited in GenBank (for our taxa + genes conditions)
  vect_year <- as.integer(c(1987:2019)) # first sequences is from 1987 in our dataframe
  
  # loop over years
  for (year in vect_year) {
    given_year <- as.integer(year) # because otherway it doesn't work with filter()
    
    # subset tables for the given year
    taxa_data_med_year <- taxa_data_med %>% filter(year == given_year)
    
    # extract information of the year tables with GB_extract_general_info() function
    tab_given_year <- GB_extract_gene(taxa_data_med = taxa_data_med_year, taxa_name = taxa_name)
    
    # binding extracted informations to the output table
    tab_given_year <- cbind(tab_given_year, year = given_year)
    gene_table <- rbind(gene_table, tab_given_year)
  }
  
  return(gene_table)
}


### add column with local / mediterranean / distant sequencer information
GB_generate_sequencer_loc <- function(kingdom_data_med) {
  
  # add empty column "sequencer_loc"
  kingdom_data_med["sequencer_loc"] <- NA
  
  # loop to fill the column
  for (i in 1:length(kingdom_data_med$access_num)) {
    if(is.na(kingdom_data_med$sequencer_nationality[i])) {} # no action if sequencer_nationality == NA
    else if(kingdom_data_med$sequencer_nationality[i] == kingdom_data_med$sample_origin[i]) {kingdom_data_med$sequencer_loc[i] <- "from_country"} # from the country
    else if(kingdom_data_med$sample_origin[i] == "Corsica" & kingdom_data_med$sequencer_nationality[i] == "France") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Balearic Islands" & kingdom_data_med$sequencer_nationality[i] == "Spain") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Sardinia" & kingdom_data_med$sequencer_nationality[i] == "Italy") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Sicily" & kingdom_data_med$sequencer_nationality[i] == "Italy") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sample_origin[i] == "Crete" & kingdom_data_med$sequencer_nationality[i] == "Greece") {kingdom_data_med$sequencer_loc[i] <- "from_country"}
    else if(kingdom_data_med$sequencer_nationality[i] %in% med_countries_list) {kingdom_data_med$sequencer_loc[i] <- "inside_med"} # from the mediterranean basin
    else if(!is.na(kingdom_data_med$sequencer_nationality[i])) {kingdom_data_med$sequencer_loc[i] <- "outside_med"} # outside mediterranean basin
  }
  
  kingdom_data_med$sequencer_loc <- factor(kingdom_data_med$sequencer_loc, levels = c("from_country", "outside_med", "inside_med"), ordered = FALSE) # fix factor levels
  kingdom_data_med$sequencer_loc <- fct_explicit_na(kingdom_data_med$sequencer_loc) # give missing values an explicit factor level to ensure that they appear in summaries and plots
  return(kingdom_data_med)
}


### extract the same information than GB_extract_gene() but for each year
GB_recap_species_level_rate <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  sp_level_table <- setNames(data.frame(matrix(ncol = 4, nrow = 27)),
                             c("country", "taxa", "sp_level_rate", "n_seq"))
  sp_level_table$country <- sample_origin_med_levels
  sp_level_table$taxa <- taxa_name
  
  # loop to fill the table
  for (i in 1:length(sp_level_table$country)) {
    country = sp_level_table$country[i]
    inter_tab <- subset(taxa_data_med, taxa_data_med$sample_origin == country)
    
    # country number of sequences
    sp_level_table$n_seq[i] <- length(inter_tab$species_level)
    
    # species level rate
    a = sum(!is.na(inter_tab$species_level)) - length(which(inter_tab$species_level == "hybrid")) # total number of sequences without hybrids
    sp_level_table$sp_level_rate[i] <- round((a/length(inter_tab$species_level))*100, 2) # in percentage
  }
  
  return(sp_level_table)
}


### make recap tables with number of different species (or species names)
GB_recap_number_species <- function(taxa_data_med, taxa_name) {
  
  # initiaze table
  sp_level_table <- setNames(data.frame(matrix(ncol = 4, nrow = 27)),
                             c("country", "taxa", "n_sp", "n_seq"))
  sp_level_table$country <- sample_origin_med_levels
  sp_level_table$taxa <- taxa_name
  
  # loop to fill the table
  for (i in 1:length(sp_level_table$country)) {
    country = sp_level_table$country[i]
    inter_tab <- subset(taxa_data_med, taxa_data_med$sample_origin == country)
    
    # country number of sequences
    sp_level_table$n_seq[i] <- length(inter_tab$species_level)
    
    # number of species
    sp_level_table$n_sp[i] <- length(levels(as.factor(inter_tab$species_level))) # here levels() if better than unique() because it don't count NA as a value

  }
  
  return(sp_level_table)
}


### transform table with number of sequences in GenBank per country (rows) and years (columns) for accumulation curve
GB_accumulate_over_years <- function(year_tab) {
  
  len <- c(1:nrow(year_tab))
  large <- c(3:ncol(year_tab))
  
  for (i in len){ # loop over rows
    for (n in large){ # loop over columns
      
      year_tab[i,n] <- year_tab[i,n] + year_tab[i,n-1]
      
    }
  }
  
  return(year_tab)
}

