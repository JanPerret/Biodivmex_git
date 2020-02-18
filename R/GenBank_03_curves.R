#################################################
#
# Biodivmex project
#
# GenBank_03_curves.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### taxa sequence accumulation curve at med scale
# pass data from wide to long format
all_taxa_year_tab_acc_long <- as_tibble(pivot_longer(all_taxa_year_tab_acc, 
                                                     cols = colnames(all_taxa_year_tab_acc)[2]:colnames(all_taxa_year_tab_acc)[ncol(all_taxa_year_tab_acc)], 
                                                     names_to = "year",
                                                     values_to = "n_seq"))

# convert from factor to numeric
all_taxa_year_tab_acc_long$year <- as.numeric(all_taxa_year_tab_acc_long$year)
all_taxa_year_tab_acc_long$n_seq <- as.numeric(levels(all_taxa_year_tab_acc_long$n_seq))[all_taxa_year_tab_acc_long$n_seq]

# truncate at 1995 because in this data frame the first sequences are from 1996
all_taxa_year_tab_acc_long <- subset(all_taxa_year_tab_acc_long, all_taxa_year_tab_acc_long$year > 1994)

# curve with colour per taxa linear y axis
GenBank_curve_seq_acc_taxa_linear <- ggplot(all_taxa_year_tab_acc_long, aes(x = year, y = n_seq, group = taxa, colour = taxa)) +
                                        geom_line(size=1.2) +
                                        labs(title = "Number of sequences for each taxonomic group in the mediterranean basin") +
                                        xlab("Year") + ylab("Number of sequences") +
                                        theme_bw() +
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                        scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year)

# curve with colour per taxa log10 y axis
GenBank_curve_seq_acc_taxa_log10 <- ggplot(all_taxa_year_tab_acc_long, aes(x = year, y = (n_seq+1), group = taxa, colour = taxa)) + ###### log10(y+1) pour que ca ne bug pas
                                        geom_line(size=1.2) +
                                        labs(title = "Number of sequences for each taxonomic group in the mediterranean basin (log10 scale)") +
                                        xlab("Year") + ylab("Number of sequences") +
                                        theme_bw() +
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                        scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year) +
                                        scale_y_continuous(trans='log10')

# make difference between Meyers taxa and others
# create a meyers/non-meyers column
all_taxa_year_tab_acc_long["meyers"] <- NA
meyers_taxa <- c("amphibian", "bird", "mammal", "plant", "reptile")
non_meyers_taxa <- c("coleoptera", "crustacea", "fish", "fungi", "lumbricina", "papilionoidea", "sponge", "tree")

for (i in 1:length(all_taxa_year_tab_acc_long$taxa)) {
  if (all_taxa_year_tab_acc_long$taxa[i] %in% meyers_taxa) {all_taxa_year_tab_acc_long$meyers[i] <- "meyers_taxa"}
  else if (all_taxa_year_tab_acc_long$taxa[i] %in% non_meyers_taxa) {all_taxa_year_tab_acc_long$meyers[i] <- "non_meyers_taxa"}
}

# curve with colour per meyers_non-meyers linear y axis
GenBank_curve_seq_acc_meyers_linear <- ggplot(all_taxa_year_tab_acc_long, aes(x = year, y = n_seq, group = taxa, colour = meyers)) +
                                          geom_line(size=1.2) +
                                          labs(title = "Number of sequences for Meyers/non-Meyers taxonomic groups in the mediterranean basin") +
                                          xlab("Year") + ylab("Number of sequences") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                          scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year)

# curve with colour per meyers_non-meyers log10 y axis
GenBank_curve_seq_acc_meyers_log10 <- ggplot(all_taxa_year_tab_acc_long, aes(x = year, y = (n_seq+1), group = taxa, colour = meyers)) + ###### log10(y+1) pour que ca ne bug pas
                                          geom_line(size=1.2) +
                                          labs(title = "Number of sequences for Meyers/non-Meyers taxonomic groups in the mediterranean basin (log10 scale)") +
                                          xlab("Year") + ylab("Number of sequences") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                          scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year) +
                                          scale_y_continuous(trans='log10')

# save figures in PDF
pdf(file = "./output/plots/GenBank_curve_seq_acc_taxa_linear.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_taxa_linear
dev.off()

pdf(file = "./output/plots/GenBank_curve_seq_acc_taxa_log10.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_taxa_log10
dev.off()

pdf(file = "./output/plots/GenBank_curve_seq_acc_meyers_linear.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_meyers_linear
dev.off()

pdf(file = "./output/plots/GenBank_curve_seq_acc_meyers_log10.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_meyers_log10
dev.off()

### one curve per taxa : sequence accumulation per country

# taxa_acc_data = plant_country_year_tab_acc
# taxa_name = "plant"
# 
# # pass data from wide to long format
# taxa_acc_data_long <- as_tibble(pivot_longer(taxa_acc_data, 
#                                                      cols = colnames(taxa_acc_data)[2]:colnames(taxa_acc_data)[ncol(taxa_acc_data)], 
#                                                      names_to = "year",
#                                                      values_to = "n_seq"))
# 
# # convert year from character to numeric
# taxa_acc_data_long$year <- as.numeric(taxa_acc_data_long$year)
# # taxa_acc_data_long$n_seq <- as.numeric(levels(taxa_acc_data_long$n_seq))[taxa_acc_data_long$n_seq]
# 
# # truncate at 1995 because the first sequences are from 1996
# taxa_acc_data_long <- subset(taxa_acc_data_long, taxa_acc_data_long$year > 1994)
# 
# # curve with colour per country and linear y axis
# GenBank_curve_seq_acc_country <- ggplot(taxa_acc_data_long, aes(x = year, y = n_seq, group = sample_origin, colour = sample_origin)) +
#                                     geom_line(size=1.2) +
#                                     labs(title = paste0("Number of sequences for ", taxa_name, " per country")) +
#                                     xlab("Year") + ylab("Number of sequences") +
#                                     theme_bw() +
#                                     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#                                     scale_x_continuous("Year", labels = as.character(taxa_acc_data_long$year), breaks = taxa_acc_data_long$year)
# 
# pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_",taxa_name,".pdf"), width = 20, height = 12)
# GenBank_curve_seq_acc_country
# dev.off()

GB_acc_curve_per_country(taxa_acc_data = plant_country_year_tab_acc, taxa_name = "Plant")
GB_acc_curve_per_country(taxa_acc_data = fungi_country_year_tab_acc, taxa_name = "Fungi")
GB_acc_curve_per_country(taxa_acc_data = amph_country_year_tab_acc, taxa_name = "Amphibian")
GB_acc_curve_per_country(taxa_acc_data = rept_country_year_tab_acc, taxa_name = "Reptile")
GB_acc_curve_per_country(taxa_acc_data = bird_country_year_tab_acc, taxa_name = "Bird")
GB_acc_curve_per_country(taxa_acc_data = mammal_country_year_tab_acc, taxa_name = "Mammals")
GB_acc_curve_per_country(taxa_acc_data = coleo_country_year_tab_acc, taxa_name = "Coleoptera")
GB_acc_curve_per_country(taxa_acc_data = papilio_country_year_tab_acc, taxa_name = "Papilionoidea")
GB_acc_curve_per_country(taxa_acc_data = lumbri_country_year_tab_acc, taxa_name = "Lumbricina")
GB_acc_curve_per_country(taxa_acc_data = fish_country_year_tab_acc, taxa_name = "Fish")
GB_acc_curve_per_country(taxa_acc_data = sponge_country_year_tab_acc, taxa_name = "Porifera")
GB_acc_curve_per_country(taxa_acc_data = crusta_country_year_tab_acc, taxa_name = "Crustacea")
GB_acc_curve_per_country(taxa_acc_data = tree_country_year_tab_acc, taxa_name = "Tree")



