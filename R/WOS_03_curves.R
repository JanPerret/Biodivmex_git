#################################################
#
# Biodivmex project
#
# WOS_03_curves.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### accumulation curve of number of publications for each taxa at med basin scale (no subdivision per country)
# pass data from wide to long format
taxa_year_table_acc_long <- as_tibble(pivot_longer(taxa_year_table_acc, 
                                                     cols = colnames(taxa_year_table_acc)[2]:colnames(taxa_year_table_acc)[ncol(taxa_year_table_acc)], 
                                                     names_to = "year", 
                                                     values_to = "n_article"))

# convert from factor to numeric
taxa_year_table_acc_long$year <- as.numeric(taxa_year_table_acc_long$year)
# taxa_year_table_acc_long$n_article <- as.numeric(levels(taxa_year_table_acc_long$n_article))[taxa_year_table_acc_long$n_article]

# curve with colour per taxa linear y axis
WOS_curve_art_acc_taxa_linear <- ggplot(taxa_year_table_acc_long, aes(x = year, y = n_article, group = taxa, colour = taxa)) +
                                        geom_line(size=1.2) +
                                        labs(title = "Number of publications for each taxonomic group in the mediterranean basin") +
                                        xlab("Year") + ylab("Number of publications") +
                                        theme_bw() +
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                        scale_x_continuous("Year", labels = as.character(taxa_year_table_acc_long$year), breaks = taxa_year_table_acc_long$year)

# curve with colour per taxa log10 y axis
WOS_curve_art_acc_taxa_log10 <- ggplot(taxa_year_table_acc_long, aes(x = year, y = (n_article+1), group = taxa, colour = taxa)) + # log10(y+1) because log10 function is defined for strictly positive values
                                        geom_line(size=1.2) +
                                        labs(title = "Number of publications for each taxonomic group in the mediterranean basin (log10 scale)") +
                                        xlab("Year") + ylab("Number of publications") +
                                        theme_bw() +
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                        scale_x_continuous("Year", labels = as.character(taxa_year_table_acc_long$year), breaks = taxa_year_table_acc_long$year) +
                                        scale_y_continuous(trans='log10')

# make difference between myers taxa and others
# create a myers/non-myers column
taxa_year_table_acc_long["myers"] <- NA
myers_taxa <- c("amphibian", "bird", "mammal", "plant", "reptile")
non_myers_taxa <- c("coleoptera", "crustacea", "fish", "fungi", "lumbricina", "papilionoidea", "porifera", "tree")

for (i in 1:length(taxa_year_table_acc_long$taxa)) {
  if (taxa_year_table_acc_long$taxa[i] %in% myers_taxa) {taxa_year_table_acc_long$myers[i] <- "myers_taxa"}
  else if (taxa_year_table_acc_long$taxa[i] %in% non_myers_taxa) {taxa_year_table_acc_long$myers[i] <- "non_myers_taxa"}
}

# curve with colour per myers_non-myers linear y axis
WOS_curve_art_acc_myers_linear <- ggplot(taxa_year_table_acc_long, aes(x = year, y = n_article, group = taxa, colour = myers)) +
                                          geom_line(size=1.2) +
                                          labs(title = "Number of publications for myers/non-myers taxonomic groups in the mediterranean basin") +
                                          xlab("Year") + ylab("Number of publications") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                          scale_x_continuous("Year", labels = as.character(taxa_year_table_acc_long$year), breaks = taxa_year_table_acc_long$year)

# curve with colour per myers_non-myers log10 y axis
WOS_curve_art_acc_myers_log10 <- ggplot(taxa_year_table_acc_long, aes(x = year, y = (n_article+1), group = taxa, colour = myers)) + ###### log10(y+1) pour que ca ne bug pas
                                          geom_line(size=1.2) +
                                          labs(title = "Number of publications for myers/non-myers taxonomic groups in the mediterranean basin (log10 scale)") +
                                          xlab("Year") + ylab("Number of publications") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                          scale_x_continuous("Year", labels = as.character(taxa_year_table_acc_long$year), breaks = taxa_year_table_acc_long$year) +
                                          scale_y_continuous(trans='log10')

# save figures in PDF
pdf(file = "./output/plots/WOS_curve_article_acc_taxa_linear.pdf", width = 11.69, height = 8.27)
WOS_curve_art_acc_taxa_linear
dev.off()

pdf(file = "./output/plots/WOS_curve_article_acc_taxa_log10.pdf", width = 11.69, height = 8.27)
WOS_curve_art_acc_taxa_log10
dev.off()

pdf(file = "./output/plots/WOS_curve_article_acc_myers_linear.pdf", width = 11.69, height = 8.27)
WOS_curve_art_acc_myers_linear
dev.off()

pdf(file = "./output/plots/WOS_curve_article_acc_myers_log10.pdf", width = 11.69, height = 8.27)
WOS_curve_art_acc_myers_log10
dev.off()


### accumulation curve of number of publications per country (one figure per taxa)
WOS_curve_art_acc_country_plant <- WOS_acc_curve_per_country(taxa_acc_data = plant_year_tab_per_country_acc, taxa_name = "Plant", marine = FALSE)
WOS_curve_art_acc_country_fungi <- WOS_acc_curve_per_country(taxa_acc_data = fungi_year_tab_per_country_acc, taxa_name = "Fungi", marine = FALSE)
WOS_curve_art_acc_country_amphibian <- WOS_acc_curve_per_country(taxa_acc_data = amph_year_tab_per_country_acc, taxa_name = "Amphibian", marine = FALSE)
WOS_curve_art_acc_country_reptile <- WOS_acc_curve_per_country(taxa_acc_data = rept_year_tab_per_country_acc, taxa_name = "Reptile", marine = FALSE)
WOS_curve_art_acc_country_bird <- WOS_acc_curve_per_country(taxa_acc_data = bird_year_tab_per_country_acc, taxa_name = "Bird", marine = FALSE)
WOS_curve_art_acc_country_mammal <- WOS_acc_curve_per_country(taxa_acc_data = mammal_year_tab_per_country_acc, taxa_name = "Mammals", marine = FALSE)
WOS_curve_art_acc_country_coleo <- WOS_acc_curve_per_country(taxa_acc_data = coleo_year_tab_per_country_acc, taxa_name = "Coleoptera", marine = FALSE)
WOS_curve_art_acc_country_papilio <- WOS_acc_curve_per_country(taxa_acc_data = papilio_year_tab_per_country_acc, taxa_name = "Papilionoidea", marine = FALSE)
WOS_curve_art_acc_country_lumbri <- WOS_acc_curve_per_country(taxa_acc_data = lumbri_year_tab_per_country_acc, taxa_name = "Lumbricina", marine = FALSE)
WOS_curve_art_acc_country_tree <- WOS_acc_curve_per_country(taxa_acc_data = tree_year_tab_per_country_acc, taxa_name = "Tree", marine = FALSE)
WOS_curve_art_acc_marine_region_fish <- WOS_acc_curve_per_country(taxa_acc_data = fish_year_tab_per_region_acc, taxa_name = "Fish", marine = TRUE)
WOS_curve_art_acc_marine_region_porifera <- WOS_acc_curve_per_country(taxa_acc_data = porifera_year_tab_per_region_acc, taxa_name = "Porifera", marine = TRUE)
WOS_curve_art_acc_marine_region_crustacea <- WOS_acc_curve_per_country(taxa_acc_data = crusta_year_tab_per_region_acc, taxa_name = "Crustacea", marine = TRUE)


### save curves
# plant
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_plant.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_plant
dev.off()

# fungi
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_fungi.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_fungi
dev.off()

# amphibian
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_amphibian.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_amphibian
dev.off()

# reptile
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_reptile.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_reptile
dev.off()

# bird
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_bird.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_bird
dev.off()

# mammal
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_mammal.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_mammal
dev.off()

# coleoptera
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_coleoptera.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_coleo
dev.off()

# papilionoidea
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_papilionoidea.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_papilio
dev.off()

# lumbricina
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_lumbricina.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_lumbri
dev.off()

# tree
pdf(file = paste0("./output/plots/WOS_curve_art_acc_country_tree.pdf"), width = 20, height = 12)
WOS_curve_art_acc_country_tree
dev.off()

# fish
pdf(file = paste0("./output/plots/WOS_curve_art_acc_marine_region_fish.pdf"), width = 20, height = 12)
WOS_curve_art_acc_marine_region_fish
dev.off()

# porifera
pdf(file = paste0("./output/plots/WOS_curve_art_acc_marine_region_porifera.pdf"), width = 20, height = 12)
WOS_curve_art_acc_marine_region_porifera
dev.off()

# crustacea
pdf(file = paste0("./output/plots/WOS_curve_art_acc_marine_region_crustacea.pdf"), width = 20, height = 12)
WOS_curve_art_acc_marine_region_crustacea
dev.off()


### accumulation curve of the number of scientific journals for each taxa at med basin scale (no subdivision per country)
# pass data from wide to long format
taxa_journals_table_long <- as_tibble(pivot_longer(taxa_journals_table, 
                                                   cols = colnames(taxa_journals_table)[5]:colnames(taxa_journals_table)[ncol(taxa_journals_table)], 
                                                   names_to = "year", 
                                                   values_to = "n_journal"))

# convert from factor to numeric
taxa_journals_table_long$year <- as.numeric(taxa_journals_table_long$year)

# curve with colour per taxa linear y axis
WOS_curve_journal_acc_taxa_linear <- ggplot(taxa_journals_table_long, aes(x = year, y = n_journal, group = taxa, colour = taxa)) +
                                        geom_line(size=1.2) +
                                        labs(title = "Number of scientific journals for each taxonomic group in the mediterranean basin") +
                                        xlab("Year") + ylab("Number of scientific journals") +
                                        theme_bw() +
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                        scale_x_continuous("Year", labels = as.character(taxa_journals_table_long$year), breaks = taxa_journals_table_long$year) +
                                        directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
                                        coord_cartesian(clip = 'off') +
                                        theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
                                        expand_limits(x = c(2021.5))
  
  
# create a myers/non-myers column
taxa_journals_table_long["myers"] <- NA
myers_taxa <- c("amphibian", "bird", "mammal", "plant", "reptile")
non_myers_taxa <- c("coleoptera", "crustacea", "fish", "fungi", "lumbricina", "papilionoidea", "porifera", "tree")

for (i in 1:length(taxa_journals_table_long$taxa)) {
  if (taxa_journals_table_long$taxa[i] %in% myers_taxa) {taxa_journals_table_long$myers[i] <- "myers_taxa"}
  else if (taxa_journals_table_long$taxa[i] %in% non_myers_taxa) {taxa_journals_table_long$myers[i] <- "non_myers_taxa"}
}

# curve with colour per myers_non-myers linear y axis
WOS_curve_journal_acc_myers_linear <- ggplot(taxa_journals_table_long, aes(x = year, y = n_journal, group = taxa, colour = myers)) +
                                          geom_line(size=1.2) +
                                          labs(title = "Number of scientific journals for myers/non-myers taxonomic groups in the mediterranean basin") +
                                          xlab("Year") + ylab("Number of scientific journals") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                          scale_x_continuous("Year", labels = as.character(taxa_journals_table_long$year), breaks = taxa_journals_table_long$year) +
                                          directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
                                          coord_cartesian(clip = 'off') +
                                          theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
                                          expand_limits(x = c(2021.5))

# save figures in PDF
pdf(file = "./output/plots/WOS_curve_journal_acc_taxa_linear.pdf", width = 11.69, height = 8.27)
WOS_curve_journal_acc_taxa_linear
dev.off()

pdf(file = "./output/plots/WOS_curve_journal_acc_myers_linear.pdf", width = 11.69, height = 8.27)
WOS_curve_journal_acc_myers_linear
dev.off()
