#################################################
#
# Biodivmex project
#
# GenBank_03_curves.R
#
# jan.perret@cefe.cnrs.fr
#################################################


### accumulation curve of number of sequences for each taxa at med basin scale (no subdivision per country)
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
                                        scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year) +
                                        directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
                                        coord_cartesian(clip = 'off') +
                                        theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
                                        expand_limits(x = c(2020))

# curve with colour per taxa log10 y axis
GenBank_curve_seq_acc_taxa_log10 <- ggplot(all_taxa_year_tab_acc_long, aes(x = year, y = (n_seq+1), group = taxa, colour = taxa)) + ###### log10(y+1) pour que ca ne bug pas
                                        geom_line(size=1.2) +
                                        labs(title = "Number of sequences for each taxonomic group in the mediterranean basin (log10 scale)") +
                                        xlab("Year") + ylab("Number of sequences") +
                                        theme_bw() +
                                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                        scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year) +
                                        scale_y_continuous(trans='log10') +
                                        directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
                                        coord_cartesian(clip = 'off') +
                                        theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
                                        expand_limits(x = c(2020))

# make difference between myers taxa and others
# create a myers/non-myers column
all_taxa_year_tab_acc_long["myers"] <- NA
myers_taxa <- c("amphibian", "bird", "mammal", "plant", "reptile")
non_myers_taxa <- c("coleoptera", "crustacea", "fish", "fungi", "lumbricina", "papilionoidea", "sponge", "tree")

for (i in 1:length(all_taxa_year_tab_acc_long$taxa)) {
  if (all_taxa_year_tab_acc_long$taxa[i] %in% myers_taxa) {all_taxa_year_tab_acc_long$myers[i] <- "myers_taxa"}
  else if (all_taxa_year_tab_acc_long$taxa[i] %in% non_myers_taxa) {all_taxa_year_tab_acc_long$myers[i] <- "non_myers_taxa"}
}

# curve with colour per myers_non-myers linear y axis
GenBank_curve_seq_acc_myers_linear <- ggplot(all_taxa_year_tab_acc_long, aes(x = year, y = n_seq, group = taxa, colour = myers)) +
                                          geom_line(size=1.2) +
                                          labs(title = "Number of sequences for Myers/non-Myers taxonomic groups in the mediterranean basin") +
                                          xlab("Year") + ylab("Number of sequences") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                          scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year) +
                                          directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
                                          coord_cartesian(clip = 'off') +
                                          theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
                                          expand_limits(x = c(2020))

# curve with colour per myers_non-myers log10 y axis
GenBank_curve_seq_acc_myers_log10 <- ggplot(all_taxa_year_tab_acc_long, aes(x = year, y = (n_seq+1), group = taxa, colour = myers)) + ###### log10(y+1) pour que ca ne bug pas
                                          geom_line(size=1.2) +
                                          labs(title = "Number of sequences for Myers/non-Myers taxonomic groups in the mediterranean basin (log10 scale)") +
                                          xlab("Year") + ylab("Number of sequences") +
                                          theme_bw() +
                                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                                          scale_x_continuous("Year", labels = as.character(all_taxa_year_tab_acc_long$year), breaks = all_taxa_year_tab_acc_long$year) +
                                          scale_y_continuous(trans='log10') +
                                          directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
                                          coord_cartesian(clip = 'off') +
                                          theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm")) +
                                          expand_limits(x = c(2020))

# save figures in PDF
pdf(file = "./output/plots/GenBank_curve_seq_acc_taxa_linear.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_taxa_linear
dev.off()

pdf(file = "./output/plots/GenBank_curve_seq_acc_taxa_log10.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_taxa_log10
dev.off()

pdf(file = "./output/plots/GenBank_curve_seq_acc_myers_linear.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_myers_linear
dev.off()

pdf(file = "./output/plots/GenBank_curve_seq_acc_myers_log10.pdf", width = 11.69, height = 8.27)
GenBank_curve_seq_acc_myers_log10
dev.off()

### sequence accumulation per country (one curve per taxa)
GenBank_curve_seq_acc_country_plant <- GB_acc_curve_per_country(taxa_acc_data = plant_country_year_tab_acc, taxa_name = "Plant")
GenBank_curve_seq_acc_country_fungi <- GB_acc_curve_per_country(taxa_acc_data = fungi_country_year_tab_acc, taxa_name = "Fungi")
GenBank_curve_seq_acc_country_amph <- GB_acc_curve_per_country(taxa_acc_data = amph_country_year_tab_acc, taxa_name = "Amphibian")
GenBank_curve_seq_acc_country_rept <- GB_acc_curve_per_country(taxa_acc_data = rept_country_year_tab_acc, taxa_name = "Reptile")
GenBank_curve_seq_acc_country_bird <- GB_acc_curve_per_country(taxa_acc_data = bird_country_year_tab_acc, taxa_name = "Bird")
GenBank_curve_seq_acc_country_mammal <- GB_acc_curve_per_country(taxa_acc_data = mammal_country_year_tab_acc, taxa_name = "Mammals")
GenBank_curve_seq_acc_country_coleo <- GB_acc_curve_per_country(taxa_acc_data = coleo_country_year_tab_acc, taxa_name = "Coleoptera")
GenBank_curve_seq_acc_country_papilio <- GB_acc_curve_per_country(taxa_acc_data = papilio_country_year_tab_acc, taxa_name = "Papilionoidea")
GenBank_curve_seq_acc_country_lumbri <- GB_acc_curve_per_country(taxa_acc_data = lumbri_country_year_tab_acc, taxa_name = "Lumbricina")
GenBank_curve_seq_acc_country_fish <- GB_acc_curve_per_country(taxa_acc_data = fish_country_year_tab_acc, taxa_name = "Fish")
GenBank_curve_seq_acc_country_sponge <- GB_acc_curve_per_country(taxa_acc_data = sponge_country_year_tab_acc, taxa_name = "Porifera")
GenBank_curve_seq_acc_country_crusta <- GB_acc_curve_per_country(taxa_acc_data = crusta_country_year_tab_acc, taxa_name = "Crustacea")
GenBank_curve_seq_acc_country_tree <- GB_acc_curve_per_country(taxa_acc_data = tree_country_year_tab_acc, taxa_name = "Tree")

### save curves
# plant
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_plant.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_plant)
dev.off()

# fungi
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_fungi.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_fungi)
dev.off()

# amphibian
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_amphibian.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_amph)
dev.off()

# reptile
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_reptile.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_rept)
dev.off()

# bird
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_bird.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_bird)
dev.off()

# mammal
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_mammal.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_mammal)
dev.off()

# coleoptera
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_coleoptera.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_coleo)
dev.off()

# papilionoidea
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_papilionoidea.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_papilio)
dev.off()

# lumbricina
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_lumbricina.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_lumbri)
dev.off()

# fish
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_fish.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_fish)
dev.off()

# sponge
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_sponge.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_sponge)
dev.off()

# crustacea
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_crustacea.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_crusta)
dev.off()

# tree
pdf(file = paste0("./output/plots/GenBank_curve_seq_acc_country_tree.pdf"), width = 20, height = 12)
print(GenBank_curve_seq_acc_country_tree)
dev.off()

