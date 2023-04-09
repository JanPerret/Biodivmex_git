
### /!\ run after running all other R scripts

library(ggplot2)
library(stringr)

#### Figure 1 ####
vect_myers_taxa <- c("plant", "amphibian", "reptile", "bird", "mammal")


df_seq_acc <- read.csv2("./output/text/GenBank_all_taxa_year_tab_acc.csv", header = TRUE)
df_seq_acc <- subset(df_seq_acc, df_seq_acc$taxa != "tree" & df_seq_acc$taxa != "lumbricina")
names(df_seq_acc) <- str_sub(names(df_seq_acc), 2)
names(df_seq_acc)[1] <- "taxa"

# convert to numeric
cols.num <- c(2:35)
df_seq_acc[cols.num] <- sapply(df_seq_acc[cols.num], as.numeric)

df_myers_taxa <- subset(df_seq_acc, df_seq_acc$taxa %in% vect_myers_taxa)
df_other_taxa <- subset(df_seq_acc, !(df_seq_acc$taxa %in% vect_myers_taxa))


# total number of sequences
df_genbank_summary <- data.frame(year = colnames(df_seq_acc[2:35]), 
                         n_seq = colSums(df_seq_acc[cols.num]), 
                         n_seq_myers = colSums(df_myers_taxa[cols.num]),
                         n_seq_other = colSums(df_other_taxa[cols.num]))
df_genbank_summary <- cbind(df_genbank_summary, ratio = df_genbank_summary$n_seq_myers/df_genbank_summary$n_seq*100 )
df_genbank_summary <- df_genbank_summary[c(14:nrow(df_genbank_summary)), ]

ylim.prim <- c(0, 180000)   # in this example, precipitation
ylim.sec <- c(0, 100)    # in this example, temperature
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] # there was a bug here

plot_genbank_summary <- ggplot(data = df_genbank_summary, aes(x = year, y = n_seq)) +
  geom_hline(yintercept = c(90000), linetype = "longdash", size = 1) +
  geom_bar(stat = "identity", fill = "#88419d", color = "black") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        text = element_text(size = 20)) +
  xlab("Year") + ylab("Cumulated number of sequences") +
  scale_y_continuous("Cumulated number of sequences", labels = c("0", "20,000", "40,000", "60,000", "80,000", "100,000", "120,000", "140,000", "160,000", "180,000", "200,000"),
                     breaks = seq(from = 0, to = 200000, by = 20000),
                     sec.axis = sec_axis(~ (. - a)/b, name = "Rate (in %)")) +
  geom_line(aes(y = a + ratio*b, x = year, group = 1), color = "#4575b4", size = 1)


all_taxa_year_tab_acc_long_continental <- subset(all_taxa_year_tab_acc_long, all_taxa_year_tab_acc_long$taxa != "fish" & all_taxa_year_tab_acc_long$taxa != "porifera" & all_taxa_year_tab_acc_long$taxa != "crustacea" & all_taxa_year_tab_acc_long$taxa != "tree" & all_taxa_year_tab_acc_long$taxa != "lumbricina")
all_taxa_year_tab_acc_long_continental$taxa <- str_to_title(all_taxa_year_tab_acc_long_continental$taxa) # capitalize first letter of taxa name
plot_curve_continental <- ggplot(all_taxa_year_tab_acc_long_continental, aes(x = year, y = n_seq, group = taxa, colour = myers)) +
  geom_line(size = 1.2) +
  xlab("Year") + ylab("Cumulated number of sequences") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous("Year", labels = as.character(seq(from = 1996, to = 2020, by = 2)), breaks = seq(from = 1996, to = 2020, by = 2)) +
  scale_y_continuous("Cumulated number of sequences", labels = c("0", "5,000", "10,000", "15,000", "20,000", "25,000", "30,000", "35,000", "40,000", "45,000", "50,000"),
                     breaks = seq(from = 0, to = 50000, by = 5000)) +
  directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
  coord_cartesian(clip = 'off') +
  theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        text = element_text(size = 20)) +
  expand_limits(x = c(2021)) +
  scale_colour_manual(values = c("#4575b4", "#d73027")) +
  geom_vline(xintercept = c(2000), linetype = "longdash", size = 1)

df_prop_before_after_myers <- all_taxa_year_tab_acc_long_continental %>% 
  group_by(year) %>% 
  summarise(total_seq = sum(n_seq)) # 958/157148*100 = 0.61% of sequences were deposited before Myers and 99.39% after


all_taxa_year_tab_acc_long_marine <- subset(all_taxa_year_tab_acc_long, all_taxa_year_tab_acc_long$taxa %in% c("fish", "porifera", "crustacea"))
all_taxa_year_tab_acc_long_marine$taxa <- str_to_title(all_taxa_year_tab_acc_long_marine$taxa) # capitalize first letter of taxa name
plot_curve_marine <- ggplot(all_taxa_year_tab_acc_long_marine, aes(x = year, y = n_seq, group = taxa, colour = myers)) +
  geom_line(size = 1.2) +
  xlab("Year") + ylab("Cumulated number of sequences") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous("Year", labels = as.character(seq(from = 1996, to = 2020, by = 2)), breaks = seq(from = 1996, to = 2020, by = 2)) +
  scale_y_continuous("Cumulated number of sequences", labels = c("0", "5,000", "10,000", "15,000", "20,000", "25,000"),
                     breaks = seq(from = 0, to = 25000, by = 5000)) +
  directlabels::geom_dl(aes(label = taxa), method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
  coord_cartesian(clip = 'off') +
  theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
        text = element_text(size = 20)) +
  expand_limits(x = c(2021)) +
  scale_colour_manual(values = c("#d73027", "#4575b4")) +
  geom_vline(xintercept = c(2000), linetype = "longdash", size = 1)

df_prop_before_after_myers <- all_taxa_year_tab_acc_long_marine %>% 
  group_by(year) %>% 
  summarise(total_seq = sum(n_seq)) # 152/18680*100 = 0.81% of sequences were deposited before Myers and 99.19% after


plot_Fig1 <- ggarrange(plot_genbank_summary, plot_curve_continental, plot_curve_marine,
          labels = c("(a)", "(b)", "(c)"),
          font.label = list(size = 17),
          ncol = 1, nrow = 3)

pdf(file = "./output/plots/Figure1_GenBank_summary.pdf", width = 15, height = 17)
print(plot_Fig1)
dev.off()

#### Figure 2 : summary maps for research and sequencing effort ####
plot_Fig2 <- ggarrange(fig2_map_continental_myers, fig2_map_continental_other, fig2_map_marine,
                       fig2_genbank_map_continental_myers, fig2_genbank_map_continental_other, fig2_genbank_map_marine,
                       labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                       font.label = list(size = 20),
                       ncol = 3, nrow = 2)


pdf(file = "./output/plots/Figure2_maps_research_sequencing_effort.pdf", width = 26, height = 10)
print(plot_Fig2)
dev.off()


#### Figure 3 : number of publications and sequences per country ####

df_fig_3 <- read.csv2("./data/data_Fig_3.csv")
df_fig_3 <- df_fig_3[rev(order(df_fig_3$area_med)), ]

x_breaks = c(0, 1, 10, 100, 1000, 10000, 100000)
my_palette <- c('#4daf4a', '#ff7f00', '#984ea3', '#377eb8', '#e41a1c')

plot_Fig3 <- ggplot(data = df_fig_3, aes(x = tot_seq, y = tot_articles, fill = group, size = area_med)) +
  geom_point(shape = 21) +
  scale_size_continuous(range = c(3, 30)) +
  scale_x_log10(breaks = x_breaks, limits = c(5, 120000), labels = function(x) format(x, scientific = FALSE)) +
  scale_y_log10(breaks = x_breaks, limits = c(5, 8000), labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Number of sequences (source GenBank)") +
  ylab("Number of publications (source WoS)") +
  scale_fill_manual(values = my_palette)
plot_Fig3

pdf(file = "./output/plots/Figure3_sequences_publications_per_country_area.pdf", width = 7, height = 5.5)
print(plot_Fig3)
dev.off()


#### Figure 4 : summary maps for research and sequencing effort ####

# a. WOS_map_terrestrial_taxa_evenness
# b. WOS_map_marine_taxa_evenness
# c. GenBank_map_terrestrial_taxa_evenness
# d. GenBank_map_marine_taxa_evenness

plot_Fig4 <- ggarrange(fig4_wos_map_evenness_continental, fig4_wos_map_evenness_marine, 
                       fig4_genbank_map_evenness_continental, fig4_genbank_map_evenness_marine, 
                       labels = c("(a)", "(b)", "(c)", "(d)"),
                       font.label = list(size = 20),
                       ncol = 2, nrow = 2)


pdf(file = "./output/plots/Figure4_maps_evenness.pdf", width = 17, height = 10)
print(plot_Fig4)
dev.off()


#### Figure S2 : GenBank maps for plants, mammals, fungi and coleoptera.  ####

# a. 
# b. 
# c. 
# d. 


#### Figure S3 : WOS maps for plants, mammals, fungi and coleoptera.  ####

# a. 
# b. 
# c. 
# d. 


