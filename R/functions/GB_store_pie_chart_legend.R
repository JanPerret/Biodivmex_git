### store the legend for the pie charts with 3 categories
GB_store_pie_chart_legend <- function() {
  # make a small tibble containing just the 3 lines from_country / inside_med / outside_med
  tibble_for_pie_legend <- tibble(author_loc = c("from_country", "inside_med", "outside_med"), sum_n_articles = c(457,143,130))
  
  # make a pie chart from the small tibble
  pie_for_legend <- ggplot(tibble_for_pie_legend, aes(x=1, y = sum_n_articles, fill = author_loc)) +
    geom_bar(width = 1, stat = "identity", colour = "black") +
    coord_polar("y", start=0) +
    geom_text(aes(label = sum_n_articles, group = author_loc),
              position = position_stack(vjust = 0.5, reverse = FALSE), size = 2.5) +
    scale_fill_manual(values = c("#636363", "#bdbdbd", "#FFFFFF")) + # labels = c("from the country", "mediterranean country", "outside med"), 
    labs(fill='Sequencer identity') +
    theme(legend.background=element_blank(), legend.position="right")
  
  # function to extract legend 
  g_legend <- function(a.gplot){ 
    tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
    legend <- tmp$grobs[[leg]] 
    return(legend)} 
  
  # extract legend
  legend_pie_3_categories <- g_legend(pie_for_legend)
  legend_pie_3_categories <- ggpubr::ggarrange(legend_pie_3_categories, ncol = 1, nrow = 1)
  
  return(legend_pie_3_categories)
}