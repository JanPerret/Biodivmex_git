### make the text with general informations to add as subtitles to the maps

WOS_make_map_subtitle <- function(taxa_table, taxa_name) {

  text_n_articles <- paste("Total number of articles : ", taxa_table$n_articles[which(taxa_table$taxa == taxa_name)], sep="")
  text_loc_rate <- paste("Study area assignation rate : ", taxa_table$loc_rate[which(taxa_table$taxa == taxa_name)], " %", sep="")
  text_subtitle <- paste(text_n_articles, text_loc_rate, sep=" ; ")
  
  return(text_subtitle)
}