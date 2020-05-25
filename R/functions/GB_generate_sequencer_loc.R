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