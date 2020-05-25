#################################################
#
# Biodivmex project
#
# make.R
#
# jan.perret@cefe.cnrs.fr
#################################################

##################
# clean workspace
##################
rm(list = ls())

##################
# load packages
##################
source("R/00_packages.R")

##################
# load functions
##################
files.sources <- list.files("./R/functions", full.names = TRUE)
sapply(files.sources, source)

##################
# format data for GenBank
##################
source("R/GenBank_02_data_formating.R")

##################
# make curves for GenBank
##################
source("R/GenBank_03_curves.R")

##################
# make maps for GenBank
##################
source("R/GenBank_04_maps.R")

##################
# format data for WOS
##################
source("R/WOS_02_data_formating.R")

##################
# make curves for WOS
##################
source("R/WOS_03_curves.R")

##################
# make maps for WOS
##################
source("R/WOS_04_maps.R")

