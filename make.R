#################################################
#
# Biodivmex project
#
# make.R
#
# jan.perret@cefe.cnrs.fr
#################################################

##################
# set working directory to source file location
##################
# library(utils)
# setwd(utils::getSrcDirectory()[1]) # to set the working directory to source file location if not run in Rstudio
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # to set the working directory to source file location in Rstudio
# getwd()

##################
# clean workspace
##################
rm(list = ls())

##################
# loading packages
##################
source("R/00_packages.R")

##################
# loading functions for GenBank data analysis
##################
source("R/00_functions_GenBank.R")

##################
# loading functions for Web of Science data analysis
##################
source("R/00_functions_WOS.R")

##################
# data formating for GenBank
##################
source("R/GenBank_02_data_formating.R")

##################
# curves for GenBank
##################
source("R/GenBank_03_curves.R")

##################
# maps for GenBank
##################
source("R/GenBank_04_maps.R")

##################
# data formating for WOS
##################
source("R/WOS_02_data_formating.R")

##################
# curves for WOS
##################
source("R/WOS_03_curves.R")

##################
# maps for WOS
##################
source("R/WOS_04_maps.R")

