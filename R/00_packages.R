#################################################
#
# Biodivmex project
#
# 00_packages.R
#
# jan.perret@cefe.cnrs.fr
#################################################

library(tidyverse)
library(rlist) # for function list.append()
library(sf) # for maps
library(RColorBrewer) # for map colour palette
library(rworldxtra) # for 'countriesHigh' SpatialPolygonsDataFrame object
library(grid) # for functions grid.newpage(), pushViewport(), viewport(), grid.layout()
library(ggpubr) # for function ggarrange()
library(directlabels) # for function geom_dl()
# library(gridExtra) # usefull ?