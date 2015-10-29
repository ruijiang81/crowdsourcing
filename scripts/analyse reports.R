################################################################################
## Analyse Reports
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$",path="./functions/",full.names=TRUE), source)


## Get the data
reports_folder = file.path(getwd(),"reports")
reports = import.reports(reports_folder)


## Calculate AUC(Cost)
outputs = interpolate.reports(reports_folder, na.rm=FALSE)

