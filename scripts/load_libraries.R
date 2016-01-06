# Sys.setlocale("LC_TIME", "English") #uses english opertaing system naming convention
################################################################################
## Load Libraries
################################################################################
## Github packages
# if (!require("devtools")) {
#         install.packages("devtools")
#         require("devtools")
# }
## Rtools
# if (!require("installr")) install.packages("installr")
# installr::install.Rtools()
## CRAN packages
packages.loader <- function(packages.list){
        suppressPackageStartupMessages(
                for (p in packages.list){
                        if(!require(p, character.only=TRUE)){
                                install.packages(p,dep=TRUE) # install form CRAN
                                require(p, character.only=TRUE)
                        } # end if require
                } # end for packages list
        ) # end suppressPackageStartupMessages
} # end functions packages.loader
packages.list = c("testthat",                                                   # Development tools for R
                  "dplyr","plyr",                                               # Data Manipulation
                  "randomForest","ROCR","e1071", #"RWeka",                      # Classification Algorithms
                  "doParallel","foreach",                                       # Parallel Tools
                  "knitr","pander",                                             # Dynamic Report Generation in R
                  "ggplot2","manipulate",                                       # Visualization tools
                  "kernlab")#,                                                  # Spam dataset
                  #"markovchain")

packages.loader(packages.list)
## Clean Up
rm(packages.list)
# rm(list = ls()); cat("\014")
