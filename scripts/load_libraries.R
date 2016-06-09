# Sys.setlocale("LC_TIME", "English") #uses english opertaing system naming convention
#' 1. Load Libraries
#' 2. Environment Variables


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
                  "dplyr","plyr","tidyr",                                       # Data Manipulation
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


################################################################################
## Environment Variables
################################################################################
# Policies names
policies_names_original = c("max_pay_per_label100", "max_ratio100",  "max_ratio1e+06", "max_total_ratio100",  "max_total_ratio1e+06", "min_pay_per_label100", "random100") 
policies_names_new      = c("Maximum Payment",      "ALP-MR",        "ALP-MR-h",       "ALP-MTR",              "ALP-MTR-h",           "Minimum Payment",      "Uniform")




