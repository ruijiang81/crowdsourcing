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
policies_metadata = data.frame(
    matrix(
        c("max_pay_per_label100", "MaxPay",    "blue",   "solid",
          "max_ratio100",         "ALP-MR",    "red",    "solid",
          "max_ratio1e+06",       "ALP-MR-h",  "green",  "solid",
          "max_total_ratio100",   "ALP-MTR",   "orange", "solid",
          "max_total_ratio1e+06", "ALP-MTR-h", "pink",   "solid",
          "min_pay_per_label100", "MinPay",    "gold",   "solid",
          "random100",            "Uniform",   "black",  "dotted"),
        ncol=4,byrow=T),stringsAsFactors=FALSE)  
colnames(policies_metadata) = c("names_original","names_new","color","linetype")

# http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
# "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678"

# ggplot2 legend : Easy steps to change the position and the appearance of a graph legend in R software
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software











