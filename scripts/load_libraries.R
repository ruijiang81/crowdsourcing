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
                  "randomForest","ROCR","e1071","ipred",#"RWeka",               # Classification Algorithms
                  "doParallel","foreach",                                       # Parallel Tools
                  "knitr","pander",                                             # Dynamic Report Generation in R
                  "ggplot2","gridExtra","manipulate",                           # Visualization tools
                  "kernlab")                                                    # Spam dataset
packages.loader(packages.list)
## Clean Up
rm(packages.list)
# rm(list = ls()); cat("\014")


################################################################################
## Environment Variables
################################################################################
policies_metadata = data.frame(
    matrix(
        c("max_pay_per_label100", "MaxPay",          "deepskyblue4", "solid", "0",
          "max_ratio100",         "ALP-MR",          "blue",         "solid", "1",
          "max_total_ratio100",   "ALP-MTR",         "red",          "solid", "3",
          "min_pay_per_label100", "MinPay",          "brown",        "solid", "5",
          "random100",            "Uniform",         "black",        "solid", "6",
          # PAYMENT REGULATION
          "max_total_ratio100_reg", "ALP-MTR-PAYMENT-REG", "brown4", "solid", "3",
          # DIFFERENT BATCH SIZES
          "random100_05_instancesperbatch",          "Uniform-05", "black", "solid", "6",
          "random100_10_instancesperbatch",          "Uniform-10", "black", "solid", "6",
          "random100_15_instancesperbatch",          "Uniform-15", "black", "solid", "6",
          "max_total_ratio100_05_instancesperbatch", "ALP-MTR-BATCH-SIZE-05", "darkgreen",     "solid", "1",
          "max_total_ratio100_10_instancesperbatch", "ALP-MTR-BATCH-SIZE-10", "red",           "solid", "2",
          "max_total_ratio100_15_instancesperbatch", "ALP-MTR-BATCH-SIZE-15", "darkgoldenrod", "solid", "3",
          # FULL HISTORY
          "max_ratio1e+06",       "ALP-MR-FULL-HISTORY",  "green",  "solid", "2",
          "max_total_ratio1e+06", "ALP-MTR-FULL-HISTORY", "orange", "solid", "4",
          # SINGLE-CV
          "max_total_ratio100-rcv",              "ALP-MTR-SINGLE-CV",       "SaddleBrown",   "solid", "0",
          # SINGLE-OMISSION
          "max_total_ratio100-1_batch_omission", "ALP-MTR-SINGLE-OMISSION", "ForestGreen",   "solid", "0",
          # Repeated Labeling
          "max_ratio100-rl3",         "ALP-MR-REPEATED-LABELING",          "blue",   "solid", "1",
          "max_ratio1e+06-rl3",       "ALP-MR-HISTORY-REPEATED-LABELING",  "green",  "solid", "2",
          "max_total_ratio100-rl3",   "ALP-MTR-REPEATED-LABELING",         "red",    "solid", "3",
          "max_total_ratio1e+06-rl3", "ALP-MTR-HISTORY-REPEATED-LABELING", "orange", "solid", "4",
          "random100-rl3",            "Uniform-REPEATED-LABELING",         "black",  "solid", "6"
        ),
        ncol=5,byrow=T),stringsAsFactors=FALSE)  
colnames(policies_metadata) = c("names_original","names_new","color","linetype","pch")

# http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
# "blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash", "1F", "F1", "4C88C488", "12345678"

# ggplot2 legend : Easy steps to change the position and the appearance of a graph legend in R software
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software

# Piont types
# http://sape.inf.usi.ch/quick-reference/ggplot2/shape









