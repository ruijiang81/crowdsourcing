################################################################################
#                                    Setup                                     # 
################################################################################
#' 
#' Configurations
options(digits=4)
Sys.setlocale("LC_TIME", "English")
set.seed(NULL)
UID <<- paste0(sample(c(letters, 0:9, toupper(letters)), 20, replace=T),
               collapse="")
#'
#' Define folder paths
k_path_project <<- getwd()
k_path_scripts <<- file.path(k_path_project, "code", "scripts")
k_path_modeling <<- file.path(k_path_project, "code", "modeling")
k_path_functions <<- file.path(k_path_project, "code", "R")
k_path_temporary <<- file.path(k_path_project, "results", "temp folder", UID)
#'
#' Load project's functions
invisible(
    sapply(
        list.files(pattern = "[.]R$",
                   path = k_path_functions,
                   full.names = TRUE),
        source)
)
#'
#' Load project's modules
invisible(
    sapply(
        list.files(pattern = "[.]R$",
                   path = k_path_modeling,
                   full.names = TRUE),
        source)
)
#' 
#' Load project's libraries
source(file.path(k_path_scripts, "load-libraries.R"))

