#' create_requirements_file
#'
#' @title Create requirements.R
#'
#' @description Create requirements.R; an R script with the project libraries.
#' This useful when using a package manager as it searches the project's files
#' to determine which libraries are required for the project.
#'
#' @param packages_list character vector with packages names. The name may
#' contain a GitHub slug such as "tidyverse/ggplot2". The function removes the
#' slug and converts it into "ggplot2".
#' @param folder_path character vector specifying where to save the file.
#'
#' @details
#' STEP 1: Remove "/" from packages_list elements
#' STEP 2: Create requirements.R in the folder path
#'
#' @author Harel Lustiger
#'
create_requirements_file <- function(packages_list, folder_path = getwd()) {
    ####################
    # Input validation #
    ####################
    stopifnot(
        !missing(packages_list),
        is.character(packages_list)
    )
    dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
    #'
    ##########
    # STEP 1 #
    ##########
    packages_list_no_slash <- strsplit(packages_list, split = "./")
    packages_list_no_slash <- lapply(packages_list_no_slash, function(x) x[[length(x)]])
    packages_list_no_slash <- unlist(packages_list_no_slash)
    #'
    ##########
    # STEP 2 #
    ##########
    file_content <- paste0("library", "(", packages_list_no_slash, ")")
    file_path <- file.path(folder_path, "requirements.R")
    write.table(file_content, file_path,
        col.names = FALSE, row.names = FALSE,
        quote = FALSE
    )
    #'
    ##########
    # Return #
    ##########
    return(invisible())
}
