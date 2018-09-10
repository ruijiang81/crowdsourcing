################################################################################
#                           Load project's libraries                           #
################################################################################
#' browseVignettes()
#'
#' There are three groups for the project's packages:
#' 1. Package is on GitHub; install and load the latest package from GitHub.
#' 2. Package is on MRAN; install and load a spesific package version from CRAN.
#' Comments:
#' * The 1nd group can include:
#'    * Packages that were not present during the original experiment, or
#'    * packages that are not relevant for reproducibility (e.g. ggplot2) or
#'    * packages from CRAN which should be in their latest version (e.g. dropbox).
#' * The 2nd group is usful for reproducible research
#'
paste0(R.Version())[c("major", "minor")]
k_snapshot_date <<- "2016-05-26"
k_path_project <<- getwd()
k_path_checkpoint <<- file.path(k_path_project, ".checkpoint")
k_path_libraries <<- .libPaths()[which.min(nchar(.libPaths()))]
stopifnot(grepl("C:/Program Files(.*)/library", k_path_libraries))
#'
####################
# Input validation #
####################
stopifnot(exists("create_requirements_file"))
#' Check R version
d1 <- as.numeric(R.Version()$major)
d2 <- as.numeric(substr(R.Version()$minor, 1, 1))
d3 <- as.numeric(substr(R.Version()$minor, 3, 3))
if (d1 != 3 | d2 > 4) {
    stop(
        "\nThe project was tasted with R version 3.4.3",
        "\nYou are using R version ", paste0(R.Version()[6:7], collapse = "."),
        "\nWe can't guarantee the results are reproducible"
    )
}
#'
############################
# Define project libraries #
############################
#' The packages are grouped in three categories:
#' 1. libraries_on_MRAN; these packages are installed at their past versions for
#'    research reproducibility purposes.
#' 2. libraries_on_CRAN; these packages are installed at their latest versions.
#' 3. libraries_on_GitHub; these packages are usually not on CRAN and therefore
#'    we install from GitHub
#'
libraries_on_MRAN <- c(
    # Classification Algorithms
    "randomForest", "e1071", "ipred",
    # Evaluating learning algorithms
    "ROCR", "AUC",
    # Parallel Tools
    "doParallel", "foreach",
    # Spam dataset
    "kernlab"
)
#'
libraries_on_CRAN <- c(
    # Defensive R Programming
    "assertive",
    # Pretty Printing of R Code
    "styler",
    # Unit Testing
    # <http://r-pkgs.had.co.nz/tests.html>
    "testthat",
    # Data Manipulation
    "tidyverse", "data.table", "dplyr", "magrittr",
    # Dynamic Report Generation in R
    "knitr", "pander",
    # Visualization tools
    "ggplot2", "gridExtra", "lemon"
)
#'
libraries_on_GitHub <- c(
    # Required by ROCR
    "cran/gtools", "cran/gplots",
    # Conflict packages resolution strategy; filter <- dplyr::filter
    # "r-lib/conflicted",
    # Addin to RStudio, which finds all TODO, FIXME, CHANGED
    "dokato/todor"
)
#'
###########################
# Install & Load Packages #
###########################
#' Remove all packages that do not come with R
#' remove.packages( installed.packages( priority = "NA" )[,1] )
#'
suppressMessages({
    if (!require("versions")) {
        install.packages("versions")
    }
    require("versions")
    if (!require("pacman")) {
        install.packages("pacman")
    }
    require("pacman")
})
#' Step 1: Explicitly tell the package manager what libraries to include
create_requirements_file(libraries_on_MRAN)
#' Step 2: Install and load packages from MRAN
message("# Install and load MRAN packages")
pb <- txtProgressBar(0, length(libraries_on_MRAN), style = 3)
i <- 0
for (package in libraries_on_MRAN) {
    suppressPackageStartupMessages({
        suppressMessages({
            if (!require(package, character.only = TRUE)) {
                install.dates(package, k_snapshot_date, k_path_libraries)
            }
            require(package, character.only = TRUE)
        })
        # Advance Progress Bar
        i <- i + 1
        setTxtProgressBar(pb, i)
    })
}
message("")
#' Step 3: Install and load packages from CRAN
message("# Install and load CRAN packages")
pb <- txtProgressBar(0, length(libraries_on_CRAN), style = 3)
i <- 0
for (package in libraries_on_CRAN) {
    suppressPackageStartupMessages({
        suppressMessages({
            if (!require(package, character.only = TRUE)) {
                install.packages(pkgs = package, lib = k_path_libraries)
            }
            require(package, character.only = TRUE)
        })
        # Advance Progress Bar
        i <- i + 1
        setTxtProgressBar(pb, i)
    })
}
message("")
#' Step 4: Install and load GitHub packages
message("# Install and load GitHub packages")
pb <- txtProgressBar(0, length(libraries_on_GitHub), style = 3)
i <- 0
for (package in libraries_on_GitHub) {
    suppressPackageStartupMessages({
        suppressMessages({
            try(withr::with_libpaths(
                new = k_path_libraries,
                code = p_load_gh(char = package)
            ),
            silent = FALSE
            )
        })
        # Advance Progress Bar
        i <- i + 1
        setTxtProgressBar(pb, i)
    })
}
message("")
#' Step 5: Make sure all packages were installed correctly
libraries_on_System <- row.names(installed.packages())
assertive::assert_is_subset(libraries_on_MRAN, libraries_on_System)
assertive::assert_is_subset(libraries_on_CRAN, libraries_on_System)
