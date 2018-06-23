################################################################################
#                            Ledger Pro Processing                             #
################################################################################
source(file.path(getwd(), "code", "scripts", "setup.R"))
#'
###################
# Get the ledgers #
###################
#' 1. Find file names and paths
file_names <- 
    list.files(pattern = "[.]csv$", path = k_path_ledgers, full.names = FALSE)
file_paths <- 
    list.files(pattern = "[.]csv$", path = k_path_ledgers, full.names = TRUE)
#'
#' 2. Extract ledger metadata from file slugs
file_slugs <- file_slug_decomposition(file_names)
#'
###########
#
###

#'
