################################################################################
#                            Reprot Pro Processing                             #
################################################################################
#'
source(file.path(getwd(), "code", "scripts", "setup.R"))
cat_80("Report Pro Processing")
#'
########################
# Get ledgers metadata #
########################
#' 1. Find file names and paths
file_names <-
    list.files(pattern = "[.]csv$", path = k_path_reports, full.names = FALSE)
file_paths <-
    list.files(pattern = "[.]csv$", path = k_path_reports, full.names = TRUE)
#'
#' 2. Extract report metadata from file slugs
file_slugs <- file_slug_decomposition(file_names)
assertive::assert_all_are_not_na(file_slugs %>% select(database_name))
#'
####################################
# Interpulate the report by dollar #
####################################
cat_40("Interpulating reports")
interval_size <- 1
reports <- interpolate.reports(reports_folder = k_path_reports,
                               na.rm = FALSE,
                               interval_size = interval_size)
#' Shape the table
reports <- 
    reports %>%
    rename(database_name = DATABASE_NAME,
           AUC_holdout = average_holdout_cost_performance,
           cost_so_far = cost_intervals) %>%
    select(database_name, model_inducer, cost_function_type,
           payment_selection_criteria, repetition, cost_so_far,
           AUC_holdout)
#'
#################
# Store results #
#################
output_folder <- file.path(k_path_results, "processed")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# output_path_1 <- file.path(output_folder, "reports-combined.csv")
output_path_2 <- file.path(output_folder, "reports-by-dollar.csv")

# write_csv(results, output_path_1)
write_csv(reports, output_path_2)
#'
cat("\nCompleted")
