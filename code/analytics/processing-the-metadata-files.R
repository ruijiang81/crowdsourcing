################################################################################
#                            Metadata Pro Processing                           #
################################################################################
#' What does it do?
#' (1) The script search the "results/metadata" folder for metadata files.
#' (2) Each metadata file has data and metadata (found in the file name)
#' (3) First, all the metadata files are appended into one long file.
#' (4) Second, the metadata file is aggregated on an instance level.
#' (5) Third, the metadata file is aggregated on a batch level.
#' (6) Finally, the metadatas are stored on disk
#' 
source(file.path(getwd(), "code", "scripts", "setup.R"))
cat_80("Metadata Pro Processing")
#'
########################
# Get metadatas metadata #
########################
#' 1. Find file names and paths
file_names <- 
    list.files(pattern = "[.]csv$", path = k_path_metadata, full.names = FALSE)
file_paths <- 
    list.files(pattern = "[.]csv$", path = k_path_metadata, full.names = TRUE)
#'
#' 2. Extract metadata metadata from file slugs
file_slugs <- file_slug_decomposition(file_names)
assertive::assert_all_are_not_na(file_slugs %>% select(database_name))
#'
##################
# Pro Processing #
##################
cat_40("Aggregating metadata")
#' 1. Append all the metadata files
cat("\n-> Reading metadata")
metadatas <- data.frame()
for(l in 1:length(file_paths)){
    # Get the file data
    report_data <- read.csv(file_paths[l])
    # Get the file metadata
    report_metadata <- file_slugs[rep(l, nrow(report_data)),]
    # Append the file data and metadata
    metadata <- bind_cols(report_metadata, report_data)
    # Append the files 
    metadatas <- bind_rows(metadatas, metadata)
}
#'
#' 2. Summarise metadata on an instance level
cat("\n-> Summarising metadata on an instance level")
results <- 
    metadatas %>% 
    group_by(database_name, model_inducer, cost_function_type, 
             payment_selection_criteria, repetition, batch) %>%
    summarise(selected_payment = mean(pay),
              batch_cost = selected_payment * n(),
              batch_label_quality = 1-mean(change),
              AUC_holdout = mean(AUC_holdout, na.rm = TRUE),
              observations_so_far = n()) %>%
    mutate(train_set_label_quality = cumsum(batch_label_quality) / seq_along(batch_label_quality),
           observations_so_far = cumsum(observations_so_far))
#'
#' 3. Summarise metadata on a batch level
cat("\n-> Summarising metadata on a batch level")
results <- 
    results %>% 
    group_by(database_name, model_inducer, cost_function_type, 
             payment_selection_criteria, repetition) %>%
    mutate(cost_so_far = cumsum(batch_cost))
#'
#' 4. Tidy the results
results <- 
    results %>% 
    select(database_name, model_inducer, cost_function_type, 
           payment_selection_criteria, repetition, batch,
           AUC_holdout,
           selected_payment, observations_so_far, batch_cost, cost_so_far,
           batch_label_quality, train_set_label_quality)
#'
######################################
# Interpulate the metadata by dollar #
######################################
cat_40("Interpulating metadata reports")
results <- 
    results %>% 
    group_by(database_name, model_inducer, cost_function_type, 
             payment_selection_criteria, repetition)
cat("\n-> AUC_holdout as a function of cost")
interpolated_results_1 <- 
    interpolate_to_the_nearest_dollar(data = results,
                                      x_col = "cost_so_far",
                                      y_col = "AUC_holdout",
                                      x_out = 40:150)
cat("\n-> train_set_label_quality as a function of cost")
interpolated_results_2 <- 
    interpolate_to_the_nearest_dollar(data = results,
                                      x_col = "cost_so_far",
                                      y_col = "train_set_label_quality",
                                      x_out = 40:150)
cat("\n-> observations_so_far as a function of cost")
interpolated_results_3 <- 
    interpolate_to_the_nearest_dollar(data = results,
                                      x_col = "cost_so_far",
                                      y_col = "observations_so_far",
                                      x_out = 40:150)
cat("\n-> Combining the interpolated results")
suppressMessages(
    interpolated_results <- 
        interpolated_results_1 %>%
        full_join(interpolated_results_2) %>%
        full_join(interpolated_results_3)
)
assertive::assert_any_are_not_na(interpolated_results)
#'
#################
# Store results #
#################
output_folder <- file.path(k_path_results, "processed")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

output_path_1 <- file.path(output_folder, "metadata-combined.csv") 
output_path_2 <- file.path(output_folder, "metadata-by-dollar.csv") 

write_csv(results, output_path_1)
write_csv(interpolated_results, output_path_2)
#'
cat("\nCompleted")
