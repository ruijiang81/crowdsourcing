################################################################################
#                            Metadata Pro Processing                           #
################################################################################
#' What does it do?
#' (1) The script search the "results/metadata" folder for metadata files.
#' (2) Each metadata file has data and metadata (found in the file name)
#' (3) First, all the metadata files are appended into one long file.
#' (4) Second, the metadata file is aggregated on an instance level.
#' (5) Third, the metadata file is aggregated on a batch level.
#' (6) Finally, the ledgers are stored on disk
#' 
source(file.path(getwd(), "code", "scripts", "setup.R"))
cat_80("Metadata Pro Processing")
#'
########################
# Get ledgers metadata #
########################
#' 1. Find file names and paths
file_names <- 
    list.files(pattern = "[.]csv$", path = k_path_metadata, full.names = FALSE)
file_paths <- 
    list.files(pattern = "[.]csv$", path = k_path_metadata, full.names = TRUE)
#'
#' 2. Extract ledger metadata from file slugs
file_slugs <- file_slug_decomposition(file_names)
assertive::assert_all_are_not_na(file_slugs %>% select(database_name))
#'
##################
# Pro Processing #
##################
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
              AUC_holdout = mean(AUC_holdout, na.rm = TRUE)) %>%
    mutate(train_set_label_quality = cumsum(batch_label_quality) / seq_along(batch_label_quality))
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
           selected_payment, batch_cost, cost_so_far,
           batch_label_quality, train_set_label_quality)
#'
#################
# Store results #
#################
cat("\n-> Saving results to hard drive")
output_path <- file.path(k_path_results, "pro-processed-metadata.csv") 
write_csv(results, output_path)
#'
cat("\nCompleted")
