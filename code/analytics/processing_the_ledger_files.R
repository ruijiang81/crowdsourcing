################################################################################
#                            Ledger Pro Processing                             #
################################################################################
#' What does it do?
#' (1) The script search the "results/ledgers" folder for ledger files.
#' (2) Each ledger has data and metadata (found in the file name)
#' (3) First, the ledger data is loaded and processed via 
#'     ledger_feature_extractor()
#'     (3.1) The ledger reports intermediate calculation at time (t) for what 
#'           should be the payment in time (t+1). It contains the calculation  
#'           for allavailable payments. Therefore, The functions starts by going  
#'           over each ledger row, and keeping only the information about the  
#'           suggested payment for the batch in time (t+1).
#'     (3.2) The expected performance for the batch in (t+1) is computed and 
#'           stored in time (t). Therefore, for fair comparison, the function  
#'           shifs the expected performance to the next ledger row.
#' (4) Second, the ledger metadata is added to the processed ledger
#' (5) Finally, the ledgers are stored on disk
#' 
source(file.path(getwd(), "code", "scripts", "setup.R"))
cat_80("Ledger Pro Processing")
#'
########################
# Get ledgers metadata #
########################
#' 1. Find file names and paths
file_names <- 
    list.files(pattern = "[.]csv$", path = k_path_ledgers, full.names = FALSE)
file_paths <- 
    list.files(pattern = "[.]csv$", path = k_path_ledgers, full.names = TRUE)
#'
#' 2. Extract ledger metadata from file slugs
file_slugs <- file_slug_decomposition(file_names)
assertive::assert_all_are_not_na(file_slugs %>% select(database_name))
#'
##################
# Pro Processing #
##################
cat_40("Aggregating ledgers")
results <- data.frame()
for(l in 1:length(file_paths)){
    #' 1. Import the ledger
    suppressMessages({
        ledger <- read_csv(file_paths[l])
        ledger <- ledger %>% mutate(payment_selected = factor(payment_selected))
    })
    
    #' 2. If batch_size column doesn't exist, add it with a default value of 10
    if(!isTRUE("batch_size" %in% colnames(ledger)))
        ledger <- ledger %>% mutate(batch_size = 10) %>% select(repetition, batch, batch_size, everything())
    
    #' 3. Feature extractor
    ledger_data <- switch(ledger %>% .$payment_selection_criteria %>% unique(),
                          max_total_ratio = {ledger_feature_extractor(ledger)},
                          ledger %>% select(-payment_selection_criteria))
    
    #' 4. Add the total cost
    ledger_data <-
        ledger_data %>% 
        group_by(repetition) %>% 
        mutate(cost_so_far = cumsum(batch_size * (payment_selected %>% as.character() %>% as.numeric()))) %>% 
        select(repetition, batch, batch_size, cost_so_far, everything()) %>%
        ungroup()
    
    #' 5. Add ledger metadata
    ledger_metadata <- file_slugs[rep(l, nrow(ledger_data)),]
    ledger <- bind_cols(ledger_metadata, ledger_data)
    
    #' 6. Append ledger
    results <- bind_rows(results, ledger)
    assertive::assert_all_are_not_na(results %>% select(database_name))
}
#'
####################################
# Interpulate the ledger by dollar #
####################################
cat_40("Interpulating ledgers")
results <- results %>% group_by(database_name, model_inducer, cost_function_type, payment_selection_criteria, repetition)
interpolated_results <- interpolate_to_the_nearest_dollar(data = results,
                                                          x_col = "cost_so_far",
                                                          y_col = "AUC_holdout_set",
                                                          x_out = 40:150)
#'
#################
# Store results #
#################
output_folder <- file.path(k_path_results, "processed")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

output_path_1 <- file.path(output_folder, "ledgers-combined.csv") 
output_path_2 <- file.path(output_folder, "ledgers-by-dollar.csv") 

write_csv(results, output_path_1)
write_csv(interpolated_results, output_path_2)
#'
cat("\nCompleted")
