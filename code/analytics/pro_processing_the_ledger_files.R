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
ledgers <- data.frame()
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
    ledgers <- bind_rows(ledgers, ledger)
    assertive::assert_all_are_not_na(ledgers %>% select(database_name))
}
#'
####################################
# Interpulate the ledger by dollar #
####################################
cat_40("Interpulating ledgers")
#' Configure the loop variables
ledgers_by_dollar <- data.frame()
keys <- 
    ledgers %>% 
    select(database_name, model_inducer, cost_function_type, payment_selection_criteria, repetition) %>%
    unique()
K <- nrow(keys)

#' Configure the interpolation variables
x_out_range <- ledgers %>% .$cost_so_far %>% range() %>% ceiling()
x_out <- x_out_range[1]:x_out_range[2]
x_col <- "cost_so_far"
y_col <- "AUC_holdout_set"

#' Interpolate the ledgers 
cat("\n")
pb <- txtProgressBar(0, K, style = 3)
for(k in 1:K){
    #' Input validation
    assertive::assert_is_data.frame(ledgers)
    assertive::assert_all_are_non_missing_nor_empty_character(c(col_x, col_y))
    assertive::assert_all_are_positive(x_out)
    
    #' Subset the data to hold a single repetition
    data <-
        ledgers %>%
        filter(database_name              == keys[k, "database_name"],
               model_inducer              == keys[k, "model_inducer"],
               cost_function_type         == keys[k, "cost_function_type"],
               payment_selection_criteria == keys[k, "payment_selection_criteria"],
               repetition                 == keys[k, "repetition"])
    
    #' Extract interpolation variables
    x <- data %>% select(x_col, y_col) %>% drop_na() %>% .[[x_col]] 
    y <- data %>% select(cost_so_far, AUC_holdout_set) %>% drop_na() %>% .[[y_col]] 
    
    #' Apply interpolation
    y_out <- interpolation.kernel.customized(x, y, x_out)[["yout"]]
    
    #' Store results
    interpolated_ledger <- bind_cols(x_col = x_out, y_col = y_out)
    colnames(interpolated_ledger) <- c(x_col, y_col)
    interpolated_ledger <- bind_cols(keys[rep(k, nrow(interpolated_ledger)),], interpolated_ledger)
    ledgers_by_dollar <- ledgers_by_dollar %>% bind_rows(interpolated_ledger)
    
    #' Advance the progress bar
    setTxtProgressBar(pb, k)
}
#'
#################
# Store results #
#################
output_path_1 <- file.path(k_path_results, "pro-processed-ledgers.csv") 
output_path_2 <- file.path(k_path_results, "pro-processed-ledgers-by-dollar.csv") 

write_csv(ledgers, output_path_1)
write_csv(ledgers_by_dollar, output_path_2)
#'
cat("\nCompleted")
