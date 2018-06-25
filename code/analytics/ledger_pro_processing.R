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
#'
##################
# Pro Processing #
##################
ledgers <- data.frame()
for(l in 1:length(file_paths)){
    #' 1. Import the ledger
    suppressMessages(
        ledger <- read_csv(file_paths[l])
    )
    #' 2. Feature extractor
    ledger_data <- ledger_feature_extractor(ledger)
    #' 3. Add ledger metadata
    ledger_metadata <- file_slugs %>% slice(l) #%>% select(database_name, )
    ledger_metadata <- ledger_metadata[rep(l, nrow(ledger_data)),]
    ledger <- bind_cols(ledger_metadata, ledger_data)
    #' 4. Append leger
    ledgers <- bind_rows(ledgers, ledger)
}
#'
#################
# Store results #
#################
output_path <- file.path(k_path_results, "pro-processed-ledgers.csv") 
write_csv(ledgers, output_path)
#'
cat("\nCompleted")
