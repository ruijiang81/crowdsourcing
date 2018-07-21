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
for (l in 1:length(file_paths)) {
    # Get the file data
    report_data <- read.csv(file_paths[l])
    # Get the file metadata
    report_metadata <- file_slugs[rep(l, nrow(report_data)), ]
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
    group_by(
        database_name, model_inducer, cost_function_type,
        payment_selection_criteria, repetition, batch
    ) %>%
    summarise(
        selected_payment = mean(pay),
        batch_cost = selected_payment * n(),
        batch_label_quality = 1 - mean(change),
        AUC_holdout = mean(AUC_holdout, na.rm = TRUE)
    ) %>%
    mutate(train_set_label_quality = cumsum(batch_label_quality) / seq_along(batch_label_quality))
#'
#' 3. Summarise metadata on a batch level
cat("\n-> Summarising metadata on a batch level")
results <-
    results %>%
    group_by(
        database_name, model_inducer, cost_function_type,
        payment_selection_criteria, repetition
    ) %>%
    mutate(cost_so_far = cumsum(batch_cost))
#'
#' 4. Tidy the results
results <-
    results %>%
    select(
        database_name, model_inducer, cost_function_type,
        payment_selection_criteria, repetition, batch,
        AUC_holdout,
        selected_payment, batch_cost, cost_so_far,
        batch_label_quality, train_set_label_quality
    )
#'
#'
######################################
# Interpulate the metadata by dollar #
######################################
cat_40("Interpulating metadatas")
#' Configure the loop variables
results_by_dollar <- data.frame()
keys <-
    metadatas %>%
    select(database_name, model_inducer, cost_function_type, payment_selection_criteria, repetition) %>%
    unique()
K <- nrow(keys)

#' Configure the interpolation variables
x_out_range <- metadatas %>% .$cost_so_far %>% range() %>% ceiling()
x_out <- x_out_range[1]:x_out_range[2]
x_col <- "cost_so_far"
y_col <- "AUC_holdout"

#' Interpolate the metadatas
cat("\n")
pb <- txtProgressBar(0, K, style = 3)
for (k in 1:K) {
    #' Input validation
    assertive::assert_is_data.frame(metadatas)
    assertive::assert_all_are_non_missing_nor_empty_character(c(x_col, y_col))
    assertive::assert_all_are_positive(x_out)

    #' Subset the data to hold a single repetition
    data <-
        metadatas %>%
        filter(
            database_name == keys[k, "database_name"],
            model_inducer == keys[k, "model_inducer"],
            cost_function_type == keys[k, "cost_function_type"],
            payment_selection_criteria == keys[k, "payment_selection_criteria"],
            repetition == keys[k, "repetition"]
        )

    #' Extract interpolation variables
    x <- data %>% select(x_col, y_col) %>% drop_na() %>% .[[x_col]]
    y <- data %>% select(x_col, y_col) %>% drop_na() %>% .[[y_col]]

    #' Apply interpolation
    y_out <- interpolation.kernel.customized(x, y, x_out)[["yout"]]

    #' Store results
    interpolated_result <- bind_cols(x_col = x_out, y_col = y_out)
    colnames(interpolated_result) <- c(x_col, y_col)
    interpolated_result <- bind_cols(keys[rep(k, nrow(interpolated_result)), ], interpolated_result)
    results_by_dollar <- results_by_dollar %>% bind_rows(interpolated_result)

    #' Advance the progress bar
    setTxtProgressBar(pb, k)
}
#'
#################
# Store results #
#################
output_path_1 <- file.path(k_path_results, "pro-processed-metadata.csv")
output_path_2 <- file.path(k_path_results, "pro-processed-metadata-by-dollar.csv")

write_csv(results, output_path_1)
write_csv(results_by_dollar, output_path_2)
#'
cat("\nCompleted")
