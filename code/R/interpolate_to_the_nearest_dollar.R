#' interpolate_to_the_nearest_dollar
#'
#'
interpolate_to_the_nearest_dollar <- function(data, x_col, y_col, x_out) {
    ####################
    # Input validation #
    ####################
    assertive::assert_is_data.frame(data)
    if (!is.grouped_df(data)) stop("\nThe dataset is not grouped by any column;\nUse dplyr::group_by")
    assertive::assert_all_are_non_missing_nor_empty_character(c(x_col, y_col))
    assertive::assert_is_subset(c(x_col, y_col), colnames(data))
    if (missing(x_out)) x_out <- 1:150 else assertive::assert_all_are_whole_numbers(x_out)
    #'
    #########
    # Setup #
    #########
    results_by_dollar <- data.frame()
    keys <- data %>% select(group_vars(data)) %>% unique()
    K <- nrow(keys)
    #'
    ##########################
    # Interpolating the data #
    ##########################
    cat("\n")
    pb <- txtProgressBar(0, K, style = 3)
    for (k in 1:K) {
        #' Subset the data to hold a single repetition
        data_subset <- data %>% as.data.frame()
        for (l in 1:ncol(keys)) {
            col_name <- colnames(keys)[l]
            col_value <- keys[k, l]
            data_subset <- data_subset[data_subset[, col_name] %in% col_value, ]
        } # for(l in 1:ncol(keys))

        #' Extract interpolation variables
        x <- data_subset %>% select(x_col, y_col) %>% drop_na() %>% .[[x_col]]
        y <- data_subset %>% select(x_col, y_col) %>% drop_na() %>% .[[y_col]]

        #' Apply interpolation
        y_out <- interpolation.kernel.customized(x, y, x_out)[["yout"]]

        #' Store results
        interpolated_result <- bind_cols(x_col = x_out, y_col = y_out)
        colnames(interpolated_result) <- c(x_col, y_col)
        interpolated_result <- bind_cols(keys[rep(k, nrow(interpolated_result)), ], interpolated_result)
        results_by_dollar <- results_by_dollar %>% bind_rows(interpolated_result)

        #' Advance the progress bar
        setTxtProgressBar(pb, k)
    } # for(k in 1:K)

    ##########
    # Return #
    ##########
    return(results_by_dollar)
}
