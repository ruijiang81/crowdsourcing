#' add_metadata_from_file_name
#'
add_metadata_from_file_name <- function(data, file_name) {
    ####################
    # Input validation #
    ####################
    assert_is_data.frame(data)
    assert_all_are_non_missing_nor_empty_character(file_name)
    assert_all_are_existing("file_slug_decomposition", globalenv())
    #'
    ##############
    # Parse slug #
    ##############
    slug_df <- file_slug_decomposition(file_name)
    #'
    ####################
    # Process the data #
    ####################
    # Drop existing columns
    suppressWarnings(data <- data %>% select(-one_of(colnames(slug_df))))
    # Join the slug and the data
    data <- bind_cols(slug_df[rep(1, nrow(data)), ], data)
    #'
    ##########
    # Return #
    ##########
    return(data)
}
