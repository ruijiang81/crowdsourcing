#' @title File slug decomposition
#'
#' @examples 
#' file_names <- list.files(pattern = "[.]csv$", path = k_path_ledgers)
#' slugs_df <- file_slug_decomposition(file_names)
#'
file_slug_decomposition <- function(file_names){
    ####################
    # Input validation #
    ####################
    assertive::assert_is_character(file_names)
    #'
    ###################
    # Decompose slugs #
    ###################
    slugs_list <- 
        stringr::str_extract_all(file_names, "(([^()]+))")
    slugs_df <- 
        slugs_list %>% 
        unlist %>% 
        matrix(nrow=length(file_names), byrow = TRUE) %>% 
        data.frame(stringsAsFactors = FALSE) %>%
        select(X1, X2, X3, X4) %>%
        dplyr::rename("DATABASE_NAME" = "X1",
                      "MODEL_INDUCER" = "X2",
                      "COST_FUNCTION_TYPE" = "X3",
                      "PAYMENT_SELECTION_CRITERIA" = "X4")
    #'
    ##########
    # Return #
    ##########
    return(slugs_df)
}


