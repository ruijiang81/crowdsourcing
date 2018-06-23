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
        data.frame() %>%
        select(X1, X2, X3, X4) %>%
        dplyr::rename("DATABASE_NAME" = "X1",
                      "MODEL_INDUCER" = "X2",
                      "COST_FUNCTION_TYPE" = "X3",
                      "PAYMENT_SELECTION_CRITERIA" = "X4")
    #'
    #' Create slug
    finishSimTime         <- Sys.time()
    Time.Diff             <- round(as.numeric(finishSimTime-startSimTime, units = "mins"),0)
    report_dir            <- file.path(k_path_project,"results")
    primary_cost_function <- param[s,"primary_cost_function"]
    cost_function_type    <- ifelse(secondary_cost_function_flag, 
                                    primary_cost_function %+% 2 %+% secondary_cost_function,
                                    primary_cost_function)
    slug                  <- paste0('(',tolower(DATABASE_NAME),')',
                                    '(',toupper(model_inducer),')',
                                    '(',tolower(cost_function_type),')',
                                    '(',tolower(payment_selection_criteria),max_instances_in_history,')',
                                    '(',Sys.Date(),')',
                                    '(',paste0(Time.Diff,' minutes'),')')
    #'
    ##########
    # Return #
    ##########
    return(slug)
}


file_names <- list.files(pattern = "[.]csv$",
                         path = k_path_ledgers,
                         full.names = FALSE)
