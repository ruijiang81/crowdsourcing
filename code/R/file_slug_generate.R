file_slug_generate <- function() {
    #' Input validation
    assertive::assert_all_are_existing(
        envir = globalenv(),
        c(
            "startSimTime",
            "k_path_project",
            "param",
            "secondary_cost_function_flag",
            "primary_cost_function",
            "DATABASE_NAME",
            "model_inducer",
            "cost_function_type",
            "payment_selection_criteria",
            "max_instances_in_history",
            "s"
        )
    )
    assertive::assert_is_non_empty(param[s, ])
    #'
    #' Create slug
    finishSimTime <- Sys.time()
    Time.Diff <- round(as.numeric(finishSimTime - startSimTime, units = "mins"), 0)
    report_dir <- file.path(k_path_project, "results")
    primary_cost_function <- param[s, "primary_cost_function"]
    cost_function_type <- ifelse(secondary_cost_function_flag,
        primary_cost_function %+% 2 %+% secondary_cost_function,
        primary_cost_function
    )
    slug <- paste0(
        "(", tolower(DATABASE_NAME), ")",
        "(", toupper(model_inducer), ")",
        "(", tolower(cost_function_type), ")",
        "(", tolower(payment_selection_criteria), max_instances_in_history, ")",
        "(", Sys.Date(), ")",
        "(", paste0(Time.Diff, " minutes"), ")"
    )
    #'
    ##########
    # Return #
    ##########
    return(slug)
}
