ledger_feature_extractor <- function(ledger) {
    ####################
    # Input validation #
    ####################
    assertive::assert_is_data.frame(ledger)
    assertive::assert_is_subset(
        c(
            "repetition", "batch", "batch_size", "payment_selected", "safety_net",
            "AUC_partial_train_set_1", "AUC_train_set_delta_1",
            "expected_performance_1"
        ),
        colnames(ledger)
    )
    if (ledger %>% select(payment_selection_criteria, repetition, batch) %>% duplicated() %>% any()) {
        stop("Only one simulation ledger can be processed at a time")
    }
    #'
    #####################
    # Feature extractor #
    #####################
    ledger <- ledger %>% mutate(payment_selected = factor(payment_selected))
    #' Extract the predicted performance for the selected payment in the simulation
    partial_performance_matrix <-
        ledger %>%
        select(starts_with("AUC_partial_train_set_")) %>%
        as.matrix()

    delta_performance_matrix <-
        ledger %>%
        select(starts_with("AUC_train_set_delta_")) %>%
        as.matrix()

    expected_performance_matrix <-
        ledger %>%
        select(starts_with("expected_performance_")) %>%
        as.matrix()

    dummy_payment_matrix <- model.matrix(~payment_selected + 0, data = ledger)

    assertive::assert_have_same_dims(partial_performance_matrix, dummy_payment_matrix)
    assertive::assert_have_same_dims(expected_performance_matrix, dummy_payment_matrix)

    AUC_partial <- (partial_performance_matrix * dummy_payment_matrix) %>% rowSums()
    AUC_delta <- (delta_performance_matrix * dummy_payment_matrix) %>% rowSums()
    AUC_expected <- AUC_delta + ledger$AUC_full_train_set
    #'
    #' Shift expected AUC values in time (t) to (t+1) such that the values are no
    #' longer in the future. Instead the predicted values and the empirical values
    #' are inline.
    AUC_expected <- bind_cols(ledger %>% select(repetition), data.frame(AUC_expected))
    safety_net <- ledger %>% select(repetition, safety_net)
    for (rep in unique(AUC_expected$repetition)) {
        relevant_indices <- which(AUC_expected$repetition %in% rep)

        AUC_expected[relevant_indices, "AUC_expected"] <-
            data.table::shift(AUC_expected[relevant_indices, "AUC_expected"],
                n = 1, type = "lag"
            )

        safety_net[relevant_indices, "safety_net"] <-
            data.table::shift(safety_net[relevant_indices, "safety_net"],
                n = 1, type = "lag"
            )
    }
    AUC_expected <- AUC_expected %>% .$AUC_expected
    safety_net_flag <- safety_net %>% .$safety_net
    #'
    #' Add the expected performance to the ledger
    ledger <-
        ledger %>%
        mutate(
            AUC_partial_train_set = AUC_partial,
            AUC_delta = AUC_delta,
            AUC_expected = AUC_expected,
            safety_net = safety_net_flag
        ) %>%
        select(
            repetition, batch, batch_size, payment_selected,
            AUC_holdout_set, AUC_full_train_set, AUC_partial_train_set,
            AUC_delta, AUC_expected, safety_net
        )
    #'
    ##########
    # Return #
    ##########
    return(ledger)
}
