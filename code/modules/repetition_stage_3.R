################################################################################
# Repetition Stage 3: Purchase initial batches and fit model on them           #
################################################################################
repetition_stage_3 <- function() {
    ####################
    # Input validation #
    ####################
    #' Check if argumenta are in the global environment
    assertive::assert_all_are_existing(envir = globalenv(), c(
        "current_batch",
        "k_batch_size",
        "payment_selection_criteria",
        "secondary_cost_function_flag",
        "current_report_line",
        "rep_metadata",
        "rep_report",
        "unlabeled_data",
        "holdout_data",
        "current_repetition",
        "rep_ledger"
    ))
    assertive::assert_is_data.frame(rep_ledger)
    #'
    ################
    # Stage Kernel #
    ################
    #' purchasing intial training set using different prices
    # Cost to pay for each intial batch
    cost_so_far <- 0
    current_instance_num <- 1
    if (num_batches_per_cost_initial_training_set > 0) {
        #' num_batches_per_cost_initial_training_set may be set to zero when
        #' using random or other non algorthmic payment selection methods
        for (i in 1:num_price_per_label_values) {
            for (j in 1:num_batches_per_cost_initial_training_set) {
                #########
                # Start #
                #########
                #' Setup
                set.seed(current_instance_num * global_seed)
                pay_per_label <- sample(price_per_label_values, 1)
                #' Create new ledger record holder
                new_record <- data.frame(
                    repetition = current_repetition,
                    batch = current_batch,
                    batch_size = k_batch_size,
                    payment_selection_criteria = payment_selection_criteria,
                    payment_selected = pay_per_label
                )
                #' Check if the condition for changing cost function is applicable
                if (secondary_cost_function_flag & (cost_so_far > model_cost_for_changing_cost_function)) {
                    cost_function_type <- secondary_cost_function
                }
                #' Set the quality level for the given cost-function and payment
                #' configuration
                labeling_accuracy <- labelingCostQualityTradeoff(
                    method = cost_function_type,
                    costPerTask = pay_per_label,
                    fixProbability = fixProbability
                )
                #'
                ########################################################
                #' Change label quality (instance-wise implementation) #
                ########################################################
                for (k in 1:k_batch_size) {
                    ## Bind train-set and labeled-set
                    if (current_instance_num == 1) {
                        training_set <- unlabeled_data[1, ]
                    }
                    else {
                        training_set <- rbind(training_set, unlabeled_data[current_instance_num, ])
                    } # end binding train/labeled sets


                    ## Alternate true label (instance-wise operation)
                    set.seed(current_instance_num * global_seed)
                    random_number <- runif(1)
                    if (random_number > labeling_accuracy) {
                        training_set$y[current_instance_num] <- change_level_value(training_set$y[current_instance_num])
                        change <- 1
                    } else {
                        change <- 0
                    }
                    #'
                    cost_so_far <- cost_so_far + pay_per_label
                    new_entry <- data.frame(
                        "instance_num" = current_instance_num,
                        "pay" = pay_per_label,
                        "change" = change,
                        "cost_so_far" = cost_so_far,
                        "updated_label" = training_set$y[current_instance_num],
                        "batch" = current_batch,
                        "svm_bug" = NA
                    )
                    rep_metadata <- merge(rep_metadata, new_entry, all = TRUE)

                    current_instance_num <- current_instance_num + 1 # updating the instance counter
                } # end for change instance quality
                #'
                #################################
                # Evaluate model on unseen data #
                #################################
                ## AUC
                calculated_AUC <- predict_set(training_set,
                    holdout_data,
                    inducer = model_inducer
                )

                #' Print out to report
                rep_metadata[current_instance_num - 1, "AUC_holdout"] <- calculated_AUC
                new_item <- rep_metadata[current_instance_num - 1, ]
                new_item$repetition <- current_repetition
                new_item$batch <- current_batch
                rep_report <- rbind(rep_report, new_item)
                current_report_line <- current_report_line + 1
                #'
                #######
                # End #
                #######
                rep_ledger <- rep_ledger %>% bind_rows(new_record)
                #' Advance counter
                current_batch <- current_batch + 1
                #'
            } # end for j (batch purchase)
        } # end for i
        cat("\n", "Finished purchasing initial training set")
        cat("\n", "AUC =", calculated_AUC)
        cat("\n---")
    } # end Purchase initial batches
    #'
    ##########
    # Return #
    ##########
    rep_report <<- rep_report
    rep_metadata <<- rep_metadata
    rep_ledger <<- rep_ledger

    cost_so_far <<- cost_so_far
    current_batch <<- current_batch
    current_instance_num <<- current_instance_num

    training_set <<- training_set

    return(invisible())
}
