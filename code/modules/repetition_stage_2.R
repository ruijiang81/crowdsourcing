################################################################################
# Repetition Stage 2: Split the data to 'unlabeled' and 'holdout'              #
################################################################################
#' The holdout-set is fixed throughout the simulation
#' The unlabeled-set is shuffled differently in each repetition
repetition_stage_2 <- function() {
    ####################
    # Input validation #
    ####################
    #' Check if argumenta are in the global environment
    assertive::assert_all_are_existing(envir = globalenv(), c(
        "global_seed",
        "dataset",
        "p_holdout",
        "rep_ledger"
    ))
    assertive::assert_is_data.frame(rep_ledger)
    assertive::assert_is_empty(rep_ledger)
    #'
    ################
    # Stage Kernel #
    ################
    set.seed(global_seed)
    index_holdout <- sample(nrow(dataset), round(nrow(dataset) * p_holdout))
    holdout_data <- dataset[index_holdout, ]
    unlabeled_data <- dataset[-index_holdout, ]
    unlabeled_data <- unlabeled_data[sample(nrow(unlabeled_data)), ]
    max_size_training_data <- nrow(unlabeled_data) # used later for sanity check
    #'
    ##########
    # Return #
    ##########
    holdout_data <<- holdout_data
    unlabeled_data <<- unlabeled_data
    max_size_training_data <<- max_size_training_data
    return(invisible())
}
