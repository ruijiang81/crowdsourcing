################################################################################
# Repetition Stage 1: Setup                                                    #
################################################################################
repetition_stage_1 <- function(){
    ####################
    # Input validation #
    ####################
    checkmate::assert(
        checkEnvironment(globalenv(), c("dataset",
                                        "start.time",
                                        "watchdog_simulation",
                                        "initial_seed",
                                        "current_repetition"))
    )
    #'
    ################
    # Stage Kernel #
    ################
    if (Sys.time()-start.time >= watchdog_simulation)
        break # watchdog stop execution
    global_seed <<- initial_seed * current_repetition
    rep_metadata <<- create_report()
    rep_report <<- create_report()
    rep_ledger <<- data.frame()
    current_batch <<- 1
    current_report_line <<- 1
    #' Display repetition info
    cat_40("Current repeatition: " %+% current_repetition)
    #'
    ##########
    # Return #
    ##########
    return(invisible())
}
