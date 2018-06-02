################################################################################
# Repetition Stage 1: Setup                                                    #
################################################################################
repetition_stage_1 <- function(){
    ####################
    # Input validation #
    ####################
    checkmate::assert(
        checkEnvironment(globalenv(), c("start.time",
                                        "watchdog_simulation",
                                        "initial_seed",
                                        "counter_repetitions"))
        )
    #'
    ################
    # Stage Kernel #
    ################
    if (Sys.time()-start.time >= watchdog_simulation)
        break # watchdog stop execution
    global_seed <<- initial_seed * counter_repetitions
    rep_metadata <<- create_report()
    rep_report <<- create_report()
    counter_batches <<- 1
    current_report_line <<- 1
    #' Display repetition info
    cat('\n', rep('#',40), 
        '\n', "current repeatition: ", counter_repetitions,
        '\n', rep('#',40),
        sep="")
    #'
    ##########
    # Return #
    ##########
    return(invisible())
}
