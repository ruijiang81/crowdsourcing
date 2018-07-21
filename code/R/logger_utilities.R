#'
#' 1. logger_create
#' 2. logger_append
#' 3. logger_as_data_frame
#'
logger_create <- function() {
    logger <<- list()
    return(invisible())
}
#'
logger_append <- function(values) {
    assertive::assert_all_are_existing("logger")
    assertive::assert_is_list(logger)
    assertive::assert_is_list(values)
    logger <<- logger %>% append(values)
    return(invisible())
}
#'
logger_as_data_frame <- function() {
    assertive::assert_all_are_existing("logger")
    assertive::assert_is_list(logger)
    new_logger <- data.frame()
    if (length(logger) > 0) {
        for (l in 1:length(logger)) {
            if (length(logger[[l]]) == 1) { # A single value
                col_name <- names(logger[l])
                new_logger[1, col_name] <- logger[[l]]
            } else { # Multi value
                for (k in 1:length(logger[[l]])) {
                    col_name <- names(logger[l]) %+% "_" %+% k
                    new_logger[1, col_name] <- logger[[l]][k]
                }
            }
        }
    }
    logger <<- new_logger
    return(invisible())
}
