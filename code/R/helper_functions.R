################################################################################
#                               Helper Functions                               #
################################################################################
#' Paste Utilities
"%+%" <- function(a, b) paste0(a, b)
"%()%" <- function(a, b) a %+% "(" %+% b %+% ")"
#' Examples
#' "" %()% Sys.Date()                     --> "(2018-06-04)"
#' "" %()% Sys.Date() %()% Sys.timezone() --> "(2018-06-04)(Pacific/Auckland)"
#'
# ---------------------------------------------------------------------------- #
# Concatenate and Print (Cat) Utilities
#'
cat_title <- function(string) {
    ############################################
    # Create titles encapsulated with hashtags #
    ############################################
    assertive::assert_is_character(string)
    assertive::assert_is_scalar(string)

    n <- nchar(string)

    new_string <-
        paste0(rep("#", 2 + n + 2), collapse = "") %+%
        "\n# " %+% string %+% " #\n" %+%
        paste0(rep("#", 2 + n + 2), collapse = "")

    cat("\n" %+% new_string, sep = "")
    return(invisible())
}
#'
cat_40 <- function(string) {
    ##################################################
    # Create titles in encapsulated with 40 hashtags #
    ##################################################
    assertive::assert_is_character(string)
    assertive::assert_is_scalar(string)

    n <- nchar(string)
    padding_left <- 20 - 2 - ceiling((n / 2))
    padding_right <- 20 - 2 - ceiling((n / 2))
    if (assertive::is_non_negative(padding_left) & assertive::is_non_negative(padding_right)) {
        string <-
            paste0(rep(" ", padding_left), collapse = "") %+%
            string %+%
            paste0(rep(" ", padding_right), collapse = "")
    }
    cat_title(string)
    # cat("padding left", padding_left,
    #     "| padding right", padding_right,
    #     "| total", nchar(string))
    return(invisible())
}
cat_80 <- function(string) {
    ##################################################
    # Create titles in encapsulated with 80 hashtags #
    ##################################################
    assertive::assert_is_character(string)
    assertive::assert_is_scalar(string)

    n <- nchar(string)
    padding_left <- 40 - 2 - ceiling((n / 2))
    padding_right <- 40 - 2 - ceiling((n / 2))
    if (assertive::is_non_negative(padding_left) & assertive::is_non_negative(padding_right)) {
        string <-
            paste0(rep(" ", padding_left), collapse = "") %+%
            string %+%
            paste0(rep(" ", padding_right), collapse = "")
    }
    cat_title(string)
    # cat("padding left", padding_left,
    #     "| padding right", padding_right,
    #     "| total", nchar(string))
    return(invisible())
}
# ---------------------------------------------------------------------------- #
