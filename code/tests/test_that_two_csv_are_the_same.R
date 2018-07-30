################################################################################
#                     Test that two csv files are the same                     #
################################################################################
test_that_two_csv_files_are_identical <- function(file_path_1, file_path_2) {
    stopifnot(
        require(checkmate),
        require(assertthat),
        require(dplyr),
        require(magrittr),
        require(assertive),
        exists("cat_40")
    )
    #'
    cat_80("Test that two CSV files are identical")
    #'
    ####################
    # Input validation #
    ####################
    collection <- checkmate::makeAssertCollection()
    assert_file_exists(file_path_1, extension = "csv", add = collection)
    assert_file_exists(file_path_2, extension = "csv", add = collection)
    reportAssertions(collection)
    #'
    ##############
    # Load files #
    ##############
    file_1 <- read.csv(file_path_1)
    file_2 <- read.csv(file_path_2)
    #'
    ####################
    # Subset the files #
    ####################
    file_temp <- data.frame()
    for (r in unique(file_2$repetition)) {
        S <- file_2 %>%
            filter(repetition == r) %>%
            summarise(batch = max(batch), repetition = max(repetition))
        file_temp <-
            bind_rows(
                file_temp,
                file_1 %>% filter(repetition == S$repetition, batch <= S$batch)
            )
    }
    file_1 <- file_temp
    #'
    #########
    # Tests #
    #########
    #' test that data frame is not empty
    assertive::assert_is_non_empty(file_1)
    #' test that both files have the same dimensions
    assertive::assert_have_same_dims(file_1, file_2, severity = "stop")
    #' test that columns are identical
    assertive::assert_are_set_equal(colnames(file_1), colnames(file_2), severity = "stop")
    #' test that columns content is identical
    for (rep in unique(file_1$repetition)) {
        cat_40("Testing Repetition" %+% " " %+% rep)
        # Subset data by repetition
        R1 <- file_1 %>% filter(repetition == rep)
        R2 <- file_2 %>% filter(repetition == rep)
        assertive::assert_have_same_dims(file_1, file_2, severity = "message")
        #'
        for (col_name in colnames(file_1)) {
            cat("\n# ->", col_name, "")
            # Subset data by column name
            E1 <- R1 %>% .[[col_name]]
            E2 <- R2 %>% .[[col_name]]
            # Perform calculations on numeric variables
            if (is.numeric(E1)) {
                # Set numerical accuracy
                E1 %<>% round(5)
                E2 %<>% round(5)
                # Quantify errors
                square_error <- (E1 - E2)^2
                cat("\t| SSE =", sum(square_error, na.rm = TRUE))
                cat("\t| MSE =", mean(square_error, na.rm = TRUE))
                cat("\t| ")
            }
            # Check equal values
            assertive::assert_are_set_equal(E1, E2, severity = "message")
            assertive::assert_are_set_equal(E1, E2, severity = "warning")
        } # end for col_names
    } # end for repetitions
    #'
    ##########
    # Return #
    ##########
    cat_80("Test Completed")
    cat("\n# Compared two tables of", nrow(file_1), "rows and", ncol(file_1), "columns")
    cat("\n")
    #'
    return(invisible())
}
# ---------------------------------------------------------------------------- #
file_name_1 <- "A"
file_name_2 <- "B"

file_path_1 <- file.path(getwd(), "results", paste0(file_name_1, ".csv"))
file_path_2 <- file.path(getwd(), "results", paste0(file_name_2, ".csv"))

source(file.path(getwd(), "code", "scripts", "setup.R"))
test_that_two_csv_files_are_identical(file_path_1, file_path_2)
