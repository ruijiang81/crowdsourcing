################################################################################
#                     Test that two csv files are the same                     #
################################################################################
test_that_two_csv_files_are_identical <- function(file_path_1, file_path_2){
    stopifnot(require(checkmate),
              require(assertthat),
              require(dplyr),
              require(magrittr))
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
    for(r in unique(file_2$repetition)){
        S <- file_2 %>% filter(repetition == r) %>% 
            summarise(batch = max(batch), repetition = max(repetition))
        file_temp <- 
            bind_rows(
                file_temp,
                file_1 %>% filter(repetition == S$repetition, batch <= S$batch)
            )
    }
    file_1 <- file_temp
    assertive::assert_is_non_empty(file_1)
    #'
    #########
    # Tests #
    #########
    #' test that columns are identical
    assert_set_equal(colnames(file_1), colnames(file_2), add = collection)
    reportAssertions(collection)
    #' test that columns content is identical  
    for(col_name in colnames(file_1)){
        if(is.numeric(file_1[,col_name])){
            file_1[,col_name] %<>% round(5)
            file_2[,col_name] %<>% round(5)
        }
        assert_set_equal(file_1[,col_name], file_2[,col_name],
                         .var.name = col_name, add = collection)
    }
    reportAssertions(collection)
    #'
    ##########
    # Return #
    ##########
    message("###############################\n# Test Completed Successfully #\n###############################")
    message("# Compared two tables of ",nrow(file_1), " rows and ", ncol(file_1), " columns")
    
    return(invisible())
}
# ---------------------------------------------------------------------------- #
file_name_1 <- "MTR_A"
file_name_2 <- "MTR_B"

# file_name_1 <- "MR_A"
# file_name_2 <- "MR_B"

file_path_1 <- file.path(getwd(), "results", paste0(file_name_1, ".csv"))
file_path_2 <- file.path(getwd(), "results", paste0(file_name_2, ".csv"))

test_that_two_csv_files_are_identical(file_path_1, file_path_2)
