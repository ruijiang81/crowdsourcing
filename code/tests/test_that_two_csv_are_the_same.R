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
    S2 <- file_2 %>% group_by(repetition) %>% summarise(batch = max(batch)) %>% as.data.frame()
    file_1_subset <- data.frame()
    for(i in 1:nrow(S2)){
        file_1_subset <- 
            bind_rows(
                file_1_subset,
                file_1 %>% dplyr::filter(repetition == S2[i,"repetition"], batch <= S2[i,"batch"])
            )
    }
    #'
    #########
    # Tests #
    #########
    file_1 <- file_1_subset
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
    return(invisible())
}
# ---------------------------------------------------------------------------- #
file_name_1 <- "A"
file_name_2 <- "B"

file_path_1 <- file.path(getwd(), "results", paste0(file_name_1, ".csv"))
file_path_2 <- file.path(getwd(), "results", paste0(file_name_2, ".csv"))

test_that_two_csv_files_are_identical(file_path_1, file_path_2)
