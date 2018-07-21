#' Utilities functions
#'
#' 1. oneVsAll
#' 2. change_level_value
#' 3. setVariablesNames
#' 4. subset_params


################################################################################
#' oneVsAll
#'
#' Used for the auto dataset
#' Assign one class to be positive while the rest are assigned to the negative
#' class
#' (assumes that the target variable is in the last column)
#'
#' calling example: dataset <- oneVsAll(dataset, positive.class=6)
oneVsAll <- function(X, positive.class) {
    if (class(X[, ncol(X)]) != "factor") error("the dependent variable is not a factor")
    # Get class names
    labels <- unique(levels(X[, ncol(X)]))
    levels(X[, ncol(X)])[levels(X[, ncol(X)]) %in% labels[-positive.class]] <- "level1"
    levels(X[, ncol(X)])[levels(X[, ncol(X)]) %in% labels[positive.class]] <- "level2"
    return(X)
} # end oneVsAll

################################################################################
#' change_level_value
#'
change_level_value <- function(level_value) {
    # changes the level value from level1 to level2 and vice versa
    if (level_value == "level1") {
        level_value <- "level2"
    } else if (level_value == "level2") {
        level_value <- "level1"
    }
    return(level_value)
} # end change_level_value

################################################################################
#' setVariablesNames
#'
#' Set the names of the variable to {'X1','X2',...,'y'} .
#' In addition set the level of y to {'level1','level2',...}
#'
#' @param fulldataset The full dataset where the last variable is dependent variable
setVariablesNames <- function(fulldataset) {
    Ncols <- ncol(fulldataset)
    ## 1. Set the dependent variable factor name
    fulldataset[, Ncols] <- as.factor(fulldataset[, Ncols])
    ## 2. Set the dependent variable name
    colnames(fulldataset)[Ncols] <- "y"
    ## 3. Set the dependent variable levels names
    levels(fulldataset$y) <- paste0("level", 1:nlevels(fulldataset$y))
    ## 4. Set the independent variables names
    colnames(fulldataset)[-Ncols] <- paste0("X", 1:(Ncols - 1))
    return(fulldataset)
} # end setVariablesNames


################################################################################
#' subset_params
subset_params <- function(data, colnames, values) {
    colnames <- unlist(colnames)
    values <- unlist(values)

    for (p in 1:length(colnames))
        data <- data[data[, colnames[p]] %in% values[p], ]

    return(data)
}
