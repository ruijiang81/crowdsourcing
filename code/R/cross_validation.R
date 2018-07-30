#' cross_validation
#'
#' @description
#' What does it do?
#' (1)	Splits the dataset into num_folds folds (sample with replacement).
#' (2)	Fits a classification model with inducer as base-learner to num_folds-1 folds.
#' (3)	Calculates the AUC on the remaining fold.
#' (4)	Does the same as in step 2-3 but with a different fold put aside.
#' (5)	Repeats steps 1-4 num_reruns times.
#' (6)	Returns a scalar - the mean AUC across all num_folds* num_reruns runs.
#'
#' INPUTS:
#' @param cv_data; the data to evaluate
#' @param num_folds; the number of folds in the K-fold CV
#' @param num_reruns; the number of repetitions for the K-fold CV
#' @param inducer; the base-learner for modeling the data (options: RF,GLM,J48)
#'
cross_validation <- function(cv_data,
                             # Number of folds
                             num_folds,
                             # Number repetition
                             num_reruns,
                             inducer = c("RF", "GLM", "J48")) {
    # Validate assumption
    require("foreach")
    if (!exists("global_seed")) {
        global_seed <- 1992
    }


    ## Returns the Average AUC for cross validation on a data set.
    ## Calls on function predict_set to calculate the AUC for each fold.
    sum_avg_AUC_all_CV_runs <- 0 # this is later divided by the number of reruns

    ## Assign repeated folds for CV
    for (j in 1:num_reruns) {
        set.seed(global_seed + j - 1)
        fold_values <- sample(1:num_folds, nrow(cv_data), replace = TRUE) # sample values from 1 to k, for each row in the data
        if (j == 1) {
            fold.df <- data.frame(col1 = fold_values)
        } else {
            fold.df <- cbind(fold.df, fold_values)
        }
    }

    # print (fold.df)
    colnames(fold.df) <- paste0("col", 1:num_reruns)
    #         col1 col2
    # [1,]    8    1
    # [2,]    7    6
    # [3,]    6    2
    # [4,]    1    7
    # [5,]    8    7
    # [6,]    8    4

    ## Fit models via repeated K-fold CV
    AUC.CV <- foreach(
        i = 1:(num_folds * num_reruns),
        .export = c("predict_set", "setVariablesNames"),
        .combine = rbind,
        .options.multicore = list(preschedule = TRUE),
        .errorhandling = "stop", #' remove',
        .packages = "foreach"
    ) %dopar% { # dopar
        ## Convert index to subscript
        fold <- ((i - 1) %% num_folds) + 1
        rep <- floor((i - 1) / num_folds) + 1
        ## Split to train and test sets
        train_data <- cv_data[!(fold.df[, rep] %in% fold), ]
        test_data <- cv_data[(fold.df[, rep] %in% fold), ]
        ## Fit & Evaluate model
        AUC.mdl <- predict_set(train_data,
            test_data,
            inducer = inducer,
            verbose = FALSE
        )

        return(data.frame(Repetition = rep, Fold = fold, AUC = AUC.mdl))
    } # end foreach fit model via repeated K-fold CV
    #       Repetition Fold AUC
    # 1     1          1    0.9907025
    # 2     1          2    0.9584847
    # 3     1          3    0.9599690
    # ...
    # 14    2          6    0.9642094
    # 15    2          7    0.9673203
    # 16    2          8    0.9671958

    ## Aggregate by repetition
    # print(AUC.CV)
    AUC.rep <- aggregate(AUC ~ Repetition, data = AUC.CV, function(x) x <- mean(x, na.rm = TRUE))

    # print (AUC.rep)
    # Repetition    AUC
    # 1             0.9673166
    # 2             0.9679297

    ## Mean repetitions
    AUC.mean <- mean(AUC.rep$AUC)

    return(AUC.mean)
} # end cross_validation
