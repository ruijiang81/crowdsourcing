#' predict_set
#'
#' Given an inducer, the function fits a model to the train_set and evaluate the
#' AUC using the test_set.
#'

predict_set <- function(train_set,
                        test_set,
                        inducer = c("RF", "GLM", "J48", "SVM", "BAG"),
                        verbose = TRUE) {
    ####################
    # Input Validation #
    ####################
    #' Train set
    if (missing(train_set)) stop("No train_set is supplied")
    assertive::assert_is_data.frame(train_set)
    assertive::assert_is_non_empty(train_set)
    #' Test set
    if (missing(test_set)) stop("No test_set is supplied")
    assertive::assert_is_data.frame(test_set)
    assertive::assert_is_non_empty(test_set)
    #'
    #########
    # Setup #
    #########
    # Global seed number
    if (!exists("global_seed")) global_seed <- 1992
    set.seed(global_seed)
    # Model inducer
    inducer <- tolower(inducer[1])
    #'
    ######################
    # Data Preprecessing #
    ######################
    # 1. Remove variables with no variance (in the train set)
    if (inducer != "rf") {
        for (i in (ncol(train_set) - 1):1) {
            if (is.factor(train_set[, i])) { ## Is it a factor variable?
                if ((sum(table(train_set[, i]) > 0)) < 2) { ## Does it have less than 2 unique values?
                    train_set <- train_set[, -i] ## Drop the variable form the training set
                    test_set <- test_set[, -i] ## Drop the variable form the test set
                }
            } else if (is.integer(train_set[, i]) |
                is.numeric(train_set[, i])) { ## Is it a numeric/integer variable?
                if (length(unique(train_set[, i])) < 2) { ## Does it have less than 2 unique values?
                    train_set <- train_set[, -i] ## Drop the variable form the training set
                    test_set <- test_set[, -i] ## Drop the variable form the test set
                }
            }
        } # end removing zero varince variables
    }

    # 2. Standardize variables names
    train_set <- setVariablesNames(train_set)
    test_set <- setVariablesNames(test_set)


    ########################
    # Fit & Evaluate model #
    ########################
    AUC <- tryCatch({ # recieves train and test set and returns AUC over the test set
        if (inducer == "rf") { # Random Forest
            model <- randomForest::randomForest(y ~ ., train_set, ntree = 100)
            y_hat <- predict(model, test_set, type = "prob")
            predictions <- as.vector(y_hat[, 2])
        } else if (inducer == "glm") { # Logistic Regression
            # Check which variables in the train-set have less then two levels, that is ZV
            # (Zero Variance)
            zv_var <- apply(train_set, 2, function(x) length(unique(x)) < 2)
            if (sum(zv_var) > 0) {
                if (verbose) cat("\n Removed", sum(zv_var), "independent variables with zero variance from the train-set")
                train_set <- train_set[, !zv_var]
            }
            model <- glm(y ~ ., data = train_set, family = "binomial")
            y_hat <- predict(model, test_set, type = "response")
            predictions <- as.vector(y_hat)
        } else if (inducer == "j48") { # J48 tree
            ## Learn J4.8 tree with reduced error pruning (-R) and
            ## minimum number of instances set to 5 (-M 5):
            # Weka_control = RWeka::Weka_control(R=TRUE, M=5)
            Weka_control <- RWeka::Weka_control()
            model <- RWeka::J48(y ~ ., train_set, control = Weka_control)
            y_hat <- predict(model, test_set, type = "probability")
            predictions <- as.vector(y_hat[, 1])
        } else if (inducer == "svm") { # Support Vector Machines
            model <- e1071::svm(y ~ ., data = train_set, scale = T, cost = 1e0, probability = TRUE)
            y_hat <- predict(model, test_set, probability = TRUE)
            y_hat <- attr(y_hat, "probabilities")
            predictions <- as.vector(y_hat[, "level2"])
        } else if (inducer == "bag") { # Bagging
            model <- ipred::bagging(y ~ ., data = train_set)
            y_hat <- predict(model, test_set, type = "prob")
            predictions <- as.vector(y_hat[, "level2"])
        } else {
            warning("Unknown inducer in predict_set")
        }

        pred <- ROCR::prediction(predictions, test_set$y)
        perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
        perf_AUC <- perf_AUC@y.values[[1]]
        # max(perf_AUC,1-perf_AUC)
    },
    error = function(cond) { # if the model fitting or the evaluation failed, return AUC = NA with a warning
        warning("predict_set could not fit/evalute a model, return AUC=NA")
        return(NA)
    } # end error
    ) # end tryCatch


    return(AUC)
} # end predict_set
