#' predict_set
#' 
#' 

predict_set <- function(train_set,test_set)
{
    # Validate assumption
    if(!exists("global_seed")) global_seed <- 1992
    set.seed(global_seed)
    
    # Fit & Evaluate model
    AUC <- tryCatch(
        { # recieves train and test set and returns AUC over the test set
            model       <- randomForest::randomForest(y ~ ., train_set, ntree=100)                                 
            set_output  <- predict(model,test_set,type="prob")  
            predictions <- as.vector(set_output[,2])
            pred        <- ROCR::prediction(predictions,test_set$y)
            perf_AUC    <- ROCR::performance(pred,"auc") #Calculate the AUC value
            return(perf_AUC@y.values[[1]])
        },
        error = function(cond){ # if the model fitting or the evaluation failed, return AUC = 0.5 with a warning
            warning("predict_set could not fit/evalute a model, return AUC=NA")
            return(NA)
        } # end error
    ) # end tryCatch
    
    
    return(AUC)   
} # end predict_set
