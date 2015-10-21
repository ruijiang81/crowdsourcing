#' predict_set
#' 
#' 

predict_set = function(train_set,
                       test_set,
                       inducer=c("RF","GLM","J48"))
{
    # Standardize variables names
    train_set = setVariablesNames(train_set)
    test_set  = setVariablesNames(test_set)
    
    
    # Validate assumption
    ## Global seed number
    if(!exists("global_seed")) global_seed <- 1992
    set.seed(global_seed)
    ## Model inducer
    inducer = tolower(inducer[1])
    
    
    # Fit & Evaluate model
    AUC <- tryCatch(
        { # recieves train and test set and returns AUC over the test set
            if(inducer=="rf"){          # Random Forest
                model = randomForest::randomForest(y ~ ., train_set, ntree=100)                                 
                y_hat = predict(model,test_set,type="prob") 
                predictions = as.vector(y_hat[,2])
                
            } else if(inducer=="glm") { # Logistic Regression
                model = glm(y ~ ., data=train_set, family = "binomial")
                y_hat = predict(model, test_set, type="response")
                predictions = as.vector(y_hat)
                
            } else if(inducer=="j48") { # J48 tree
                ## Learn J4.8 tree with reduced error pruning (-R) and 
                ## minimum number of instances set to 5 (-M 5):
                #Weka_control = RWeka::Weka_control(R=TRUE, M=5)
                Weka_control = Weka_control()
                model = RWeka::J48(y ~ ., train_set, control=Weka_control)
                y_hat = predict(model, test_set, type="probability")
                predictions = as.vector(y_hat[,2])
                
            } else {
                warning("Unknown inducer in predict_set")
            }
            
            pred     = ROCR::prediction(predictions,test_set$y)
            perf_AUC = ROCR::performance(pred,"auc") #Calculate the AUC value
            return(perf_AUC@y.values[[1]])
        },
        error = function(cond){ # if the model fitting or the evaluation failed, return AUC = 0.5 with a warning
            warning("predict_set could not fit/evalute a model, return AUC=NA")
            return(NA)
        } # end error
    ) # end tryCatch
    
    
    return(AUC)   
} # end predict_set
