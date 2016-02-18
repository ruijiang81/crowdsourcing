#' top_n
#' 
#' 1. Rank the test-set estimated probabilities \hat{P}(Y=Minority Class) in 
#'    descending order. That is:
#'    * Rank = 1 is the instance which is most likely to be in the minority 
#'      class
#'    * Rank = n is the instance which is least likely to be in the minority
#'      class
#' 2. Evaluate the top 1%, 5%, 10% of the ranked \hat{P}(Y=Minority Class)
#' 3. Calculate recall + precision
#' 

top_n <- function(train_set,
                  test_set,
                  inducer=c("RF","GLM","J48","SVM"),
                  percentage_cutoffs=c(0.01, 0.05, 0.10), # 1%, 5%, 10%
                  measures=c('rec', 'prec'), # Recall, Precision
                  verbose=TRUE)
{
    ###############################
    # Standardize variables names #
    ###############################
    train_set = setVariablesNames(train_set)
    test_set  = setVariablesNames(test_set)
    
    
    #######################
    # Validate assumption #
    #######################
    # Global seed number
    if(!exists("global_seed")) global_seed <- 1992
    set.seed(global_seed)
    # Model inducer
    inducer = tolower(inducer[1])
    
    
    ##############
    # Parameters #
    ##############
    params = expand.grid(measures=measures,
                         percentage_cutoffs=percentage_cutoffs,
                         stringsAsFactors=FALSE)
    params$n_cutoffs = round(nrow(test_set)*params$percentage_cutoffs,0)
    
    
    ########################
    # Fit & Evaluate model #
    ########################
    evaluations <- tryCatch(
        { # recieves train and test set and returns AUC over the test set
            if(inducer=="rf"){          # Random Forest
                model = randomForest::randomForest(y ~ ., train_set, ntree=100)                                 
                y_hat = predict(model,test_set,type="prob") 
                y_hat = as.vector(y_hat[,2])
                
            } else if(inducer=="glm") { # Logistic Regression
                # Check which variables in the train-set have less then two levels, that is ZV 
                # (Zero Variance)
                zv_var = apply(train_set, 2, function(x) length(unique(x))<2)
                if(sum(zv_var)>0) {
                    if(verbose) cat('\n Removed', sum(zv_var), "independent variables with zero variance from the train-set")
                    train_set = train_set[,!zv_var]
                }
                model = glm(y ~ ., data=train_set, family = "binomial")
                y_hat = predict(model, test_set, type="response")
                y_hat = as.vector(y_hat)
                
            } else if(inducer=="j48") { # J48 tree
                ## Learn J4.8 tree with reduced error pruning (-R) and 
                ## minimum number of instances set to 5 (-M 5):
                #Weka_control = RWeka::Weka_control(R=TRUE, M=5)
                Weka_control = RWeka::Weka_control()
                model = RWeka::J48(y ~ ., train_set, control=Weka_control)
                y_hat = predict(model, test_set, type="probability")
                y_hat = as.vector(y_hat[,1])
                
            } else if(inducer=="svm") { # Support Vector Machines
                model = e1071::svm(y ~ ., data=train_set, scale=T, cost=1e0, probability=TRUE)
                y_hat = predict(model, test_set, probability=TRUE)
                y_hat = attr(y_hat, "probabilities")
                y_hat = as.vector(y_hat[,1])
                
            } else {
                warning("Unknown inducer in predict_set")
            }
            
            #######################
            ## Models evaluation ##
            #######################
            ranked_indices = order(y_hat,decreasing=TRUE)
            eval = data.frame()
            
            for(k in 1:nrow(params)){
                # Find the n_cutoff observation corresponding to the highest \hat{P}(Y = Minority Class)  
                predictions = y_hat[ranked_indices %in% 1:params[k,"n_cutoffs"]]
                labels = test_set[ranked_indices %in% 1:params[k,"n_cutoffs"],"y"]
                # Evaluate the top n performance
                prediction.obj = ROCR::prediction(predictions, labels)
                perf = ROCR::performance(prediction.obj,params[k,"measures"])
                x = perf@x.values[[1]] # cutoff values
                y = perf@y.values[[1]] # performance values
                # Find the (inherent) cutoff which yields the max F1 performance
                f1 = ROCR::performance(prediction.obj,"f")
                f1_cutoff = f1@x.values[[1]][which.max(f1@y.values[[1]])[1]]
                # Plot
                # plot(0,0,type="n", xlim=c(0,1), ylim=c(0,1), xlab="Cutoff", ylab="Performance")
                # lines(perf@x.values[[1]], perf@y.values[[1]], type="b", col="red")
                # abline(v=f1_cutoff, lty=2)
                
                # Stroe result
                eval[k,"measure"] = params[k,"measures"]
                eval[k,"top"]     = paste0(params[k,"percentage_cutoffs"]*100,'%')
                eval[k,"value"]   = y[x==max(x[x<=f1_cutoff])]
                
            } # end for params
        },
        error = function(cond){ # if the model fitting or the evaluation failed, return AUC = NA with a warning
            warning("predict_set could not fit/evalute a model, return NAs")
            eval = data.frame()
            for(k in 1:nrow(params)){
                eval[k,"measure"] = params[k,"measures"]
                eval[k,"top"]     = paste0(params[k,"percentage_cutoffs"]*100,'%')
                eval[k,"value"]   = NA
            }
            return(eval)
        } # end error
    ) # end tryCatch
    
    
    #############################################
    # Convert long data frame into a row vector #
    ############################################# 
    eval_vec = data.frame()
    for(k in 1:nrow(eval))
    {
        eval_vec[1,k] = eval[k,"value"]
        colnames(eval_vec)[k] = paste(eval[k,"measure"],eval[k,"top"],sep="_")
    }
    
    
    return(eval_vec)   
} # end predict_set
