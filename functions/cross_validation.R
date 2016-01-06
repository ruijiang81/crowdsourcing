#' cross_validation
#' 
#' 

cross_validation <- function(cv_data, 
                             num_folds,  # Number of folds
                             num_reruns, # Number repetition
                             inducer=c("RF","GLM","J48"))
{  
    # Validate assumption
    require("foreach")
    if(!exists("global_seed")) {global_seed <- 1992}
    
    
    ## Returns the Average AUC for cross validation on a data set. 
    ## Calls on function predict_set to calculate the AUC for each fold.
    sum_avg_AUC_all_CV_runs <- 0 #this is later divided by the number of reruns
    
    ## Assign repeated folds for CV
    #fold.df <- foreach(r = 1:num_reruns,.combine=cbind) %do% caret::createFolds(1:nrow(cv_data), k=num_folds,list = FALSE, returnTrain = FALSE)
    #
    for (j in 1:num_reruns){
        set.seed(global_seed+j-1)
        fold_values<-sample(1:num_folds, nrow(cv_data), replace = TRUE) # sample values from 1 to k, for each row in the data
        if (j==1) {fold.df<-fold_values}
        else {
            fold.df<-cbind(fold.df,fold_values)    
        }
    }
    
    #print (fold.df)
    colnames(fold.df) <- paste0("col",1:num_reruns)
    #         col1 col2
    # [1,]    8    1
    # [2,]    7    6
    # [3,]    6    2
    # [4,]    1    7
    # [5,]    8    7
    # [6,]    8    4
    
    ## Fit models via repeated K-fold CV
    AUC.CV <- foreach(
        i = 1:(num_folds*num_reruns),
        .export=c("predict_set","setVariablesNames"),
        .combine=rbind,
        .options.multicore=list(preschedule=TRUE),
        .errorhandling='stop',#'remove',
        .packages="foreach") %dopar% { #dopar
            ## Convert index to subscript
            fold <- ((i-1) %% num_folds) + 1
            rep  <- floor((i-1) / num_folds) + 1
            ## Split to train and test sets
            train_data <- cv_data[!(fold.df[,rep] %in% fold), ]
            test_data  <- cv_data[(fold.df[,rep] %in% fold), ]
            #train_data <- subset(cv_data,!(fold.df[,rep] %in% fold))
            #test_data <- subset(cv_data,(fold.df[,rep] %in% fold))
            
            ## Fit & Evaluate model
            AUC.mdl <- predict_set(train_data,
                                   test_data,
                                   inducer=inducer,
                                   verbose=FALSE)
            
            return(data.frame(Repetition=rep,Fold=fold,AUC=AUC.mdl))
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
    #print(AUC.CV)
    AUC.rep <- aggregate(AUC ~ Repetition, data=AUC.CV, function(x) x=mean(x,na.rm=TRUE))
    
    #print (AUC.rep)
    # Repetition    AUC
    # 1             0.9673166
    # 2             0.9679297
    
    ## Mean repetitions
    AUC.mean <- mean(AUC.rep$AUC)
    
    return(AUC.mean)
} # end cross_validation
