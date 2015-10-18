## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$", path="./functions/", full.names=TRUE), source)


##########################################################functions####################################################
decide_price_per_label<-function(train,pay_criteria,payment_options,cur_instance_num,meta_data,repeatition_num){
    if (pay_criteria =="random"){
        set.seed(cur_instance_num*global_seed)
        pay<-sample(payment_options,1)
    } 
    else if (pay_criteria =="min_pay_per_label"){
        pay<-payment_options[which.min(payment_options)]
    } 
    else if (pay_criteria =="max_pay_per_label"){
        pay<-payment_options[which.max(payment_options)]
    } 
    else if ((pay_criteria =="max_quality")|(pay_criteria =="max_ratio")|(pay_criteria =="max_total_ratio")|(pay_criteria=="delta_AUC_div_total_cost")){
        # for testing  
        #payment_options<-price_per_label_values
        #train<-training_set
        #meta_data<-metadata
        #cur_instance_num<-250
        #
        columns_names<- c(payment_options)
        summary_partial_model_performance <- read.table(text = "", col.names = columns_names)
        
        num_payment_options<-length(payment_options) 
        full_model_CV_performance<-cross_validation(train,cross_validation_folds,cross_validation_reruns)
        full_model_CV_performance
        for (i in 1:num_payment_options) {
            #print (i)
            for (j in 1:number_batch_omissions){
                #print (j)
                payment_row_numbers<-which(meta_data$pay == payment_options[i]) #row number with this payments 
                set.seed(cur_instance_num*global_seed+i+j*1000)
                random_rows_to_remove_with_payment <- sample(payment_row_numbers, batch_size) 
                randomly_remaining_instances <- train[-random_rows_to_remove_with_payment, ]; 
                summary_partial_model_performance[j,i]<-cross_validation(randomly_remaining_instances,cross_validation_folds,cross_validation_reruns)
            }
        }  
        partial_model_performance<-colMeans(summary_partial_model_performance, na.rm = FALSE, dims = 1) #vector with the average performance of the partial models per payment option
        delta_performance_improvement<-full_model_CV_performance-partial_model_performance #vector with average delta improvement over a the full model    
        
        out<-toString(c(cur_instance_num,delta_performance_improvement))
        
        file_path<-paste0(directory,"/",repeatition_num,"delta_performance_improvements.txt")
        cat(out,file=file_path, sep="\n", append=TRUE) 
        
        if (pay_criteria =="max_quality"){ 
            pay<-payment_options[which.max(delta_performance_improvement)]
        } 
        
        if (pay_criteria =="max_ratio"){
            if (max(delta_performance_improvement)<0){
                # in this case all values are negative so falling back to the max quality rule
                print ("used_safety_net")
                pay<-payment_options[which.max(delta_performance_improvement)]
            } else {
                ratio_performance<-delta_performance_improvement/payment_options
                pay<-payment_options[which.max(ratio_performance)]
            }
        }
        
        if (pay_criteria =="max_total_ratio"){
            if (max(delta_performance_improvement)<0){
                # in this case all values are negative so falling back to the max quality rule
                print ("used_safety_net")
                pay<-payment_options[which.max(delta_performance_improvement)]
            } else {
                for (t in 1:num_payment_options){
                    if (delta_performance_improvement[t]<0)       #using a large negative number to eliminate out of consideration negative delta improvements (in case part of them are negative and part positive)
                    {delta_performance_improvement[t]=-100000000}
                }
                expected_performance<-delta_performance_improvement+full_model_CV_performance
                expected_total_cost<-meta_data$cost_so_far[cur_instance_num-1]+batch_size*payment_options
                total_ratio_performance<-expected_performance/expected_total_cost
                pay<-payment_options[which.max(total_ratio_performance)]
            }
        }
        
        if (pay_criteria =="delta_AUC_div_total_cost"){
            if (max(delta_performance_improvement)<0){
                # in this case all values are negative so falling back to the max quality rule
                print ("used_safety_net")
                pay<-payment_options[which.max(delta_performance_improvement)]
            } else {
                expected_total_cost<-meta_data$cost_so_far[cur_instance_num-1]+batch_size*payment_options
                ratio_delta_AUC_div_total_cost<-delta_performance_improvement/expected_total_cost
                
                pay<-payment_options[which.max(ratio_delta_AUC_div_total_cost)]
            }
        }
        
    }
    
    else {
        stop("WRONG CRITERIA in decide_price_per_label")  
    }
    
    return(pay)
}






change_level_value<-function(level_value){
    #changes the level value from level1 to level2 and vice versa
    if (level_value=="level1") {
        level_value<-"level2"
    } else if (level_value=="level2") {
        level_value<-"level1"
    }         
    return (level_value)     
}








#' setVariablesNames
#' Set the names of the variable to {'X1','X2',...,'y'} . 
#' In addition set the level of y to {'level1','level2',...}
#' 
#' @param fulldataset The full dataset where the last variable is dependent variable 
setVariablesNames <- function(fulldataset){
    Ncols <- ncol(fulldataset)
    ## 1. Set the dependent variable factor name
    fulldataset[,Ncols]  <- as.factor(fulldataset[,Ncols])
    ## 2. Set the dependent variable name
    colnames(fulldataset)[Ncols] <- 'y'
    ## 3. Set the dependent variable levels names
    levels(fulldataset$y) <- paste0('level',1:nlevels(fulldataset$y))
    ## 4. Set the independent variables names
    colnames(fulldataset)[-Ncols]  <- paste0('X',1:(Ncols-1))
    return(fulldataset)
}


labelingCostQualityTradeoff <- function(method=c('Fix','Concave','Asymptotic'),costPerLabel=NULL, fixProbability=NULL) {
    #'Returns the probability for a correct label given payment per label and tradeoff function type ('Fix','Concave','Asymptotic')
    #'
    # Check for null values. If TRUE then set the default
    if (is.null(method))                 method <- 'Fix'
    if (is.null(costPerLabel))       costPerLabel <- 0.1
    if (is.null(fixProbability)) fixProbability <- 0.75        
    # Check arguments validity
    ## Cost
    C = costPerLabel
    C_Range = c(0.02,0.25)
    if (!is.numeric(C) || C<C_Range[1] || C>C_Range[2]) {
        stop('Unvalid cost per label')
    }
    ## Probability
    if (!is.numeric(fixProbability) || fixProbability<=0 || fixProbability>1){
        stop('Probability sholud be in the range (0,1]')
    }
    
    if (method[1]=='Fix') {
        p = rep(fixProbability,length(costPerLabel))               
    } else if (method[1]=='Concave') {
        #                 p =-0.002*(C*100)^2+0.066*(C*100)+0.37
        p = 0.48+0.066*(100*C)-0.0022*(100*C)^2
        ## Make sure p \in [0,1], one p at a time
        p <- sapply(p,function(p) max(0,min(p,1)))
        #                 curve(coeff[1] + coeff[1]*x + coeff[2]*x^2, from=0.02, to=0.25)
        
    } else if (method[1]=='Asymptotic') {
        p=1-1/(C*100) 
    } else {
        stop('Unknown tradeoff method')
    }    
    return(p)   
}


#' oneVsAll
#' Used for the auto dataset
#' Assign one class to be positive while the rest are assigned to the negative 
#' class 
#' (assumes that the target variable is in the last column)
#' 
#' calling example: dataset <- oneVsAll(dataset, positive.class=6)
oneVsAll <- function(X, positive.class){
    if(class(X[,ncol(X)])!="factor") error("the dependent variable is not a factor")
    # Get class names
    labels <- unique(levels(X[,ncol(X)]))
    levels(X[,ncol(X)])[levels(X[,ncol(X)]) %in% labels[-positive.class]] <- "level1"
    levels(X[,ncol(X)])[levels(X[,ncol(X)]) %in% labels[positive.class]]  <- "level2"
    return(X)
}

###############################################

############### General parameters to set#######################
DATABASE_NAME="Otto" #IMPORTANT TO SET THIS NAME!!!
#DATABASE_NAME="SPAM" #IMPORTANT TO SET THIS NAME!!!
#DATABASE_NAME="OTTO" #IMPORTANT TO SET THIS NAME!!!
#basic_directory<-"./TEST_MODELING"

cores_not_to_use<-0 #0 means use all cores
p_holdout=0.3 #percentage of data in external holdout
initial_seed<-2015 #large number
batch_size<<-10
number_batch_omissions<<-10
num_batches_per_cost_initial_training_set=10 # 5  e.g., if the batch size is 10, num_price_per_label_values=5 and num_batches_per_cost_initial_training_set=5 then this will purchase 250 instances
#for random payment selection best to use 0
price_per_label_values= c(0.02,0.08,0.14,0.19,0.25)
max_total_cost<-80 #should be larger than the cost of paying for the initial training batches

#if reverting to max_number_of_training_instance instead of max_total_cost then activate this manually in the while loop
#max_number_of_training_instance<-1000 #should at least eqaul to  batch_size*num_batches_per_cost_initial_training_set*(num_price_per_label_values)

payment_selection_criteria<-"random" #"random", "max_quality", "max_ratio", "max_total_ratio","delta_AUC_div_total_cost", "min_pay_per_label", "max_pay_per_label"
cost_function_type<-"Concave" #"Fix","Concave"',"Asymptotic"

cross_validation_folds<<-8 #global10
cross_validation_reruns<<-4 #global5
repeatitions<-1 #10

#GENERATING A NEW DIRECTORY FOR THE RESULTS
directory <<- file.path(getwd(), DATABASE_NAME, payment_selection_criteria)
directory
dir.create(directory, showWarnings = FALSE, recursive = TRUE, mode = "0777")

#DISABLE IN CASE OF DOMINO!!!
#################################################################

######Get the data and create a standard_name dataset######
# #SPAM DATASET
# #NOTE remember in other datasets to remove ID if exists....
#  require("kernlab")  
#  data(spam)
#  dataset <- spam

#OTTO DATASET
### NOT USED setwd("C:\\Users\\user\\Dropbox\\economic labelling pers\\Otto")

dataset <- read.csv(file.path(getwd(),'data','Otto','train.csv'), colClasses=c("integer",rep("numeric",93),"factor"))
#dataset <- read.csv("Ottotrain.csv", colClasses=c("integer",rep("numeric",93),"factor"))
dataset <- dataset[,-1] #deletes the first (ID) column 
dataset <- oneVsAll(dataset, positive.class=3) #customized function that assigns one positive and one negative class

standard_name_dataset<-setVariablesNames(dataset)

###########################################################
cl <- makeCluster(detectCores()-cores_not_to_use,outfile="") #detects the number of cores and prepares for parallel run  
registerDoParallel(cl)
for  (counter_repeatitions in 1:repeatitions)
{
    global_seed<<-initial_seed*counter_repeatitions
    #global_seed<<-2015
    
    print ("current repeatition")
    print (counter_repeatitions) 
    
    
    
    ######Split the data to 'unlabeled' and 'holdout'######
    num_rows <- nrow(standard_name_dataset)
    set.seed(global_seed)
    prob_value <- runif(num_rows) #assigns a random number to a vector prob_value
    #Sorting the dataset according to the random probability value. this determines which data instances will be considered for unlabeled_data and holdout.
    #It will also determine,later on, the order of labeling the data      
    sorted_standard_name_dataset<- standard_name_dataset[order(prob_value),] 
    first_holdout_row<-floor(num_rows-num_rows*p_holdout)
    unlabeled_data <- sorted_standard_name_dataset[1:(first_holdout_row-1),]
    holdout_data <- sorted_standard_name_dataset[first_holdout_row:num_rows,]
    
    max_size_training_data<-nrow(unlabeled_data) #used later for sanity check
    ###########################################################
    
    
    #############Additional setup########################
    
    #defining the columns of the metadata table
    metadata_columns<- c("instance_num", "pay", "change", "cost_so_far") 
    metadata <- read.table(text = "", col.names = metadata_columns)
    
    report_columns<- c("instance_num","pay", "change", "cost_so_far", "AUC_holdout") 
    report <- read.table(text = "", col.names = report_columns)
    current_report_line=1
    start.time<-Sys.time()
    print (start.time)
    ###########################################################
    
    
    ########purchasing intial training set using different prices#######
    # Cost to pay for each intial batch 
    num_price_per_label_values<-length(price_per_label_values) 
    cost_so_far=0
    current_instance_num<-1
    
    if (num_batches_per_cost_initial_training_set>0){
        #num_batches_per_cost_initial_training_set may be set to zero when using random or other non algorthmic payment selection methods
        for (i in 1:num_price_per_label_values){
            for (j in 1:num_batches_per_cost_initial_training_set){
                for (k in 1:batch_size) {
                    if (current_instance_num==1){
                        training_set<-unlabeled_data[1,]
                    } 
                    else {
                        training_set<-rbind(training_set,unlabeled_data[current_instance_num,])
                    }
                    
                    labeling_accuracy<-labelingCostQualityTradeoff(cost_function_type,price_per_label_values[i])
                    set.seed(current_instance_num*global_seed)
                    random_number <- runif(1)
                    if (random_number>labeling_accuracy){
                        training_set$y[current_instance_num]<-change_level_value(training_set$y[current_instance_num])
                        change<-1
                    }
                    else {
                        change<-0
                    }
                    cost_so_far=cost_so_far+price_per_label_values[i]
                    metadata[current_instance_num,]<-c(current_instance_num,price_per_label_values[i],change,cost_so_far)
                    
                    current_instance_num<-current_instance_num+1 #updating the counter
                }
            }
        }
        
        calculated_AUC<-predict_set(training_set,holdout_data)
        print (calculated_AUC)
    }  
    print ("finished purchasing initial training set")
    
    
    #####################################################################
    
    ############################running the rest of the simulation############
    #while (current_instance_num<=max_number_of_training_instance) {
    while (cost_so_far<=max_total_cost) {
        print (current_instance_num)
        print (cost_so_far)
        
        if (current_instance_num+batch_size>max_size_training_data)
        {stop ("current_instance_num+batch_size>max_size_training_data")} #sanity check that the allocation of instances according to cost doesn't exceed the number of initially available unlabelled data
        
        pay_per_label<-decide_price_per_label(training_set,payment_selection_criteria,price_per_label_values,current_instance_num,metadata,counter_repeatitions)
        
        for (k in 1:batch_size) {
            
            if (current_instance_num==1){
                training_set<-unlabeled_data[1,]
            } 
            else {
                training_set<-rbind(training_set,unlabeled_data[current_instance_num,])
            }
            labeling_accuracy<-labelingCostQualityTradeoff(cost_function_type,pay_per_label)
            set.seed(current_instance_num*global_seed)
            random_number <- runif(1)
            if (random_number>labeling_accuracy){
                training_set$y[current_instance_num]<-change_level_value(training_set$y[current_instance_num])
                change<-1
            }
            else {
                change<-0
            }
            cost_so_far=cost_so_far+pay_per_label
            metadata[current_instance_num,]<-c(current_instance_num,pay_per_label,change,cost_so_far)
            
            current_instance_num<-current_instance_num+1 #updating the counter
        }
        
        calculated_AUC<-predict_set(training_set,holdout_data)
        print (calculated_AUC)
        report[current_report_line,]<-c(metadata[current_instance_num-1,],calculated_AUC)
        current_report_line<-current_report_line+1
    }  
    
    stop.time<- Sys.time()
    
    filename  <- paste("report",payment_selection_criteria,cost_function_type,max_total_cost,"repeat", counter_repeatitions,".csv")
    file_path <- file.path(directory, filename)
    
    #filename<-paste("report",payment_selection_criteria,cost_function_type,max_number_of_training_instance,"repeat", counter_repeatitions,".csv")
    write.table(report, file = file_path, sep = ",", col.names = NA)
    round(difftime(stop.time, start.time, units="mins"))
    
} #repettitions 
stopCluster(cl) 

stop.time<- Sys.time()
print (stop.time)






#########################################################################
#NON PARALLEL IMPLEMENTATION
# predict_set<- function(train_set,test_set){
#   #recieves train and test set and returns AUC over the test set
#   set.seed(global_seed)
#   model<-randomForest(y ~ .,train_set,ntree=100)                                 
#   set_output<-predict(model,test_set,type="prob")  
#   predictions<-as.vector(set_output[,2])
#   pred<-prediction(predictions,test_set$y)
#   perf_AUC<-performance(pred,"auc") #Calculate the AUC value
#   AUC<-perf_AUC@y.values[[1]]
#   
#   return(AUC)   
# }
# 
# cross_validation<-function(cv_data,num_folds,num_reruns){
#   #Returns the Average AUC for cross validation on a data set. Calls on function predict_set to calculate the AUC for each fold.
#   num_folds #number of Folds
#   data=cv_data  #data set used
#   sum_avg_AUC_all_CV_runs<-0 #this is later divided by the number of reruns
#   
#   for (j in 1:num_reruns){
#     set.seed(global_seed+j-1)
#     data$row_fold <- sample(1:num_folds, nrow(data), replace = TRUE)    # sample values from 1 to k, for each row in the data
#     #print (data$row_fold)
#     list <- 1:num_folds
#     sum_AUC_CV_run<-0
#     for (i in 1:num_folds){
#       # remove rows with row_fold i from dataframe to create training set. Select rows with row_fold i to create test set
#       train_data <- subset(data, row_fold %in% list[-i])
#       test_data <- subset(data, row_fold %in% c(i))
#       calculated_AUC<-predict_set(train_data,test_data) 
#       #print (calculated_AUC)
#       sum_AUC_CV_run<-sum_AUC_CV_run+calculated_AUC 
#     }
#     avg_AUC_CV_run<-sum_AUC_CV_run/num_folds #this is the specific cross validation result
#     #print(avg_AUC_CV_run)
#     sum_avg_AUC_all_CV_runs<-sum_avg_AUC_all_CV_runs+avg_AUC_CV_run #adding to the sum of all cross validation reruns
#   }
#   return(sum_avg_AUC_all_CV_runs/num_reruns) #returning the avg of all cross validation reruns
# }
#this was the older version of predict set
# predict_set<- function(train_set,test_set)
# {
#   # Validate assumption
#   if(!exists("global_seed")) {global_seed <- 1992}
#   
#   #recieves train and test set and returns AUC over the test set
#   set.seed(global_seed)
#   model       <- randomForest::randomForest(y ~ ., train_set, ntree=100)                                 
#   set_output  <- predict(model,test_set,type="prob")  
#   predictions <- as.vector(set_output[,2])
#   pred        <- ROCR::prediction(predictions,test_set$y)
#   perf_AUC    <- ROCR::performance(pred,"auc") #Calculate the AUC value
#   AUC         <- perf_AUC@y.values[[1]]
#   
#   return(AUC)   
# } # end predict_set


