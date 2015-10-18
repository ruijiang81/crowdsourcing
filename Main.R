## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$", path="./functions/", full.names=TRUE), source)


## Setup
DATABASE_NAME="Otto" #"Spam","Otto"

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


