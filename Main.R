## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$", path="./functions/", full.names=TRUE), source)


## Setup
DATABASE_NAME="Synthetic_Balanced" #"Spam","Otto","Synthetic_Balanced","Synthetic_Unbalanced"

cores_not_to_use  = 0 #0 means use all cores
p_holdout         = 0.3 #percentage of data in external holdout
initial_seed      = 2015 #large number
batch_size        <<-10
number_batch_omissions<<-10
num_batches_per_cost_initial_training_set=5 # 5  e.g., if the batch size is 10, num_price_per_label_values=5 and num_batches_per_cost_initial_training_set=5 then this will purchase 250 instances
#for random payment selection best to use 0
price_per_label_values= c(0.02,0.08,0.14,0.19,0.25)
max_total_cost<-80 #should be larger than the cost of paying for the initial training batches

model_inducer = c("RF","GLM","J48")[2]

#if reverting to max_number_of_training_instance instead of max_total_cost then activate this manually in the while loop
#max_number_of_training_instance<-1000 #should at least eqaul to  batch_size*num_batches_per_cost_initial_training_set*(num_price_per_label_values)

payment_selection_criteria = "random" #"random", "max_quality", "max_ratio", "max_total_ratio","delta_AUC_div_total_cost", "min_pay_per_label", "max_pay_per_label"
cost_function_type         = "Concave" #"Fix","Concave"',"Asymptotic"

cross_validation_folds<<-8 #global10
cross_validation_reruns<<-4 #global5
repeatitions <- 1 #10

#GENERATING A NEW DIRECTORY FOR THE RESULTS
directory <<- file.path(getwd(), DATABASE_NAME, payment_selection_criteria)
directory
dir.create(directory, showWarnings = FALSE, recursive = TRUE, mode = "0777")


## Get the data
DATABASE_NAME <- tolower(DATABASE_NAME)
if(DATABASE_NAME=="otto"){
    dataset <- read.csv(file.path(getwd(),'data','Otto','train.csv'), colClasses=c("integer",rep("numeric",93),"factor"))
    dataset <- dataset[,-1] #deletes the first (ID) column 
    dataset <- oneVsAll(dataset, positive.class=3) #customized function that assigns one positive and one negative class
    
} else if (DATABASE_NAME=="spam") {
    library("kernlab")  
    data(spam)
    dataset <- spam
    
} else if (DATABASE_NAME=="synthetic_balanced") {
    source("scripts/generate_balanced_dataset.R")
    
} else if (DATABASE_NAME=="synthetic_unbalanced") {
    source("scripts/generate_unbalanced_dataset.R")
    
} else {
    error("Unknow dataset")
} # end get the data

dataset <- setVariablesNames(dataset)


################################################################################
#' Start simulation
################################################################################
# Detects the number of cores and prepares for parallel run
cl <- makeCluster(detectCores()-cores_not_to_use,outfile="")   
registerDoParallel(cl)
# Allocate report
report_columns = c("instance_num", "pay", "change", "cost_so_far", "AUC_holdout") 
report = c()

for(counter_repeatitions in 1:repeatitions)
{
    ############################################################################
    #' Repetition setup
    ############################################################################
    global_seed <<- initial_seed*counter_repeatitions
    start.time   <- Sys.time()
    # Defining the columns of the metadata table
    metadata_columns = c("instance_num", "pay", "change", "cost_so_far") 
    metadata   = read.table(text = "", col.names = metadata_columns)
    rep_report = read.table(text = "", col.names = report_columns)
    current_report_line = 1
    # Display repetition info
    cat('\n', rep('#',40), 
        '\n', "current repeatition: ", counter_repeatitions,
        '\n', rep('#',40),
        sep="")
    
    
    ############################################################################
    #' Split the data to 'unlabeled' and 'holdout'
    ############################################################################
    #' The holdout-set is fixed throughout the simulation
    #' The unlabeled-set is shuffled differently in each repetition
    set.seed(initial_seed)
    index.holdout  = sample(nrow(dataset), round(nrow(dataset)*p_holdout))
    holdout_data   = dataset[index.holdout,]
    set.seed(global_seed)
    unlabeled_data = dataset[-sample(index.holdout),]
    
    max_size_training_data<-nrow(unlabeled_data) #used later for sanity check
    
    
    ############################################################################
    #' Purchase initial batches and fit model on them
    ############################################################################
    #' purchasing intial training set using different prices
    # Cost to pay for each intial batch 
    num_price_per_label_values = length(price_per_label_values) 
    cost_so_far = 0
    current_instance_num = 1
    
    if (num_batches_per_cost_initial_training_set>0){
        #' num_batches_per_cost_initial_training_set may be set to zero when 
        #' using random or other non algorthmic payment selection methods
        for (i in 1:num_price_per_label_values){
            for (j in 1:num_batches_per_cost_initial_training_set){
              
              set.seed(current_instance_num*global_seed)
              pay_per_label<-sample(price_per_label_values,1)
              labeling_accuracy<-labelingCostQualityTradeoff(cost_function_type,
                                                             pay_per_label)  
              for (k in 1:batch_size) {
                    if (current_instance_num==1){
                        training_set<-unlabeled_data[1,]
                    } 
                    else {
                        training_set<-rbind(training_set,unlabeled_data[current_instance_num,])
                    }
                    
                    #batch payment option                  
                    #labeling_accuracy<-labelingCostQualityTradeoff(cost_function_type,
                    #                                               price_per_label_values[i])
                    
                                                                   
                    set.seed(current_instance_num*global_seed)
                    random_number <- runif(1)
                    if (random_number>labeling_accuracy){
                        training_set$y[current_instance_num] <- change_level_value(training_set$y[current_instance_num])
                        change<-1
                    }
                    else {
                        change<-0
                    }
                    #cost_so_far=cost_so_far+price_per_label_values[i]
                    cost_so_far=cost_so_far+pay_per_label
                    metadata[current_instance_num,]<-c(current_instance_num,
                                                       price_per_label_values[i],
                                                       change,cost_so_far)
                    
                    current_instance_num<-current_instance_num+1 #updating the counter
                }
            }
        }
        
        calculated_AUC = predict_set(training_set,
                                     holdout_data,
                                     inducer=model_inducer)
        cat('\n',"Finished purchasing initial training set")
        cat('\n',"AUC =",calculated_AUC)
    
    }  
   
    
    ############################################################################
    #' Running the rest of the simulation
    ############################################################################
    #while (current_instance_num<=max_number_of_training_instance) {
    while (cost_so_far<=max_total_cost){
        cat("\n","Total model cost",paste0(round(cost_so_far,1),"$"))
        cat("\n","Total instances in the model",current_instance_num)
        
        if (current_instance_num+batch_size>max_size_training_data)
        {stop("current_instance_num+batch_size>max_size_training_data")} #sanity check that the allocation of instances according to cost doesn't exceed the number of initially available unlabelled data
        ########################################################################
        #' Choose next cost to pay
        ########################################################################
        pay_per_label = decide_price_per_label(training_set,
                                               payment_selection_criteria,
                                               price_per_label_values,
                                               current_instance_num,
                                               metadata,
                                               counter_repeatitions,
                                               inducer=model_inducer)
        
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
        
        calculated_AUC = predict_set(training_set,
                                     holdout_data,
                                     inducer=model_inducer)
        cat('\n',"AUC =",calculated_AUC)
        rep_report[current_report_line,] = c(metadata[current_instance_num-1,],calculated_AUC)
        current_report_line<-current_report_line+1
    }  
    
    stop.time = Sys.time()
    rep_report$repettition = counter_repeatitions
    report = rbind(report, rep_report)
    #rm(rep_report)
    round(difftime(stop.time, start.time, units="mins"))
    
} #repettitions

## Save report on hard drive
file_name = paste("report", payment_selection_criteria, cost_function_type, max_total_cost,".csv")
file_path = file.path(directory, file_name)
write.csv(report, file = file_path, row.names=F)


stopCluster(cl) 
stop.time<- Sys.time()
