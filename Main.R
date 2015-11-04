## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$", path="./functions/", full.names=TRUE), source)
# options(error=recover) # debugging mode

## Setup
### Worst-case execution time
watchdog_simulation = as.difftime(24, units="hours")
### Dataset
DATABASE_NAME="Synthetic_Balanced" #"Spam","Otto","Synthetic_Balanced","Synthetic_Unbalanced"

cores_not_to_use  = 0 #0 means use all cores
p_holdout         = 0.5 #percentage of data in external holdout
initial_seed      = 2015 #large number
batch_size             <<- 10
number_batch_omissions <<- 10
num_batches_per_cost_initial_training_set=5 # 5  e.g., if the batch size is 10, num_price_per_label_values=5 and num_batches_per_cost_initial_training_set=5 then this will purchase 250 instances
#for random payment selection best to use 0
price_per_label_values= c(0.02,0.08,0.14,0.19,0.25)
max_total_cost = 150 #should be larger than the cost of paying for the initial training batches

#if reverting to max_number_of_training_instance instead of max_total_cost then activate this manually in the while loop
#max_number_of_training_instance<-1000 #should at least eqaul to  batch_size*num_batches_per_cost_initial_training_set*(num_price_per_label_values)


cross_validation_folds  <<- 8 #global10
cross_validation_reruns <<- 4 #global5
repeatitions <- 10 #10


## Control simulation nuances
param <- expand.grid(
    # What inducer should be used to fit models?
    model_inducer=c("RF","GLM","J48")[c(1)],
    # By which rule to decide how much to pay for the next batch?
    payment_selection_criteria=c("random", "min_pay_per_label", "max_pay_per_label",
                                 "max_quality", "max_ratio", "max_total_ratio",
                                 "delta_AUC_div_total_cost")[c(1)],
    # Quality-Cost tradeoff
    cost_function_type = c("Fix","Concave","Asymptotic")[2],
    stringsAsFactors=FALSE)


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

for(s in 1:nrow(param)){
    ## Setup simulation parameters
    model_inducer              = param[s,"model_inducer"]
    payment_selection_criteria = param[s,"payment_selection_criteria"]
    cost_function_type         = param[s,"cost_function_type"]
    
    ## Allocate report
    report   = create_report()
    metadata = create_report()
    
    
    ## Start simulation timer
    start.time = Sys.time()
    
    
    for(counter_repeatitions in 1:repeatitions)
    {
        ########################################################################
        #' Repetition setup
        ########################################################################
        if (Sys.time()-start.time >= watchdog_simulation) break # watchdog stop execution
        global_seed <<- initial_seed*counter_repeatitions
        rep_metadata = create_report()
        rep_report   = create_report()
        counter_batches = 1
        current_report_line = 1
        # Display repetition info
        cat('\n', rep('#',40), 
            '\n', "current repeatition: ", counter_repeatitions,
            '\n', rep('#',40),
            sep="")
        
        
        ########################################################################
        #' Split the data to 'unlabeled' and 'holdout'
        ########################################################################
        #' The holdout-set is fixed throughout the simulation
        #' The unlabeled-set is shuffled differently in each repetition
        set.seed(global_seed)
        index.holdout  = sample(nrow(dataset), round(nrow(dataset)*p_holdout))
        holdout_data   = dataset[index.holdout,]
        unlabeled_data = dataset[-sample(index.holdout),]
        
        max_size_training_data<-nrow(unlabeled_data) #used later for sanity check
        
        
        ########################################################################
        #' Purchase initial batches and fit model on them
        ########################################################################
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
                    
                    ############################################################
                    #' Change label quality (instance-wise implementation)
                    ############################################################
                    for (k in 1:batch_size) {
                        ## Bind train-set and labeled-set
                        if (current_instance_num==1){
                            training_set = unlabeled_data[1,]
                        } 
                        else {
                            training_set<-rbind(training_set,unlabeled_data[current_instance_num,])
                        } # end binding train/labeled sets
                        
                        
                        ## Alternate true label (instance-wise operation)
                        set.seed(current_instance_num*global_seed)
                        random_number <- runif(1)
                        if (random_number>labeling_accuracy){
                            training_set$y[current_instance_num] <- change_level_value(training_set$y[current_instance_num])
                            change<-1
                        } else {
                            change<-0
                        } 
                        
                        #cost_so_far=cost_so_far+price_per_label_values[i]
                        cost_so_far=cost_so_far+pay_per_label
                        new_entry = data.frame("instance_num"=current_instance_num,
                                               "pay"=pay_per_label,
                                               "change"=change,
                                               "cost_so_far"=cost_so_far,
                                               "updated_label"=training_set$y[current_instance_num],
                                               "batch"=counter_batches)
                        rep_metadata = merge(rep_metadata, new_entry, all=TRUE)
                        
                        current_instance_num = current_instance_num+1 # updating the instance counter
                    } # end for change instance quality
                    
                    counter_batches = counter_batches+1 # updating the batch counter
                } # end for batch purchase
                
            } 
            
            calculated_AUC = predict_set(training_set,
                                         holdout_data,
                                         inducer=model_inducer)
            cat('\n',"Finished purchasing initial training set")
            cat('\n',"AUC =",calculated_AUC)
        } # end Purchase initial batches
        
        
        ########################################################################
        #' Running the rest of the simulation
        ########################################################################
        #while (current_instance_num<=max_number_of_training_instance) {
        while ((cost_so_far <= max_total_cost) & 
               (Sys.time()-start.time < watchdog_simulation)){
            cat("\n","Total model cost",paste0(round(cost_so_far,1),"$"))
            cat("\n","Total instances in the model",current_instance_num)
            
            if (current_instance_num+batch_size>max_size_training_data)
            {stop("current_instance_num+batch_size>max_size_training_data")} #sanity check that the allocation of instances according to cost doesn't exceed the number of initially available unlabelled data
            ####################################################################
            #' Choose next cost to pay
            ####################################################################
            pay_per_label = decide_price_per_label(training_set,
                                                   payment_selection_criteria,
                                                   price_per_label_values,
                                                   current_instance_num,
                                                   rep_metadata,
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
                new_entry = data.frame("instance_num"=current_instance_num,
                                       "pay"=pay_per_label,
                                       "change"=change,
                                       "cost_so_far"=cost_so_far,
                                       "updated_label"=training_set$y[current_instance_num],
                                       "batch"=counter_batches)
                rep_metadata = merge(rep_metadata, new_entry, all=TRUE)
                
                current_instance_num<-current_instance_num+1 # updating the instance counter
            }
            
            counter_batches = counter_batches+1 # updating the batch counter
            
            
            calculated_AUC = predict_set(training_set,
                                         holdout_data,
                                         inducer=model_inducer)
            cat('\n',"AUC =",calculated_AUC)
            
            ## Store iteration metadata in the report
            rep_metadata[current_instance_num-1,"AUC_holdout"] = calculated_AUC
            new_entry            = rep_metadata[current_instance_num-1,]
            new_entry$repetition = counter_repeatitions
            new_entry$batch      = counter_batches
            
            
            
            if(payment_selection_criteria %in% c("max_quality","max_ratio","max_total_ratio","delta_AUC_div_total_cost")){
                new_entry$full_AUC = NA
                new_entry$subset_AUC = NA
                ## Add data from text file
                dir_path = file.path(getwd(),"results","temp folder")
                delta_performance = read.csv(file.path(dir_path,"delta_performance_improvements.txt"), header = FALSE)
                full_performance  = read.csv(file.path(dir_path,"full_performance_improvements.txt"), header = FALSE)
                ## Add full performance
                new_entry$full_AUC = full_performance[1,"V2"]
                ## Add delta performance
                dn = nrow(delta_performance)
                dm = ncol(delta_performance)
                ### Duplicate new_entry
                for(i in 2:(dm-1)) new_entry[i,] = new_entry[i-1,]
                ### Store subset AUC
                for(i in 2:dm) new_entry[i-1,"subset_AUC"] = delta_performance[i]
            } else {
                new_entry$full_AUC   = NA
                new_entry$subset_AUC = NA
            }
            
            rep_report = rbind(rep_report,new_entry)
            current_report_line <- current_report_line+1
        } # end Running the rest of the simulation
        
        report   = rbind(report, rep_report)
        rep_metadata$repetition = counter_repeatitions
        metadata = merge(metadata, rep_metadata, all=TRUE)
    } #repetitions
    
    ## Save report on hard drive
    report_dir   = file.path(getwd(),"results")
    metadata_dir = file.path(report_dir,"metadata")
    file_name    = paste0('(',tolower(DATABASE_NAME),')',
                          '(',toupper(model_inducer),')',
                          '(',tolower(cost_function_type),')',
                          '(',tolower(payment_selection_criteria),')',
                          '(',Sys.Date(),')',".csv")
    
    dir.create(report_dir, show=FALSE, recursive=TRUE)
    write.csv(report, file=file.path(report_dir,file_name), row.names=F)
    dir.create(metadata_dir, show=FALSE, recursive=TRUE)
    write.csv(metadata, file=file.path(metadata_dir,file_name), row.names=F)
} # end simulation

stopCluster(cl) 
stop.time<- Sys.time()
