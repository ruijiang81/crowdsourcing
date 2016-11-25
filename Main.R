## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$", path="./functions/", full.names=TRUE), source)
# options(error=recover) # debugging mode
options(digits=4)


## Setup
### Get unique ID for the run
runID <<- paste0(sample(c(letters,0:9),20),collapse="")
### Worst-case execution time
watchdog_simulation = as.difftime(24*3, units="hours")
### Dataset
DATABASE_NAME=
    c("Spam",                 # 1
      "Mushroom",             # 2
      "Pen Digits",           # 3
      "Otto",                 # 4
      "Synthetic_Balanced",   # 5
      "Synthetic_Unbalanced", # 6
      "Tax Audit",            # 7
      "Adult",                # 8
      "Movies Reviews"        # 9      
    )[1]

p_holdout         = 0.3 #percentage of data in external holdout
initial_seed      = 1811 #large number
batch_size             <<- 10
number_batch_omissions <<- 10
num_batches_per_cost_initial_training_set=10 # 5 e.g., if the batch size is 10, num_price_per_label_values=5 and num_batches_per_cost_initial_training_set=5 then this will purchase 250 instances
#for random payment selection best to use 0
#price_per_label_values = c(0.02,0.08,0.14,0.19,0.25)
price_per_label_values = c(0.02,0.14,0.25)

max_total_cost = 150 # should be larger than the cost of paying for the initial training batches

max_instances_in_history <<- 100 #the size (in terms of instances) of the number of last instances for each payment option to consider
#to DEACTIVATE this option use a very large number (larger than all the number of instances in data)

#if reverting to max_number_of_training_instance instead of max_total_cost then activate this manually in the while loop
#max_number_of_training_instance<-1000 #should at least eqaul to  batch_size*num_batches_per_cost_initial_training_set*(num_price_per_label_values)


cross_validation_folds  <<- 8 #global10
cross_validation_reruns <<- 4 #global5
repeatitions <- 20 #20


## Control simulation nuances
param <- expand.grid(
    # What inducer should be used to fit models?
    model_inducer=c("RF","SVM","GLM","BAG","J48")[2],
    # By which rule to decide how much to pay for the next batch?
    payment_selection_criteria=c("random",              # 1
                                 "min_pay_per_label",   # 2
                                 "max_pay_per_label",   # 3
                                 "max_quality",         # 4
                                 "max_ratio",           # 5
                                 "max_total_ratio")[6], # 6
    # Quality-Cost tradeoff
    primary_cost_function = c("Fix",                   # 1
                              "Concave",               # 2   
                              "Asymptotic",            # 3
                              "Fix3Labels",            # 4
                              "Concave3Labels",        # 5
                              "Asymptotic3Labels")[3], # 6
    stringsAsFactors=FALSE)

## Fix value
fixProbability = 0.85

## Hash-table
primary_cost_function = tolower(param[1,"primary_cost_function"])
# if(primary_cost_function %in% c("fix3labels","concave3labels","asymptotic3labels")))
if(primary_cost_function %in% "fix3labels"){
    price_per_label_values = 3*price_per_label_values
    fixProbability = data.frame(cost=price_per_label_values,
                                probability=c(0.93925,0.93925,0.93925))
    
} else if (primary_cost_function %in% "concave3labels") {
    price_per_label_values = 3*price_per_label_values
    fixProbability = data.frame(cost=price_per_label_values,
                                probability=c(0.6526018,0.9978207,0.8493373))
    
} else if (primary_cost_function %in% "asymptotic3labels") {
    price_per_label_values = 3*price_per_label_values
    fixProbability = data.frame(cost=price_per_label_values,
                                probability=c(0.5000000,0.9854227,0.9953280))
    
}


## Setup cost function change
secondary_cost_function_flag          = FALSE
secondary_cost_function               = c("Fix","Concave","Asymptotic","HashTable")[2]
model_cost_for_changing_cost_function = 75


## Get the data
DATABASE_NAME <- tolower(DATABASE_NAME)
if(DATABASE_NAME=="otto"){
    source("./data/Otto/import dataset.R")
    
} else if (DATABASE_NAME=="spam") {
    library("kernlab")  
    data(spam)
    dataset <- spam
    
} else if (DATABASE_NAME=="synthetic_balanced") {
    source("scripts/generate_balanced_dataset.R")
    
} else if (DATABASE_NAME=="synthetic_unbalanced") {
    source("scripts/generate_unbalanced_dataset.R")
    
} else if (DATABASE_NAME=="tax audit") {
    source("./data/Tax Audit/import dataset.R")
    
} else if (DATABASE_NAME=="mushroom") {
    source("./data/Mushroom/import dataset.R")
    
} else if (DATABASE_NAME=='adult') {
    source("./data/Adult/import dataset.R")
    
} else if (DATABASE_NAME=='pen digits') {
    source("./data/Pen Digits/import dataset.R")
    
} else if (DATABASE_NAME=='movies reviews') {
    source("./data/Movie Review/import dataset.R")
    
} else {
    error("Unknow dataset")
} # end get the data

dataset <- setVariablesNames(dataset)


################################################################################
#' Start simulation
################################################################################
# Detects the number of cores and prepares for parallel run
cl <- makeCluster(detectCores(),outfile="")   
registerDoParallel(cl)

for(s in 1:nrow(param)){
    startSimTime  = Sys.time()

        
    ## Setup simulation parameters
    model_inducer              = param[s,"model_inducer"]
    payment_selection_criteria = param[s,"payment_selection_criteria"]
    cost_function_type         = param[s,"primary_cost_function"]

        
    ## Allocate report
    report   = create_report()
    metadata = create_report()
    
    
    ## Start simulation timer
    start.time = Sys.time()
    
    
    for(counter_repetitions in 1:repeatitions)
    {
        cost_function_type = param[s,"primary_cost_function"]
        #' In order to handle repetitions failure we encapsulate the repetitions  
        #' in tryCatch.
        #' For example, initial batches don't encompass all the available costs.
        tryCatch({
            ####################################################################
            #' Repetition setup
            ####################################################################
            if (Sys.time()-start.time >= watchdog_simulation) break # watchdog stop execution
            global_seed <<- initial_seed*counter_repetitions
            rep_metadata = create_report()
            rep_report   = create_report()
            counter_batches = 1
            current_report_line = 1
            # Display repetition info
            cat('\n', rep('#',40), 
                '\n', "current repeatition: ", counter_repetitions,
                '\n', rep('#',40),
                sep="")
            
            
            ####################################################################
            #' Split the data to 'unlabeled' and 'holdout'
            ####################################################################
            #' The holdout-set is fixed throughout the simulation
            #' The unlabeled-set is shuffled differently in each repetition
            set.seed(global_seed)
            index_holdout  = sample(nrow(dataset), round(nrow(dataset)*p_holdout))
            holdout_data   = dataset[index_holdout,]
            unlabeled_data = dataset[-index_holdout,]
            unlabeled_data = unlabeled_data[sample(nrow(unlabeled_data)),]
            max_size_training_data = nrow(unlabeled_data) #used later for sanity check
            
            
            ####################################################################
            #' Purchase initial batches and fit model on them
            ####################################################################
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
                        
                        if (secondary_cost_function_flag & (cost_so_far>model_cost_for_changing_cost_function))
                            cost_function_type = secondary_cost_function
                        
                        labeling_accuracy<-labelingCostQualityTradeoff(cost_function_type,
                                                                       pay_per_label,
                                                                       fixProbability)  
                        
                        ########################################################
                        #' Change label quality (instance-wise implementation)
                        ########################################################
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
                        
                        #################################
                        # Evaluate model on unseen data #
                        #################################
                        ## AUC
                        calculated_AUC = predict_set(training_set,
                                                     holdout_data,
                                                     inducer=model_inducer)
                        ## Top N measures
                        # topN = top_n(training_set,
                        #              holdout_data,
                        #              inducer=model_inducer)
                        
                        #printing out to report
                        rep_metadata[current_instance_num-1,"AUC_holdout"] = calculated_AUC
                        # for(col_name in colnames(topN))
                        #     rep_metadata[current_instance_num-1,col_name] = topN[1,col_name]
                        new_item            = rep_metadata[current_instance_num-1,]
                        new_item$repetition = counter_repetitions
                        new_item$batch      = counter_batches-1
                        rep_report = rbind(rep_report,new_item)
                        current_report_line <- current_report_line+1  
                        
                    } # end for batch purchase
                    
                } 
                
                
                cat('\n',"Finished purchasing initial training set")
                cat('\n',"AUC =",calculated_AUC)
                
                
                
                
                
            } # end Purchase initial batches
            
            
            # ---------------------------------------------------------------- #
            
            
            ####################################################################
            #' Running the rest of the simulation
            ####################################################################
            
            
            #while (current_instance_num<=max_number_of_training_instance) {
            while ((cost_so_far <= max_total_cost) & 
                   (Sys.time()-start.time < watchdog_simulation)){
                cat("\n","Total model cost",paste0(round(cost_so_far,1),"$"))
                cat("\n","Total instances in the model",current_instance_num)
                
                
                ##################
                #' Sanity Checks #
                ##################
                # Handle the "run out of instances" issue
                if(current_instance_num + batch_size > max_size_training_data){
                    cat("\n", "Out of unlabeled instances")
                    break
                } 
                
                
                ################################################################
                #' Choose next cost to pay
                ################################################################
                pay_per_label = decide_price_per_label(training_set,
                                                       payment_selection_criteria,
                                                       price_per_label_values,
                                                       current_instance_num,
                                                       rep_metadata,
                                                       counter_repetitions,
                                                       inducer=model_inducer)
                
                for (k in 1:batch_size) {
                    
                    if (current_instance_num==1){
                        training_set<-unlabeled_data[1,]
                    } 
                    else {
                        training_set<-rbind(training_set,unlabeled_data[current_instance_num,])
                    }
                    
                    if (secondary_cost_function_flag & (cost_so_far>model_cost_for_changing_cost_function))
                        cost_function_type = secondary_cost_function
                    labeling_accuracy<-labelingCostQualityTradeoff(cost_function_type,
                                                                   pay_per_label,
                                                                   fixProbability)
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
                
                #counter_batches = counter_batches+1 # updating the batch counter
                
                
                #################################
                # Evaluate model on unseen data #
                #################################
                ## AUC
                calculated_AUC = predict_set(training_set,
                                             holdout_data,
                                             inducer=model_inducer)
                cat('\n',"AUC =",calculated_AUC)
                ## Top N measures
                # topN = top_n(training_set,
                #              holdout_data,
                #              inducer=model_inducer)
                
                ## Store iteration metadata in the report
                rep_metadata[current_instance_num-1,"AUC_holdout"] = calculated_AUC
                # for(col_name in colnames(topN))
                #     rep_metadata[current_instance_num-1,col_name] = topN[1,col_name]
                new_item            = rep_metadata[current_instance_num-1,]
                new_item$repetition = counter_repetitions
                new_item$batch      = counter_batches
                
                
                
                if(payment_selection_criteria %in% c("max_quality","max_ratio","max_total_ratio","delta_AUC_div_total_cost")){
                    new_item$full_AUC = NA
                    new_item$subset_AUC = NA
                    ## Add data from text file
                    dir_path = file.path(getwd(),"results","temp folder",runID)
                    delta_performance = read.csv(file.path(dir_path,"delta_performance_improvements.txt"), header = FALSE)
                    full_performance  = read.csv(file.path(dir_path,"full_performance_improvements.txt"), header = FALSE)
                    ## Add full performance
                    new_item$full_AUC = full_performance[1,"V2"]
                    ## Add delta performance
                    dn = nrow(delta_performance)
                    dm = ncol(delta_performance)
                    ### Duplicate new_item
                    for(i in 2:(dm-1)) new_item[i,] = new_item[i-1,]
                    ### Store subset AUC
                    for(i in 2:dm) new_item[i-1,"subset_AUC"] = delta_performance[i]
                } else {
                    new_item$full_AUC   = NA
                    new_item$subset_AUC = NA
                }
                
                rep_report = rbind(rep_report,new_item)
                counter_batches = counter_batches+1 # updating the batch counter
                current_report_line <- current_report_line+1
            } # end Running the rest of the simulation
            
            report   = rbind(report, rep_report)
            rep_metadata$repetition = counter_repetitions
            metadata = merge(metadata, rep_metadata, all=TRUE)
        },
        error = function(cond){ # if the model fitting or the evaluation failed, return AUC = 0.5 with a warning
            warning(paste0("failed to run repetition #",counter_repetitions,"; ignoring repetition"))
            return(NA)
        } # end error
        ) # end tryCatch
    } #repetitions
    
    ## Save report on hard drive
    finishSimTime      = Sys.time()
    Time.Diff          = round(as.numeric(finishSimTime-startSimTime, units = "mins"),0)
    report_dir         = file.path(getwd(),"results")
    metadata_dir       = file.path(report_dir,"metadata")
    primary_cost_function = param[s,"primary_cost_function"]
    cost_function_type = ifelse(secondary_cost_function_flag,paste0(primary_cost_function,2,secondary_cost_function),primary_cost_function)
    file_name          = paste0('(',tolower(DATABASE_NAME),')',
                                '(',toupper(model_inducer),')',
                                '(',tolower(cost_function_type),')',
                                '(',tolower(payment_selection_criteria),max_instances_in_history,')',
                                '(',Sys.Date(),')',
                                '(',paste0(Time.Diff,' minutes'),')',
                                ".csv")
    
    dir.create(report_dir, show=FALSE, recursive=TRUE)
    write.csv(report, file=file.path(report_dir,file_name), row.names=F)
    dir.create(metadata_dir, show=FALSE, recursive=TRUE)
    write.csv(metadata, file=file.path(metadata_dir,file_name), row.names=F)
} # end simulation

stopCluster(cl)

stop.time <- Sys.time()
cat("\n",rep("#",40),
    "\n# Completed in ", round(as.numeric(stop.time-start.time, units = "mins"),0), " [mins]",
    "\n",rep("#",40), sep="")
