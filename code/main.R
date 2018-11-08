##################
# Initialization #
##################
message("##################\n# Initialization #\n##################")
source(file.path(getwd(), "code", "scripts", "setup.R"))
#'
#########
# Setup #
#########
#' Dataset
DATABASE_NAME <- c("Spam",                 # 1
                   "Mushroom",             # 2
                   "Pen Digits",           # 3
                   "Otto",                 # 4
                   "Synthetic_Balanced",   # 5
                   "Synthetic_Unbalanced", # 6
                   "Tax Audit",            # 7
                   "Adult",                # 8
                   "Movies Reviews")[3]    # 9      
get_the_data(DATABASE_NAME)
#'
p_holdout    <- 0.3  # percentage of data in external holdout
initial_seed <- 1811 # large number
price_per_label_values <- c(0.02,0.14,0.25)
# price_per_label_values <- c(0.02,0.08,0.14,0.20,0.25)
#'
init_budget_size <- 15
k_batch_size <- 10
k_budget_size <- 3
num_price_per_label_values <- length(price_per_label_values) 
num_batches_per_cost_initial_training_set <- ceiling(300 / (k_batch_size * num_price_per_label_values))
#num_batches_per_cost_initial_training_set = 5
# if the batch_size is 10, 
# num_price_per_label_values = 3 and 
# num_batches_per_cost_initial_training_set = 5 then 
# this will purchase 150 instances.
# for random payment selection best to use 0
#'
max_total_cost <- 150 # should be larger than the cost of paying for the initial training batches
#'
#' The size (in terms of instances) of the number of last instances for each 
#' payment option to consider. To DEACTIVATE max_instances_in_history, use a 
#' very large number (larger than all the number of instances in data), say 1e6.
max_instances_in_history <- 100 
#'
number_batch_omissions  <<- 10
cross_validation_folds  <<- 8
cross_validation_reruns <<- 4
#'
#############################
# Simulation Configurations #
#############################
repeatitions <- 1 #20
#' Simulation nuances
param <- expand.grid(
    # What inducer should be used to fit models?
    model_inducer = c("RF","SVM","GLM","BAG","J48")[1],
    # By which rule to decide how much to pay for the next batch?
    payment_selection_criteria = c("random",            # 1
                                   "min_pay_per_label", # 2
                                   "avg_pay_per_label", # 3
                                   "max_pay_per_label", # 4
                                   "max_quality",       # 5
                                   "max_ratio",         # 6
                                   "max_total_ratio")   # 7
    [c(1)], 
    # Quality-Cost tradeoff
    primary_cost_function = c("Fix",        # 1
                              "Concave",    # 2   
                              "Asymptotic", # 3
                              "Linear")     # 4
    [c(2)],
    stringsAsFactors = FALSE)
param$primary_cost_function %<>% tolower()
#'
## Fix value
fixProbability = 0.85
#'
## Hash-table
if(any(param$primary_cost_function %in% "linear")){
    indices <- which(param$primary_cost_function %in% "linear")
    x_in <- range(price_per_label_values)
    y_in <- c(0.84, 0.86)
    x_out <- price_per_label_values
    y_out <- approx(x_in, y_in, x_out)$y   
    fixProbability <- data.frame(cost = x_out, probability = y_out)
    param[indices, "primary_cost_function"] <- "linear" %+% "_" %+% min(y_in) %+% "-" %+% max(y_in)
} 
#'
## Setup cost function change
secondary_cost_function_flag          = FALSE
secondary_cost_function               = c("Fix","Concave","Asymptotic")[1]
model_cost_for_changing_cost_function = 75
#'
################################################################################
#' Start simulation
################################################################################
cat_80("Start simulation")
#'
# Detects the number of cores and prepares for parallel run
cl <- makeCluster(detectCores() , outfile = "")
library(checkmate)
registerDoParallel(cl)
#'
# Run multiple simulations
for (s in 1:nrow(param)) {
    startSimTime <- Sys.time()
    #'
    # Setup simulation parameters
    model_inducer <- param[s, "model_inducer"]
    payment_selection_criteria <- param[s, "payment_selection_criteria"]
    primary_cost_function <- param[s, "primary_cost_function"]
    #'
    # Allocate report
    report <- create_report()
    ledger <- data.frame()
    metadata <- cbind(create_report(), svm_bug = data.frame())
    svm_bug <- NA
    #'
    # Start simulation timer
    start.time <- Sys.time()
    #'
    # Run simulation
    for (current_repetition in 1:repeatitions)
    {
        cost_function_type <- param[s, "primary_cost_function"]
        #' (1) Setup
        repetition_stage_1()
        #' (2) Split the data to 'unlabeled' and 'holdout'
        repetition_stage_2()
        #' (3) Purchase initial batches and fit model on them
        repetition_stage_3()
        #' (4) Running the rest of the simulation
        alt_repetition_stage_4()
    } # repetitions for loop
    #'
    ## Save report on hard drive
    slug <- file_slug_generate()
    write_csv(report %>% select(-svm_bug),
        path = file.path(k_path_reports, slug %+% ".csv")
    )
    write_csv(metadata %>% arrange(repetition, batch),
        path = file.path(k_path_metadata, slug %+% ".csv")
    )
    write_csv(ledger,
        path = file.path(k_path_ledgers, slug %+% ".csv")
    )
} # end multiple simulations
#'
stopCluster(cl)
stop.time <- Sys.time()
cat_80("Completed in " %+% round(as.numeric(stop.time - start.time, units = "mins"), 0) %+% " [mins]")
cat("\n")
#'
#'

print('Starting majority inference')
library(tidyr)
library(Matrix)
library(igraph)
mj_sample_size = 330
pay_per_label = price_per_label_values[2]
cost_function_type = c("Fix",        # 1
                          "Concave",    # 2   
                          "Asymptotic", # 3
                          "Linear")[3]
generate_majority <- function(mj_sample_size, budget, pay_per_label, cost_function_type, fixProbability){
    #num_l = floor(budget/pay_per_label/mj_sample_size)
    num_l = 6
    mj_sample_size = floor(budget/pay_per_label/num_l)
    #mj_sample_size = mj_sample_size - 1
    init_matrix = tryCatch(as.matrix(as_adj(sample_k_regular(mj_sample_size, num_l)))*1, 
                           error = function(e)as.matrix(as_adj(sample_k_regular(mj_sample_size - 1, num_l)))*1)
    mj_sample_size = dim(init_matrix)[1]
    init_matrix = data.frame(which(init_matrix!=0, arr.ind = T))
    colnames(init_matrix) = c('task','worker')
    init_matrix['label'] = 1
    training_set = unlabeled_data[sample(1:dim(unlabeled_data)[1],mj_sample_size),]
    labeling_accuracy <- labelingCostQualityTradeoff(
        cost_function_type,
        pay_per_label,
        fixProbability
    )
    set.seed(current_instance_num * global_seed)
    for(current_instance_num in 1:mj_sample_size){
        for(iter in 1:num_l){
            random_number <- runif(1)
            if (random_number > labeling_accuracy) {
                changed_value <-
                    change_level_value(training_set$y[current_instance_num])
                # Flag that the label is changed
                change <- 1
            }
            else {
                # Flag that the label is unchanged
                changed_value <-
                    as.numeric(training_set$y[current_instance_num])
                change <- 0
            }
            if(changed_value == 'level2'){
            init_matrix[init_matrix['task'] == current_instance_num,][iter,'label'] = 2}
            else if(changed_value == 'level1'){
                init_matrix[init_matrix['task'] == current_instance_num,][iter,'label'] = 1}
            else init_matrix[init_matrix['task'] == current_instance_num,][iter,'label'] = changed_value
        }
    }
    
    return(list(init_matrix, training_set))
}

cost_function_type = c("Fix",        # 1
                       "Concave",    # 2   
                       "Asymptotic", # 3
                       "Linear")[1]
pay_per_label = price_per_label_values[1]
record = data.frame()
for(repetition in 1:10){
    for(budget in 3:150){
        res = generate_majority(mj_sample_size, budget, pay_per_label, cost_function_type, fixProbability)
        init_matrix = res[1][[1]]
        training_set_mj = res[2][[1]]
        write.csv(init_matrix,'/home/ruijiang/E/utaustin/project/cost_efficient_labeling/KargerAlgorithm/rawmj.csv', row.names = FALSE)
        setwd('/home/ruijiang/E/utaustin/project/cost_efficient_labeling/KargerAlgorithm')
        system('python2 messgae_passing.py')
        
        inference_result = read.csv('output.csv', header = FALSE)
        setwd('/home/ruijiang/E/utaustin/project/cost_efficient_labeling/Quick-and-Dirty')
        inference_result = inference_result[,c(1,32)]
        inference_result$V1 = as.numeric(inference_result$V1)
        inference_result = inference_result[order(inference_result$V1),]
        inference_result$V32 = as.character(inference_result$V32)
        inference_result$V32[inference_result$V32 == 'True'] = 'level1'
        inference_result$V32[inference_result$V32 == 'False'] = 'level2'
        inference_result$V32 = as.factor(inference_result$V32)
        print('inference result')
        print(sum(as.numeric(inference_result$V32) == as.numeric(training_set_mj$y))/dim(inference_result)[1])
        training_set_mj$y = inference_result$V32
        calculated_AUC <- predict_set(training_set_mj,
                                      holdout_data,
                                      inducer = model_inducer
        )
        calculated_AUC
        temp = data.frame(list(nrow(init_matrix),pay_per_label,1,budget,calculated_AUC,'NA', 'NA','NA','NA',repetition))
        names(temp) = names(record)
        record = rbind(record, temp)
    }
}
colnames(record) = c('instance_num','pay','change','cost_so_far','AUC_holdout','full_AUC','subset_AUC','updated_label','batch','repetition')
write.csv(record,'summary.csv')
