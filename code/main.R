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
                   "Movies Reviews")[1]    # 9      
get_the_data(DATABASE_NAME)
#'
p_holdout    <- 0.3  # percentage of data in external holdout
initial_seed <- 1811 # large number
price_per_label_values <- c(0.02,0.14,0.25)
#price_per_label_values = c(0.02,0.08,0.14,0.19,0.25)
#'
batch_size                                <- 10
num_price_per_label_values                <- length(price_per_label_values) 
num_batches_per_cost_initial_training_set <- ceiling(300/(batch_size*num_price_per_label_values))
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
repeatitions <- 20 #20
#' Simulation nuances
param <- expand.grid(
    # What inducer should be used to fit models?
    model_inducer=c("RF","SVM","GLM","BAG","J48")[1],
    # By which rule to decide how much to pay for the next batch?
    payment_selection_criteria=c("random",            # 1
                                 "min_pay_per_label", # 2
                                 "max_pay_per_label", # 3
                                 "max_quality",       # 4
                                 "max_ratio",         # 5
                                 "max_total_ratio")   # 6
    [c(6)], 
    # Quality-Cost tradeoff
    primary_cost_function = c("Fix",               # 1
                              "Concave",           # 2   
                              "Asymptotic",        # 3
                              "Fix3Labels",        # 4
                              "Concave3Labels",    # 5
                              "Asymptotic3Labels") # 6
    [c(3)],
    stringsAsFactors = FALSE)
#'
## Fix value
fixProbability = 0.85
#'
## Hash-table
primary_cost_function = tolower(param[1,"primary_cost_function"])
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
#'
## Setup cost function change
secondary_cost_function_flag          = FALSE
secondary_cost_function               = c("Fix","Concave","Asymptotic","HashTable")[2]
model_cost_for_changing_cost_function = 75
#'
################################################################################
#' Start simulation
################################################################################
cat_80("Start simulation")
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
    ledger   = data.frame()
    metadata = cbind(create_report(), svm_bug = data.frame())
    svm_bug  = NA
    
    ## Start simulation timer
    start.time = Sys.time()
    
    for(current_repetition in 1:repeatitions)
    {
        cost_function_type <- param[s,"primary_cost_function"]
        #' (1) Setup
        repetition_stage_1()
        #' (2) Split the data to 'unlabeled' and 'holdout'
        repetition_stage_2()
        #' (3) Purchase initial batches and fit model on them
        repetition_stage_3()
        #' (4) Running the rest of the simulation
        repetition_stage_4()
    }# repetitions for loop 
    #'
    ## Save report on hard drive
    slug <- generate_file_slug()
    write_csv(report %>% select(-svm_bug),
              path = file.path(k_path_reports, slug %+% ".csv"))
    write_csv(metadata %>% arrange(repetition, batch),
              path = file.path(k_path_metadata, slug %+% ".csv"))
    write_csv(ledger,
              path = file.path(k_path_ledgers, slug %+% ".csv"))
} # end simulation
#'
stopCluster(cl)
stop.time <- Sys.time()
cat_80("Completed in " %+% round(as.numeric(stop.time-start.time, units = "mins"),0) %+% " [mins]")
#'
