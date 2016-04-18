################################################################################
## Analyze Reports - Calculate Differences
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
invisible(sapply(list.files(pattern="[.]R$",path="./functions/",full.names=TRUE), source))


################
# Get the data #
################
reports_folder = file.path(getwd(),"reports")
reports = import.reports(reports_folder,
                         # Remove the "random" rule metadata
                         random.rm=FALSE)
interval_size = 1
isolated_repetitions = FALSE
outputs = interpolate.reports(reports_folder,
                              na.rm=FALSE,
                              interval_size,
                              isolated_repetitions)
head(outputs)


###############################
# Create different reports by #
###############################
report_div = c("DATABASE_NAME","model_inducer","cost_function_type")
report_param = unique(outputs[,report_div])


##########################################
# Export "AUC as function of Cost" table #
##########################################
isolated_repetitions=FALSE
for(k in 1:nrow(report_param))
{
    lower_bound = 50
    upper_bound = 150
    
    # Subset the output
    cases = !logical(nrow(outputs))
    for(p in 1:length(report_div)) 
        cases = (cases & outputs[,report_div[p]] %in% report_param[k,p])
    output = outputs[cases,]
    
    # Calculation
    AUC.tabel = AUC.as.a.function.of.Cost(output,
                                          query_points=lower_bound:upper_bound)
    
    report_dir = file.path(getwd(),"results")
    file_name  = paste0('(','Auc as a function of Cost',')',
                        '(','Intervales of size ',interval_size,')',
                        '(',unique(tolower(output$DATABASE_NAME)),')',
                        '(',unique(toupper(output$model_inducer)),')',
                        '(',unique(tolower(output$cost_function_type)),')',
                        '(',Sys.Date(),')',".csv")
    dir.create(report_dir, show=FALSE, recursive=TRUE)
    write.csv(AUC.tabel, file=file.path(report_dir,file_name), row.names=F)
    head(AUC.tabel)
} # end for AUC as function of Cost


##########################################
# Export "Cost as function of AUC" table #
##########################################
for(k in 1:nrow(report_param))
{
    lower_bound = 50
    upper_bound = 150
    
    # Subset the output
    cases = !logical(nrow(outputs))
    for(p in 1:length(report_div)) 
        cases = (cases & outputs[,report_div[p]] %in% report_param[k,p])
    output = outputs[cases,]
    output = subset(output, cost_intervals<=upper_bound)
    
    key_dic = unique(output[,c("key","payment_selection_criteria")])
    random_value  = key_dic[tolower(substr(key_dic$payment_selection_criteria,1,6)) %in% "random","payment_selection_criteria"]
    random_key    = key_dic[tolower(substr(key_dic$payment_selection_criteria,1,6)) %in% "random","key"]
    random_output = subset(output, key==random_key)
    
    
    # Find the AUC(Cost=50) and max(AUC) of the random rule, create 100 linear 
    # spaced values among them, and query the other methods what is the price to 
    # get the same AUC
    random_AUC_at_50 = subset(random_output, cost_intervals==50, select=average_holdout_cost_performance) 
    random_AUC_max   = max(random_output[,"average_holdout_cost_performance"], na.rm=T)
    query_points     = seq(random_AUC_at_50[[1]], random_AUC_max, length.out=100)
    
    Cost.tabel = Cost.as.a.function.of.AUC(output, query_points)
    Cost.tabel
    
    
    # Find the index for a reference AUC value within random. 
    # there are 2 options:
    # (1) Find the max(AUC) of the random rule, and query the other methods  
    #     what is the price to get the same AUC
    # (2) Take the AUC of the random rule with the max cost (typically 150$)
    
    # Option (1)
    comparison_cost_point_index = which.max(random_output[,"average_holdout_cost_performance"])
    # Option (2)
    # comparison_cost_point_index = which.max(random_output[,"cost_intervals"])
    
    
    random_tuple = random_output[comparison_cost_point_index,c("cost_intervals","average_holdout_cost_performance")]
    Cost.tabel   = Cost.as.a.function.of.AUC(output, query_points=random_tuple[,"average_holdout_cost_performance"])
    Cost.tabel[,random_value] = random_tuple[,"cost_intervals"] # append random real cost value
    Cost.tabel
    
    # Export results to csv file
    report_dir = file.path(getwd(),"results")
    file_name  = paste0('(','Cost as a function of AUC',')',
                        '(','Intervales of size ',interval_size,')',
                        '(',unique(tolower(output$DATABASE_NAME)),')',
                        '(',unique(toupper(output$model_inducer)),')',
                        '(',unique(tolower(output$cost_function_type)),')',
                        '(',Sys.Date(),')',".csv")
    dir.create(report_dir, show=FALSE, recursive=TRUE)
    write.csv(Cost.tabel, file=file.path(report_dir,file_name), row.names=F)
    head(Cost.tabel)
} # end for Cost as a function of AUC
