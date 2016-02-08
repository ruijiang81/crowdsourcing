################################################################################
## Analyze Reports - Calculate Differences
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
invisible(sapply(list.files(pattern="[.]R$",path="./functions/",full.names=TRUE), source))


################################################################################
## Get the data
################################################################################
reports_folder = file.path(getwd(),"reports")
reports = import.reports(reports_folder,
                         # Remove the "random" rule metadata
                         random.rm=FALSE)
interval_size = 1
outputs = interpolate.reports(reports_folder, na.rm=FALSE, interval_size)
head(outputs)


##########################################
# Export "AUC as function of Cost" table #
##########################################
lower_bound = 50
upper_bound = 150

AUC.tabel = AUC.as.a.function.of.Cost(outputs, query_points=lower_bound:upper_bound)

report_dir = file.path(getwd(),"results")
file_name  = paste0('(','Auc as a function of Cost',')',
                    '(','Intervales of size ',interval_size,')',
                    '(',unique(tolower(outputs$DATABASE_NAME)),')',
                    '(',unique(toupper(outputs$model_inducer)),')',
                    '(',unique(tolower(outputs$cost_function_type)),')',
                    '(',Sys.Date(),')',".csv")
dir.create(report_dir, show=FALSE, recursive=TRUE)
write.csv(AUC.tabel, file=file.path(report_dir,file_name), row.names=F)
head(AUC.tabel)


##########################################
# Export "Cost as function of AUC" table #
##########################################
key_dic = unique(outputs[,c("key","payment_selection_criteria")])
random_value  = key_dic[tolower(substr(key_dic$payment_selection_criteria,1,6)) %in% "random","payment_selection_criteria"]
random_key    = key_dic[tolower(substr(key_dic$payment_selection_criteria,1,6)) %in% "random","key"]
random_output = subset(outputs, key==random_key)


# Find the AUC(Cost=50) and max(AUC) of the random rule, create 100 linear 
# spaced values among them, and query the other methods what is the price to get 
# the same AUC
random_AUC_at_50 = subset(random_output, cost_intervals==50, select=average_holdout_cost_performance) 
random_AUC_max   = max(random_output[,"average_holdout_cost_performance"], na.rm=T)
query_points = seq(random_AUC_at_50[[1]], random_AUC_max, length.out=100)

Cost.tabel = Cost.as.a.function.of.AUC(outputs, query_points)
Cost.tabel


# Find the max(AUC) of the random rule, and query the other methods what is the 
# price to get the same AUC
random_tuple  = random_output[which.max(random_output[,"average_holdout_cost_performance"]),c("cost_intervals","average_holdout_cost_performance")]

Cost.tabel = Cost.as.a.function.of.AUC(outputs, query_points=random_tuple[,"average_holdout_cost_performance"])
Cost.tabel[,random_value] = random_tuple[,"cost_intervals"] # append random real cost value
Cost.tabel

# Export results to csv file
report_dir = file.path(getwd(),"results")
file_name  = paste0('(','Cost as a function of AUC',')',
                    '(','Intervales of size ',interval_size,')',
                    '(',unique(tolower(outputs$DATABASE_NAME)),')',
                    '(',unique(toupper(outputs$model_inducer)),')',
                    '(',unique(tolower(outputs$cost_function_type)),')',
                    '(',Sys.Date(),')',".csv")
dir.create(report_dir, show=FALSE, recursive=TRUE)
write.csv(Cost.tabel, file=file.path(report_dir,file_name), row.names=F)
head(Cost.tabel)
