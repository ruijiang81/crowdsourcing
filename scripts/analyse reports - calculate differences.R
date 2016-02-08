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
# Export "AUC as function of Cost" tabel #
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
indicator_of_the_random_key = tolower(substr(outputs$payment_selection_criteria,1,6)) %in% "random"

randomRule_range = range(outputs[indicator_of_the_random_key,"average_holdout_cost_performance"], na.rm=T)
query_points = seq(randomRule_range[1], randomRule_range[2], length.out=100)

Cost.tabel = Cost.as.a.function.of.AUC(outputs, query_points)

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
