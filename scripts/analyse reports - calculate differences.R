################################################################################
## Analyze Reports - Calculate Differences
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$",path="./functions/",full.names=TRUE), source)


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


#########################
# Export interval tabel #
#########################
lower_bound = 50
upper_bound = 150

interval.tabel = export.interval.table(outputs)
interval.tabel = subset(interval.tabel, cost_intervals>=lower_bound & cost_intervals<=upper_bound)

report_dir = file.path(getwd(),"results")
file_name  = paste0('(','Intervales of size ',interval_size,')',
                    '(',unique(tolower(outputs$DATABASE_NAME)),')',
                    '(',unique(toupper(outputs$model_inducer)),')',
                    '(',unique(tolower(outputs$cost_function_type)),')',
                    '(',Sys.Date(),')',".csv")
dir.create(report_dir, show=FALSE, recursive=TRUE)
write.csv(interval.tabel, file=file.path(report_dir,file_name), row.names=F)
