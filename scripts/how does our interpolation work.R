################################################################################
## How does our interpolation work?
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
# outputs = interpolate.reports(reports_folder,
#                               na.rm=FALSE,
#                               interval_size=1,
#                               isolated_repetitions=FALSE)
# head(outputs)
params = unique(reports[,c("DATABASE_NAME","model_inducer","cost_function_type",
                           "payment_selection_criteria","repetition")])
# results = data.frame()
# for(s in 1:nrow(params)){
s=1
#################
# Subset report #
#################
chosen_DATABASE_NAME              = params[s,"DATABASE_NAME"]
chosen_model_inducer              = params[s,"model_inducer"]
chosen_cost_function_type         = params[s,"cost_function_type"]
chosen_payment_selection_criteria = params[s,"payment_selection_criteria"]
chosen_repetition                 = params[s,"repetition"]

report = subset(reports, DATABASE_NAME==chosen_DATABASE_NAME & 
                    model_inducer==chosen_model_inducer & 
                    cost_function_type==chosen_cost_function_type & 
                    payment_selection_criteria==chosen_payment_selection_criteria &
                    repetition==chosen_repetition,
                select=c("cost_so_far","AUC_holdout"))
head(report)



x = report$cost_so_far
y = report$AUC_holdout

return_list = interpolation.kernel.customized(x=x ,y=y, xout=50:150)
xout = return_list[["xout"]]
yout = return_list[["yout"]]


plot(x,y,type="o",xlim=range(xout),ylim=range(yout))
abline(v=xout,lty=2)
lines(xout,yout,col=2,type="s")
