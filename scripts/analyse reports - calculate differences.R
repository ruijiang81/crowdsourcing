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
interval_size = 5
outputs = interpolate.reports(reports_folder, na.rm=FALSE, interval_size)
head(outputs)


key_dic = unique(outputs[,c("key","payment_selection_criteria")])
key_dic



# key_A - key_B
key_A = 1
key_B = 2
key_A_table = subset(outputs, key==key_A, select = c("cost_intervals","average_holdout_cost_performance"))
key_B_table = subset(outputs, key==key_B, select = c("cost_intervals","average_holdout_cost_performance"))
colnames(key_A_table) = c("cost_intervals","auc")
colnames(key_B_table) = c("cost_intervals","auc")


###################
# Cost difference #
###################
Cost_difference = data.frame()
lower_bound = 40
upper_bound = 100

for(i in 1:min(nrow(key_A_table),nrow(key_B_table)))
{
    ## Check if the cost is within the bounds
    if(key_A_table[i,"cost_intervals"] < lower_bound | key_A_table[i,"cost_intervals"] > upper_bound) next
    ## Calculate the difference
    Cost_difference[i,"cost_intervals"] = key_A_table[i,"cost_intervals"]
    Cost_difference[i,"AUC_difference"] = key_A_table[i,"auc"] - key_B_table[i,"auc"]
} # end for Cost difference

# Remove NAs
Cost_difference = Cost_difference[complete.cases(Cost_difference),]
Cost_difference


###############################
# AUC difference calculations #
###############################
A_table = key_A_table
B_table = key_B_table

auc_query_points = quantile(c(A_table[,"auc"],B_table[,"auc"]),
                            probs=seq(0.5, 1, by=0.1), na.rm=TRUE)

## Step 1: Check that f(x) is monotonic nonincreasing function. 
## If f(x) not monotnic then append the value in index i to index i+1
for(i in 3:nrow(A_table)) 
    if(A_table[i,"auc"]<A_table[i-1,"auc"])
        A_table[i,"auc"] = A_table[i-1,"auc"]
for(i in 3:nrow(B_table)) 
    if(B_table[i,"auc"]<B_table[i-1,"auc"])
        B_table[i,"auc"] = B_table[i-1,"auc"]  
## Step 2: Find f(x)^-1 and query it at the desired auc values via linear 
## interpolation
x_A   = data.matrix(A_table[-1,"auc"])
y_A   = data.matrix(A_table[-1,"cost_intervals"])
app_A = approx(x_A,y_A,auc_query_points) 

x_B   = data.matrix(B_table[-1,"auc"])
y_B   = data.matrix(B_table[-1,"cost_intervals"])
app_B = approx(x_B,y_B,auc_query_points) 
## Step 3: Calculate the sum of differences 
sum(app_A$y-app_B$y, na.rm=TRUE)

