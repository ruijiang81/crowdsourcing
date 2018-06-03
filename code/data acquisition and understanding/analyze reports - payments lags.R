################################################################################
## Analyze Reports - Payments Lags
################################################################################
#' Check how many iteration have past between payment of the same value
#' 
 
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


################
# Analyze data #
################
param = unique(reports[,c("key","repetition")])
results = data.frame()
for (p in 1:nrow(param)){
    # 1. Subset the data
    subReport = subset(reports, 
                       key %in% param[p,"key"] & repetition %in% param[p,"repetition"],
                       select = c("pay","batch"))
    subReport = unique(subReport)
    # 2. Initial counter
    lagCounter = as.data.frame(matrix(0, ncol=length(unique(reports[,"pay"]))))
    lagDF = as.data.frame(matrix(NA, ncol=length(unique(reports[,"pay"])), nrow=max(subReport$batch)+1))
    colnames(lagDF) = colnames(lagCounter) = sort(unique(reports[,"pay"]))
    
    # 3. Count lags (not including the random rounds 1 to 30)
    for(l in 31:max(subReport$batch)){
        lagCounter = lagCounter + c(1,1,1)
        currentPay = subReport[l,"pay"]
        currentCol = colnames(lagCounter) %in% currentPay
        lagDF[l,currentCol] = lagCounter[, currentCol]
        lagCounter[, currentCol] = 0
    }
    
    # 4. Save the last state
    lagDF[l+1,!currentCol] = lagCounter[, !currentCol]
    
    # 5. Store results
    results = rbind(results, cbind(key=param[p,"key"],
                                   repetition=param[p,"repetition"],
                                   lagDF))
}# end for loop


resultsLong = reshape2::melt(results[,3:5], value.name="Count", variable.name="Payment")
resultsLong = resultsLong[complete.cases(resultsLong),]
# remove lags of 1 and subtract 1
resultsLong = resultsLong[resultsLong$Count>1,]
resultsLong[,"Count"] = resultsLong[,"Count"] - 1

barplot(table(resultsLong$Payment,resultsLong$Count), 
        main="Stacked Bar Plot", col=c("darkblue","red","green"),
        legend = unique(resultsLong$Payment))

barplot(table(resultsLong$Payment,resultsLong$Count), 
        main="Grouped Bar Plot", col=c("darkblue","red","green"),
        legend = unique(resultsLong$Payment), beside=TRUE)

