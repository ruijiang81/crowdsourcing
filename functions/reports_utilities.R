################################################################################
## Reports Utilities
################################################################################
#' 1. create_report; Create report template
#' 2. import.reports; Import all the reports from the dest folde
#' 3. interpolate.reports; Calculate the AUC per cost from the reports
#'


################################################################################
#' create_report
#'
create_report <- function()
{
    col_names  = c("instance_num", "pay", "change", "cost_so_far", "AUC_holdout","full_AUC","subset_AUC") 
    rep_report = read.table(text="", col.names=col_names)
    return(rep_report)
} # end create_report


################################################################################
#' import.reports
#'
import.reports <- function(reports_folder="./reports",
                           # Remove the "random" rule metadata
                           random.rm=FALSE)
{
    ## List the (csv) reports in the folder
    reports_list = list.files(pattern="[.]csv$", path=reports_folder, full.names=TRUE)
    
    
    ## Phrase the reports names
    reports_metadata = data.frame(DATABASE_NAME=NA,
                                  model_inducer=NA,
                                  cost_function_type=NA,
                                  payment_selection_criteria=NA,
                                  Sys_Date=NA)
    for(k in 1:length(reports_list)){
        #' Find the indices of the metadata in the file name.
        #' The metadata is encapsulated between [] (i.e. square parentheses)
        index_metadata = gregexpr("\\((.*?)\\)", reports_list[k], TRUE)
        #' Check that the number of sub string composing the file name is as 
        #' defined in reports_metadata
        stopifnot(ncol(reports_metadata)==length(index_metadata[[1]]))
        ## Extract the sub string to the metadata data frame
        for(l in 1:ncol(reports_metadata)){
            match_start  = index_metadata[[1]][l]
            match_length = attributes(index_metadata[[1]])$match.length[l]
            reports_metadata[k,l] = substr(reports_list[k],
                                           match_start+1,
                                           match_start+match_length-2)
        } # end extracting sub strings
    } # end extracting list_metadata
    
    
    ## Read reports and add meta data
    reports = c()
    for(r in 1:length(reports_list)) 
    {
        report                                = read.csv(reports_list[r])
        report[,"DATABASE_NAME"]              = reports_metadata[r,"DATABASE_NAME"]
        report[,"model_inducer"]              = reports_metadata[r,"model_inducer"]
        report[,"cost_function_type"]         = reports_metadata[r,"cost_function_type"]
        report[,"payment_selection_criteria"] = reports_metadata[r,"payment_selection_criteria"]
        report[,"Sys_Date"]                   = reports_metadata[r,"Sys_Date"]
        report[,"key"]                        = r # Unique key number for each report
        
        reports = rbind(reports,report)
    } # end combining reports with metadata
    
    
    ## Remove "random" rule data
    reports$payment_selection_criteria = tolower(reports$payment_selection_criteria)
    if(random.rm) reports = subset(reports,payment_selection_criteria != "random")
    
    return(reports)
} # import.reports


################################################################################
#' interpolate.reports
#'
interpolate.reports <- function(reports_folder="./reports",
                                na.rm = FALSE){
    
    ## Get the data
    reports = import.reports(reports_folder)
    
    
    ## Test that each rule has the same number of repetitions
    rep_table = matrix(NA, nrow=length(unique(reports$key)), ncol=max(reports$repetition))
    for(k in sort(unique(reports$key))){
        index_rep = t(unique(subset(reports, key==k, select=repetition)))
        rep_table[k,index_rep] = TRUE
    }
    rep_table = rep_table[,unique(reports$repetition)]
    if(any(is.na(rep_table))){
        cat("\ndifferent repetitions detected")
        ans <- readline(prompt="Would you like to ignore missing repetitions? Y/N: ")
        if('n'==tolower(ans))
            stop("different repetitions detected")
        valid_rep = apply(rep_table,2,function(x) !any(is.na(x)))
        valid_rep = unique(reports$repetition)[valid_rep]
        reports   = reports[reports$repetition %in% valid_rep,]
    } else {
        cat("\n found",length(unique(reports$repetition)),"repetitions")
    }
    
    
    ## Calculate Auc(Cost)
    outputs = c()
    
    ## Load each report at a time
    Keys = unique(reports$key)
    for(k in Keys){
        ### Aggregae the data by talking the Mean of the subset_AUC column
        report = subset(reports, key==k)
        report <- tryCatch(
            {
                report = aggregate(subset_AUC ~ ., report, mean)
                report = arrange(report, repetition, instance_num)
            }, 
            error = function(cond){ # for random rule
                report = report
            }
        ) # end trycatch
        
        ### Generate cost table
        interval_size = 1 #usually 1, or 2
        #### Find the minimum model costs among all the intial model costs
        initial_model_costs = aggregate(cost_so_far ~ repetition,data=reports, min)["cost_so_far"]
        min_cost = ceiling(min(initial_model_costs)) #dont start from zero. Start with 0+interval_size (or desired value+interval size)
        #### Find the minimum model costs among all the final model costs
        final_model_costs = aggregate(cost_so_far ~ repetition,data=reports, max)["cost_so_far"]
        max_cost = floor(min(final_model_costs))
        #### Set intervals
        cost_intervals = seq(from = min_cost, to = max_cost, by = interval_size)
        num_cost_intervals = length(cost_intervals)
        
        num_repeations = length(unique(report$repetition))
        
        for (repeation_counter in unique(report$repetition)){
            ## Subset the repetition across all reports
            current_repeatition=subset(report, repetition==repeation_counter)
            num_lines=nrow(current_repeatition)
            
            
            #####generating calculated performance for fixed cost intervals
            interval_cost_performance=numeric()
            
            for (i in 1:num_cost_intervals){
                #print (i)
                line_counter=1
                current_interval_cost=cost_intervals[i]
                
                while ((line_counter<=num_lines)&(current_interval_cost>=current_repeatition$cost_so_far[line_counter])){
                    interval_cost_performance[i]=current_repeatition$AUC_holdout[line_counter]
                    line_counter=line_counter+1
                }
            }
            ###########
            if (repeation_counter==1){
                #contains the performance per cost intervals
                sum_interval_cost_performance = interval_cost_performance 
            } else { 
                # contains the performance per cost intervals
                sum_interval_cost_performance = sum_interval_cost_performance + interval_cost_performance
                
            }
        }
        
        average_holdout_cost_performance=sum_interval_cost_performance/num_repeations  
        output = data.frame(cost_intervals, average_holdout_cost_performance)
        
        ###  Add metadata
        output[,"DATABASE_NAME"]              = report[1,"DATABASE_NAME"]
        output[,"model_inducer"]              = report[1,"model_inducer"]
        output[,"cost_function_type"]         = report[1,"cost_function_type"]
        output[,"payment_selection_criteria"] = report[1,"payment_selection_criteria"]
        output[,"Sys_Date"]                   = report[1,"Sys_Date"]
        output[,"key"]                        = report[1,"key"]
        
        ### Store output
        outputs = rbind(outputs,output)
    } #end keys
    
    ## Remove NA rows
    if(na.rm) outputs = outputs[complete.cases(outputs),]
    
    return(outputs)
} # interpolate.reports
