################################################################################
## Reports Utilities
################################################################################
#' 1. create_report; Create report template
#' 2. import.reports; Import all the reports from the dest folde
#' 3. interpolate.reports; Calculate the AUC per cost from the reports
#' 4. export.interval.table;
#' 


#################
# create_report #
#################
create_report <- function()
{
    col_names  = c("instance_num", "pay", "change", "cost_so_far", "AUC_holdout","full_AUC","subset_AUC") 
    rep_report = read.table(text="", col.names=col_names)
    return(rep_report)
} # end create_report


##################
# import.reports #
##################
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


#######################
# interpolate.reports #
#######################
interpolate.reports <- function(reports_folder="./reports",
                                na.rm=FALSE,
                                interval_size=1){
    ################
    # Get the data #
    ################
    reports = import.reports(reports_folder)
    
    
    ##########################################################
    # Test that each rule has the same number of repetitions #
    ##########################################################
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
    
    
    #########################################################
    # Test that each repetition has the same max model cost #
    #########################################################
    # Remove the min_payment instances index
    select_indicator = !(substr(reports$payment_selection_criteria, 1, 6) %in% c("min_pa"))
    # Find the model cost for each run
    max_model_cost = aggregate(cost_so_far ~ repetition + payment_selection_criteria, 
                               reports[select_indicator,], max)
    min_max_model_cost_index = which.min(max_model_cost$cost_so_far)[1]    
    min_max_model_cost_value = min(max_model_cost$cost_so_far)[1]    
    cat("\n  Maximal Minimum Model Cost is ", min_max_model_cost_value, "$", sep="")
    # Trim the max model cost
    cutoff_offset = 10*0.25 # since if there is no problem the difference between the most and least value can be 2.5$ we add an offset
    reports = subset(reports, cost_so_far<=(min_max_model_cost_value + cutoff_offset))    
    
    
    ########################
    ## Calculate Auc(Cost) #
    ########################
    outputs = c()
    # Load each report at a time
    Keys = unique(reports$key)
    for(k in Keys){
        ## Aggregae the data by taking the Mean of the average_holdout_cost_performance column
        report = subset(reports, key==k)
        report <- tryCatch(
            {
                report = aggregate(average_holdout_cost_performance  ~ . -subset_AUC,
                                   report,
                                   function(x) mean(x, na.rm=T))
                report = arrange(report, repetition, instance_num)
            }, 
            error = function(cond){ # for random rule
                report = report
            }
        ) # end trycatch
        
        ### Generate cost table
        #### Find the minimum model costs among all the intial model costs
        initial_model_costs = aggregate(cost_so_far ~ repetition,data=reports,
                                        function(x) min(x, na.rm=T))["cost_so_far"]
        min_cost = ceiling(min(initial_model_costs)) #dont start from zero. Start with 0+interval_size (or desired value+interval size)
        #### Find the minimum model costs among all the final model costs
        final_model_costs = aggregate(cost_so_far ~ repetition,data=reports,
                                      function(x) max(x, na.rm=T))["cost_so_far"]
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
        
        average_holdout_cost_performance = sum_interval_cost_performance/num_repeations  
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


#############################
# AUC.as.a.function.of.Cost #
#############################
#' @param outputs the product of interpolate.reports()
#' @query_points the cost values
#' 
AUC.as.a.function.of.Cost <- function(outputs, query_points=NA)
{
    key_dic = unique(outputs[,c("key","payment_selection_criteria")])
    
    results = data.frame(cost_intervals=unique(outputs$cost_intervals))
    for(k in key_dic$key){
        key_value = unique(key_dic[key_dic$key %in% k, "payment_selection_criteria"])
        output = subset(outputs, key==k, select = c("cost_intervals","average_holdout_cost_performance"))
        results[results$cost_intervals %in% output$cost_intervals,key_value] = output$average_holdout_cost_performance
    }
    # > head(results)
    # cost_intervals max_ratio100   max_ratio1e+06     max_total_ratio100   random100
    # 1              NA             NA                 NA                   NA
    # 2              NA             NA                 NA                   NA
    # 3              0.7944134      0.7944134          0.7944134            0.7944134
    # 4              0.8074540      0.8074540          0.8074540            0.8074540
    # 5              0.8254625      0.8254625          0.8254625            0.8254625
    # 6              0.8579312      0.8579312          0.8579312            0.8579312
    
    
    # Subset the table
    if(!is.na(query_points[1])) results = subset(results, cost_intervals %in% query_points)
    
    
    return(results)
} # end AUC.as.a.function.of.Cost


#############################
# Cost.as.a.function.of.AUC #
#############################
#' @param outputs the product of interpolate.reports()
#' @query_points the AUC values
#' 
Cost.as.a.function.of.AUC <- function(outputs, query_points){
    key_dic = unique(outputs[,c("key","payment_selection_criteria")])
    results = data.frame("auc"=query_points)
    
    for(k in unique(key_dic$key))
    {
        key_value = unique(key_dic[key_dic$key %in% k, "payment_selection_criteria"])
        key_table = subset(outputs, key %in% k, select=c("cost_intervals","average_holdout_cost_performance"))
        colnames(key_table) = c("cost_intervals","auc")
        
        
        ## Step 1: Replace NA in index i with the value at index i+1
        #         for(i in nrow(key_table):2)
        #             if(is.na(key_table[i-1,"auc"]))
        #                 key_table[i-1,"auc"] = key_table[i,"auc"]
        
        ## Step 2: Check that f(x) is monotonic nonincreasing function. 
        ## If f(x) not monotnic then append the value in index i to index i+1
        for(i in 2:nrow(key_table)) 
            if(!any(is.na(key_table[(i-1):i,"auc"])) & key_table[i,"auc"]<key_table[i-1,"auc"])
                key_table[i,"auc"] = key_table[i-1,"auc"]
        
        ## Step 3: Find f(x)^-1 and query it at the desired auc values via linear 
        ## interpolation
        x   = data.matrix(key_table[-1,"auc"])
        y   = data.matrix(key_table[-1,"cost_intervals"])
        app = approx(x, y, query_points) 
        
        ## Step 4: Store query points
        results[results$auc %in% app$x, key_value] = app$y
    } # end for loop
    
    
    return(results)
} # end Cost.as.a.function.of.AUC

