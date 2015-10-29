################################################################################
## Reports Utilities
################################################################################
#' 1. import.reports; Import all the reports from the dest folde
#' 

################################################################################
#' import.reports
#'
import.reports <- function(reports_folder="./reports"){
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
        index_metadata = gregexpr("\\[(.*?)\\]", reports_list[k], TRUE)
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
        
        reports = rbind(reports,report)
    } # end combining reports with metadata
    
    return(reports)
} # import.reports


