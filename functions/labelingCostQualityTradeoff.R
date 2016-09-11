#' Labeling cost/quality tradeoff
#' 
#' Initial Suggested Functions for simulating the tradeoff between labeling
#' accuracy (P-probability that a label will correctly classify an instance) 
#' and C (cost – price paid for the instance):
#' 
#' @param method Tradeoff function:
#' 'Fix' returns the same probability for any price. (Mason and Watts, 2009). 
#' 'Concave' produces lower probabilities in the cost range, where the highest
#' probability is somewhere in the middle. (Donghui et al., 2009)
#' 'Asymptotic' the probability increases.
#' 'Quality' the strict probability to assign return, 
#' e.g. costPerTask=0.8 would return probability=0.8.
#' 'HashTable' maps costs to probabilities. 
#' The user should add a dataframe with two columns: "cost" and "probability".
#' The dataframe is passed with the fixProbability input argument.
#' @param costPerTask Price paid to classify an instance.
#' Need to be in the range [0.02,0.25].
#' @return The probability \code{p}.
#' 

labelingCostQualityTradeoff <- function(method=c('Fix','Concave','Asymptotic','HashTable'),
                                        costPerTask=NULL,
                                        fixProbability=NULL){
    stopifnot(length(method)==1)
    method  = tolower(method)
    C_Range = c(0.02,0.25) # define the cost range
    C = costPerTask
    
    
    # Check for null values. If TRUE then set the default
    if (is.null(method))                 method <- 'fix'
    if (is.null(costPerTask))       costPerTask <- 0.1
    if (is.null(fixProbability)) fixProbability <- 0.75        
    
    
    if (method=='fix') {
        p = rep(fixProbability,length(C))
        
    } else if (method=='concave') {
        p = 0.48+0.066*(100*C)-0.0022*(100*C)^2
        
    } else if (method=='asymptotic') {
        p = 1-1/(C*100) 
        
    } else if (method=='f1') {
        # {{0.02,0.75},{0.08,0.93},{0.14,0.94},{0.19,0.95},{0.25,0.75}}
        f1 = function(x) round(0.559388 + 12.1491*x - 145.554*x^2 + 760.25*x^3 - 1440.88*x^4,4)
        p = f1(C)
        
    } else if (method=='f2') {
        # {{0.02,0.82},{0.25,0.88}}
        f2 = function(x) round(0.814783 + 0.26087*x,4)
        p = f2(C)
        
    } else if (method=='f3') {
        # {{0.02,0.75},{0.04,0.80},{0.08,0.97},{0.22,0.94},{0.25,0.80}}
        f3 = function(x) round(0.756717 - 2.24434*x + 108.603*x^2 - 681.85*x^3 + 1144.47*x^4,4)
        p = f3(C)
        
    } else if (method=='f4') {
        # {{0.02,0.65},{0.25,0.95}}
        f4 = function(x) round(0.625 + 1.3*x,4)
        p = f4(C)
        
    } else { # Hashtable
        colnames(fixProbability) = tolower(colnames(fixProbability))
        p = fixProbability[fixProbability$cost %in% C,"probability"]
    }     
    # stop('Unknown tradeoff method')
    
    
    return(p)   
} # end function labelingCostQualityTradeoff
