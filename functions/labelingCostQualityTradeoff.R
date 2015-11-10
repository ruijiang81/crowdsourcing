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
    method  = tolower(method)
    C_Range = c(0.02,0.25) # define the cost range
    
    
    # Check for null values. If TRUE then set the default
    if (is.null(method))                 method <- 'fix'
    if (is.null(costPerTask))       costPerTask <- 0.1
    if (is.null(fixProbability)) fixProbability <- 0.75        
    
    
    if (method=='fix') {
        p = rep(fixProbability,length(costPerTask))
        
    } else if (method=='concave') {
        C = costPerTask
        #                 p =-0.002*(C*100)^2+0.066*(C*100)+0.37
        p = 0.48+0.066*(100*C)-0.0022*(100*C)^2
        #                 curve(0.48 +0.066*(100*x) + -0.0022*(100*x)^2, from=0.02, to=0.25)
        ## Supply 3 points
        #                 x = c(0.02,0.16,0.25) # Cost
        #                 y = c(0.60,0.95,0.70) # Probability
        ## Fin the coefficients for the polynom
        #                 coeff <- lm(y~poly(x,degree=2,raw=TRUE))$coefficients
        #                 p <- coeff[1] + coeff[1]*C + coeff[2]*C^2
        ## Make sure p \in [0,1], one p at a time
        p <- sapply(p,function(p) max(0,min(p,1)))
        #                 curve(coeff[1] + coeff[1]*x + coeff[2]*x^2, from=0.02, to=0.25)
        
        
    } else if (method=='asymptotic') {
        C = costPerTask
        p = 1-1/(C*100) 
        
          
    } else if (method=='hashtable') {
        C = costPerTask
        colnames(fixProbability) = tolower(colnames(fixProbability))
        p = fixProbability[fixProbability$cost %in% C,"probability"]
        
        
    } else {
        stop('Unknown tradeoff method')
    }    
    return(p)   
} # end function labelingCostQualityTradeoff
