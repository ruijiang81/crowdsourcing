#' labelingCostQualityTradeoff
#' 
#' 

labelingCostQualityTradeoff <- function(method=c('Fix','Concave','Asymptotic'),
                                        costPerLabel=NULL,
                                        fixProbability=NULL) {
    #'Returns the probability for a correct label given payment per label and tradeoff function type ('Fix','Concave','Asymptotic')
    #'
    # Check for null values. If TRUE then set the default
    if (is.null(method))                 method <- 'Fix'
    if (is.null(costPerLabel))       costPerLabel <- 0.1
    if (is.null(fixProbability)) fixProbability <- 0.75        
    # Check arguments validity
    ## Cost
    C = costPerLabel
    C_Range = c(0.02,0.25)
    if (!is.numeric(C) || C<C_Range[1] || C>C_Range[2]) {
        stop('Unvalid cost per label')
    }
    ## Probability
    if (!is.numeric(fixProbability) || fixProbability<=0 || fixProbability>1){
        stop('Probability sholud be in the range (0,1]')
    }
    
    if (method[1]=='Fix') {
        p = rep(fixProbability,length(costPerLabel))               
    } else if (method[1]=='Concave') {
        #                 p =-0.002*(C*100)^2+0.066*(C*100)+0.37
        p = 0.48+0.066*(100*C)-0.0022*(100*C)^2
        ## Make sure p \in [0,1], one p at a time
        p <- sapply(p,function(p) max(0,min(p,1)))
        #                 curve(coeff[1] + coeff[1]*x + coeff[2]*x^2, from=0.02, to=0.25)
        
    } else if (method[1]=='Asymptotic') {
        p=1-1/(C*100) 
    } else {
        stop('Unknown tradeoff method')
    }    
    return(p)   
}
