################################################################################
## Cost-Quality tradoff functions
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("functions/labelingCostQualityTradeoff.R")
price_per_label_values = c(0.02,0.08,0.14,0.19,0.25)
trade_offs = c("Fix","Concave","Asymptotic","F1","F2")

par(mfrow=c(2,3))

for(t in trade_offs){
    ## Calculate probabilities
    probabilities = labelingCostQualityTradeoff(t, price_per_label_values, fixProbability = 0.85)
    probabilities = round(probabilities,2)
    ## Plot trade-off function
    plot(price_per_label_values, probabilities, type="b", ylim=c(0.5,1), xlim=c(0.02,0.25), axes = F, main=paste(t,"trade-off function"))
    text(price_per_label_values,probabilities,probabilities,pos=1)
    axis(1, at=price_per_label_values); axis(2, at=seq(0.5,1,by=0.1), las=2); box()
}
