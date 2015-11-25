################################################################################
## Cost-Quality tradoff functions
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("functions/labelingCostQualityTradeoff.R")
price_per_label_values = c(0.02,0.08,0.14,0.19,0.25)
par(mfrow=c(2,3))


## Fix 
probabilities = labelingCostQualityTradeoff("Fix", price_per_label_values, fixProbability = 0.85)
plot(price_per_label_values, probabilities, type="b", ylim=c(0.5,1), xlim=c(0.02,0.25), axes = F, main="Fix Tradoff")
text(price_per_label_values,probabilities,probabilities,pos=1)
axis(1, at=price_per_label_values); axis(2, at=seq(0.5,1,by=0.1), las=2); box()


## Concave
probabilities = labelingCostQualityTradeoff("Concave", price_per_label_values)
probabilities = round(probabilities,2)
plot(price_per_label_values, probabilities, type="b", ylim=c(0.5,1), xlim=c(0.02,0.25), axes = F, main="Concave Tradoff")
text(price_per_label_values,probabilities,probabilities,pos=1)
axis(1, at=price_per_label_values); axis(2, at=seq(0.5,1,by=0.1), las=2); box()


## Asymptotic 
probabilities = labelingCostQualityTradeoff("Asymptotic", price_per_label_values)
probabilities = round(probabilities,2)
plot(price_per_label_values, probabilities, type="b", ylim=c(0.5,1), xlim=c(0.02,0.25), axes = F, main="Asymptotic Tradoff")
text(price_per_label_values,probabilities,probabilities,pos=1)
axis(1, at=price_per_label_values); axis(2, at=seq(0.5,1,by=0.1), las=2); box()


## F1
probabilities = c(0.75,0.93,0.94,0.95,0.75)
plot(price_per_label_values, probabilities, type="b", ylim=c(0.5,1), xlim=c(0.02,0.25), axes = F, main="F1 Tradoff")
text(price_per_label_values,probabilities,probabilities,pos=1)
axis(1, at=price_per_label_values); axis(2, at=seq(0.5,1,by=0.1), las=2); box()


## F2
probabilities = c(0.6,0.87,0.97,0.94,0.76)
plot(price_per_label_values, probabilities, type="b", ylim=c(0.5,1), xlim=c(0.02,0.25), axes = F, main="F2 Tradoff")
text(price_per_label_values,probabilities,probabilities,pos=1)
axis(1, at=price_per_label_values); axis(2, at=seq(0.5,1,by=0.1), las=2); box()

