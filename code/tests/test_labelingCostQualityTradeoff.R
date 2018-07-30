################################################################################
## Unit testing for labelingCostQualityTradeoff
################################################################################
## 1. Initialization
source("scripts/load_libraries.R")
sapply(list.files(pattern = "[.]R$", path = "./functions/", full.names = TRUE), source)
set.seed(2015)

costs <- c(0.02, 0.08, 0.14, 0.19, 0.25)
## method = c('Fix','Concave','Asymptotic','HashTable')
################################################################################
## Fix Method
labelingCostQualityTradeoff(
    method = "Fix",
    costPerTask = costs,
    fixProbability = 0.75
)
## Concave Method
labelingCostQualityTradeoff(
    method = "Concave",
    costPerTask = costs
)
## Asymptotic Method
labelingCostQualityTradeoff(
    method = "Asymptotic",
    costPerTask = costs
)
## HashTable Method
labelingCostQualityTradeoff(
    method = "HashTable",
    costPerTask = 2 * costs,
    fixProbability = data.frame(
        cost = 2 * costs,
        probability = c(0.6, 0.87, 0.97, 0.94, 0.76)
    )
)
