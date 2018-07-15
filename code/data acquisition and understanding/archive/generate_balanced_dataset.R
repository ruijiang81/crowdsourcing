## Generate Balanced Synthetic Data Set
seed.number <- 2016
set.seed(seed.number)
p <- 10
n <- 20000
# skewness=0   for class 1 only (only FALSE)
# skewness=0.5 for balanced dataset
# skewness=1   for class 2 only (only TRUE)
skewness <- 0.5
### Generate the design matrix
X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = diag(rep(1, p)))
### Generate coefficient vector
beta <- round(20 * runif(p) - 10, 1) # explanatory variables coefficients \in [-10,10]
### Create the dependent variable
y.sd <- sd(X %*% beta)
noise <- 0 * rnorm(n, mean = 0, sd = 1 * y.sd) # Add white noise
### Create the dependent variable
y <- X %*% beta + noise
labels <- ifelse(y < rep(quantile(y, skewness), n), TRUE, FALSE)
### Assamble the variables
dataset <- data.frame(X, y = labels)
dataset <- setVariablesNames(dataset)
# dataset.name <- "Synthetic"

## Sanity Check
# rbind("observations"=table(dataset$y),"proportion"=table(dataset$y)/n)
# hist(X %*% beta + noise)
# abline(v=quantile(y, skewness), col="red", lwd=2)
