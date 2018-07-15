################################################################################
## Analyze Reports - Payments System of Dual Cost Functions
################################################################################
#' Check how different payments had been used after the 75$ threshold
#' 1. What payments were payed  after threshold?
#' 2. How many iterations (after the threshold) took to go across all payments?
#'

## Initialization
cat("\014")
rm(list = ls())
source("scripts/load_libraries.R")
invisible(sapply(list.files(pattern = "[.]R$", path = "./functions/", full.names = TRUE), source))
threshold <- 75
################
# Get the data #
################
reports_folder <- file.path(getwd(), "reports")
reports <- import.reports(reports_folder,
  # Remove the "random" rule metadata
  random.rm = FALSE
)


################
# Analyze data #
################
param <- unique(reports[, c("key", "repetition")])
param$batchTime <- NA
param$howManyPayments <- NA
for (p in 1:nrow(param)) {
  # 1. Subset the data
  subReport <- subset(reports,
    key %in% param[p, "key"] & repetition %in% param[p, "repetition"] & cost_so_far >= threshold,
    select = c("pay", "batch", "cost_so_far")
  )
  subReport <- unique(subReport)

  # 2. Initial counter
  paymentsIndicator <- matrix(FALSE, ncol = length(unique(reports[, "pay"])), nrow = nrow(subReport))
  paymentsIndicator <- as.data.frame(paymentsIndicator)
  colnames(paymentsIndicator) <- sort(unique(reports[, "pay"]))
  rownames(paymentsIndicator) <- subReport$batch

  # 3. Check when each payment appears
  for (l in 1:nrow(paymentsIndicator))
    paymentsIndicator[l:nrow(paymentsIndicator), colnames(paymentsIndicator) %in% subReport[l, "pay"]] <- TRUE

  # 5. Store results
  param[p, "batchTime"] <- which.max(rowSums(paymentsIndicator))
  param[p, "howManyPayments"] <- max(rowSums(paymentsIndicator))
} # end for loop


counts <- param[param$howManyPayments %in% length(unique(reports[, "pay"])), "batchTime"]
counts <- c(counts, rep(0, sum(!(param$howManyPayments %in% length(unique(reports[, "pay"]))))))

# Cumulative barplot
barplot(c(table(counts)[1], cumsum(table(counts)[-1])),
  main = paste("Cumulative barplot: How many iterations after the total cost was", paste0(threshold, "$"), "passed until all payments were chosen?\n
        (0 indicates how many times it never happened)"),
  ylab = "Cumulative Counts"
)

# Basic barplot
xx <- barplot(table(counts),
  ylim = range(table(counts)) * 1.1,
  main = paste("How many iterations after the total cost was", paste0(threshold, "$"), "passed until all payments were chosen?\n 
        (0 indicates how many times it never happened)"),
  ylab = "Frequency"
)
## Add text at top of bars
text(x = xx, y = table(counts), label = table(counts), pos = 3, cex = 0.8, col = "red")
