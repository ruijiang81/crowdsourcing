################################################################################
## Analyze Reports - Calculate Differences
################################################################################
## Initialization
cat("\014")
rm(list = ls())
source("scripts/load_libraries.R")
invisible(sapply(list.files(pattern = "[.]R$", path = "./functions/", full.names = TRUE), source))


################
# Get the data #
################
reports_folder <- file.path(getwd(), "reports")
reports <- import.reports(reports_folder,
  # Remove the "random" rule metadata
  random.rm = FALSE
)
lower_bound <- 50
upper_bound <- 150
interval_size <- 1
isolated_repetitions <- FALSE
outputs <- interpolate.reports(reports_folder,
  na.rm = FALSE,
  interval_size
)
# Find the index for a reference AUC value within the random rule.
# there are 2 options:
# (1) Find the max(AUC) of the random rule, and query the other methods
#     what is the price to get the same AUC
# (2) Take the AUC of the random rule with the max cost (typically 150$)
AUC_REF_OPTION <- 1 # 1 or 2
# In case of option 2, what is the max cost?
AUC_MAX_COST <- 150 # Typically 150, if multiple labeling was applied, then enter 300


#####################
# Aggregate outputs #
#####################
col_names <- colnames(outputs)
if (isolated_repetitions == FALSE) {
  outputs$repetition <- 0
  outputs <- aggregate(
    average_holdout_cost_performance ~ . - cost_intervals,
    outputs,
    function(x) mean(x, na.rm = T)
  )
  outputs <- outputs[col_names]
} # end isolated_repetitions
head(outputs)


###############################
# Create different reports by #
###############################
report_div <- c("DATABASE_NAME", "model_inducer", "cost_function_type")
report_param <- unique(outputs[, report_div])


##########################################
# Export "AUC as function of Cost" table #
##########################################
for (k in 1:nrow(report_param))
{
  # Subset the output
  cases <- !logical(nrow(outputs))
  for (p in 1:length(report_div))
    cases <- (cases & outputs[, report_div[p]] %in% report_param[k, p])
  output <- outputs[cases, ]

  # Calculation
  AUC.tabel <- AUC.as.a.function.of.Cost(output,
    query_points = lower_bound:upper_bound
  )

  report_dir <- file.path(getwd(), "results")
  file_name <- paste0(
    "(", "Auc as a function of Cost", ")",
    "(", "Intervales of size ", interval_size, ")",
    "(", unique(tolower(output$DATABASE_NAME)), ")",
    "(", unique(toupper(output$model_inducer)), ")",
    "(", unique(tolower(output$cost_function_type)), ")",
    "(", Sys.Date(), ")", ".csv"
  )
  dir.create(report_dir, show = FALSE, recursive = TRUE)
  write.csv(AUC.tabel, file = file.path(report_dir, file_name), row.names = F)
  head(AUC.tabel)
} # end for AUC as function of Cost


##########################################
# Export "Cost as function of AUC" table #
##########################################
params <- unique(outputs[, c("DATABASE_NAME", "model_inducer", "cost_function_type", "payment_selection_criteria", "repetition")])
params <- arrange(params, DATABASE_NAME, model_inducer, cost_function_type, repetition, payment_selection_criteria)
params$AUC <- NA
params$Cost <- NA

# Find reference points, that is, for each "random" rule find a tuple {cost,auc}
# to query the other rules within the same category
params$random_flag <- FALSE
for (p in 1:nrow(params))
{
  ## Find which rule is "random"
  if (tolower(substr(params[p, "payment_selection_criteria"], 1, 6)) == "random") {
    params[p, "random_flag"] <- TRUE
  }
  ## Find the reference point
  if (params[p, "random_flag"]) { # "random" rule

    x <- subset_params(data = outputs, colnames = colnames(params)[1:5], values = params[p, 1:5])[["cost_intervals"]]
    y <- subset_params(data = outputs, colnames = colnames(params)[1:5], values = params[p, 1:5])[["average_holdout_cost_performance"]]

    # Find the index for a reference AUC value within the random rule.
    # there are 2 options:
    # (1) Find the max(AUC) of the random rule, and query the other methods
    #     what is the price to get the same AUC
    # (2) Take the AUC of the random rule with the max cost (typically 150$)

    if (AUC_REF_OPTION == 1) {
      # Option (1)
      params[p, "AUC"] <- max(y, na.rm = TRUE)
      params[p, "Cost"] <- x[y %in% params[p, "AUC"]][1]
    } else if (AUC_REF_OPTION == 2) {
      # Option (2)
      params[p, "Cost"] <- AUC_MAX_COST
      params[p, "AUC"] <- y[x %in% 300]
    }
  } # end if "random" rule
} # find reference points

# Query the other rules within the same category
for (p in 1:nrow(params)) {
  if (!params[p, "random_flag"]) { # NOT "random" rule
    ## Find the reference AUC value
    data_ref <- subset_params(
      data = params,
      colnames = colnames(params)[c(1:3, 5)],
      values = params[p, c(1:3, 5)]
    )
    AUC_ref <- data_ref[data_ref$random_flag, "AUC"]
    ## Find the corresponding value for that reference
    x <- subset_params(data = outputs, colnames = colnames(params)[1:5], values = params[p, 1:5])[["cost_intervals"]]
    y <- subset_params(data = outputs, colnames = colnames(params)[1:5], values = params[p, 1:5])[["average_holdout_cost_performance"]]
    ## Check that f(x) is monotonic nonincreasing function.
    ## If f(x) not monotnic then append the value in index i to index i+1
    for (i in 2:length(x))
      if (!any(is.na(y[(i - 1):i])) & y[i] < y[i - 1]) {
        y[i] <- y[i - 1]
      }
    ## Find f(x)^-1 and query it at the desired auc values via linear
    ## interpolation
    candidates <- c()
    for (k in 2:length(x)) {
      if (any(is.na(y[(k - 1):k]))) {
        next
      }
      if (y[k - 1] <= AUC_ref & y[k] >= AUC_ref) {
        candidates <- c(candidates, x[k])
      }
      if (y[k - 1] >= AUC_ref & y[k] <= AUC_ref) {
        candidates <- c(candidates, x[k])
      }
    } # end for
    if (is.null(candidates)) { # In case "random" is better then the other policy
      params[p, "Cost"] <- NA
      params[p, "AUC"] <- AUC_ref
    } else {
      params[p, "Cost"] <- max(candidates)
      params[p, "AUC"] <- AUC_ref
    } # end if random is better
  } # end if not "random" rule
} # find corresponding points

# Store the results
## Make the data frame wide
library(tidyr)
results_wide <- spread(params[, -8],
  key = payment_selection_criteria,
  value = Cost
)
results_wide <- dplyr::arrange(results_wide, DATABASE_NAME, model_inducer, cost_function_type)
## Save on hard disk
report_dir <- file.path(getwd(), "results")
file_name <- paste0(
  "(", "Cost as a function of AUC", ")",
  "(", Sys.Date(), ")", ".csv"
)
dir.create(report_dir, show = FALSE, recursive = TRUE)
write.csv(results_wide, file = file.path(report_dir, file_name), row.names = F)
