################################################################################
## Analyze Reports - Compare Curves
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

lower_bound <- 40
upper_bound <- 150


params <- unique(reports[, c(
  "DATABASE_NAME", "model_inducer", "cost_function_type",
  "payment_selection_criteria", "repetition"
)])
results <- data.frame()
for (s in 1:nrow(params)) {
  cat("\n#", "Integrating curve", s, "/", nrow(params))
  #################
  # Subset report #
  #################
  chosen_DATABASE_NAME <- params[s, "DATABASE_NAME"]
  chosen_model_inducer <- params[s, "model_inducer"]
  chosen_cost_function_type <- params[s, "cost_function_type"]
  chosen_payment_selection_criteria <- params[s, "payment_selection_criteria"]
  chosen_repetition <- params[s, "repetition"]

  report <- subset(reports, DATABASE_NAME == chosen_DATABASE_NAME &
    model_inducer == chosen_model_inducer &
    cost_function_type == chosen_cost_function_type &
    payment_selection_criteria == chosen_payment_selection_criteria &
    repetition == chosen_repetition,
  select = c("cost_so_far", "AUC_holdout")
  )


  ######################
  # Interpolate report #
  ######################
  x <- report[, "cost_so_far"]
  y <- report[, "AUC_holdout"]
  xout <- seq(lower_bound, upper_bound, by = 1)
  # Linear Interpolation
  yout <- approx(x, y, xout)[["y"]]
  # Customized Interpolation
  yout <- interpolation.kernel.customized(x, y, xout)[["yout"]]


  ###################
  # Integrate curve #
  ###################
  Area <- AUC_by_curve_integration(
    x = xout, y = yout,
    xmin = lower_bound, xmax = upper_bound,
    ymin = 0, ymax = 1
  )


  #################
  # Store results #
  #################
  results <- rbind(
    results,
    data.frame(
      DATABASE_NAME = chosen_DATABASE_NAME,
      model_inducer = chosen_model_inducer,
      cost_function_type = chosen_cost_function_type,
      payment_selection_criteria = chosen_payment_selection_criteria,
      repetition = chosen_repetition,
      curve_integration = Area
    )
  )
} # end for params
results_long <- results


#################
# Visualisation #
#################
# library(ggplot2)
# ggplot(results, aes(curve_integration, colour = payment_selection_criteria)) +
#     geom_density() + ggtitle("")


##############################
# Export results to csv file #
##############################
report_dir <- file.path(getwd(), "results")
## Long data frame
file_name <- paste0(
  "(", "Comparing Curves", ")",
  "(", "long format", ")",
  "(", Sys.Date(), ")", ".csv"
)
dir.create(report_dir, show = FALSE, recursive = TRUE)
write.csv(results_long, file = file.path(report_dir, file_name), row.names = F)
## Wide data frame
library(tidyr)
results_wide <- spread(results,
  key = payment_selection_criteria,
  value = curve_integration
)
file_name <- paste0(
  "(", "Comparing Curves", ")",
  "(", "wide format", ")",
  "(", Sys.Date(), ")", ".csv"
)
dir.create(report_dir, show = FALSE, recursive = TRUE)
write.csv(results_wide, file = file.path(report_dir, file_name), row.names = F)
