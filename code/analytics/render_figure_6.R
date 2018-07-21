################################################################################
#                               Render figure 6                                #
################################################################################
#'
#' | ------------------- | ------------------- | ------------------- |
#' | (1,1)               | (1,2)               | (1,3)               |
#' | (a) Mushroom,       | (b) Mushroom,       | (c) Mushroom,       |
#' | concave tradeoff    | asymptotic tradeoff | fixed tradeoff      |
#' | ------------------- | ------------------- | ------------------- |
#' | (2,1)               | (2,2)               | (2,3)               |
#' | (a) Spam,           | (b) Spam,           | (c) Spam,           |
#' | concave tradeoff    | asymptotic tradeoff | fixed tradeoff      |
#' | ------------------  | ------------------- | ------------------- |
#' | (3,1)               | (3,2)               | (3,3)               |
#' | (a) Pen Digits,     | (b) Pen Digits,     | (c) Pen Digits,     |
#' | concave tradeoff    | asymptotic tradeoff | fixed tradeoff      |
#' | ------------------- | ------------------- | ------------------- |
#'
#########
# Setup #
#########
source(file.path(getwd(), "code", "scripts", "setup.R"))
cat_80("Render figure 6")
#'
##########################
# Configure x and y axes #
##########################
x_axis <- "cost_so_far"
y_axis <- c("AUC_holdout", "train_set_label_quality")[1]
#'
################
# Get the data #
################
input_folder <- file.path(k_path_results, "processed")
input_file <- file.path(input_folder, "metadata-by-dollar.csv")
dataset <- read.csv(input_file) %>% as.data.frame()
#'
#####################
# Quality assurance #
#####################
#' Test that expected column names exist in the dataset
assertive::assert_is_subset(
    c(x_axis, y_axis, "database_name", "cost_function_type", "payment_selection_criteria"),
    colnames(dataset)
)
#' Test that all expected information exist in the dataset
database_name_levles <- c("mushroom", "spam", "pen digits")
cost_function_type_levles <- c("concave", "asymptotic", "fix")
expected_combinations <-
    expand.grid(
        database_name = database_name_levles,
        cost_function_type = cost_function_type_levles
    )
dataset_combinations <-
    dataset %>%
    select(database_name, cost_function_type) %>%
    unique()
assertive::assert_are_set_equal(expected_combinations, dataset_combinations)
#' Test that the selected columns have no NA
assertive::assert_any_are_not_na(dataset %>% select(x_axis, y_axis))
#'
#################
# Preprocessing #
#################
dataset <-
    dataset %>%
    mutate_(
        x_axis = paste0("as.numeric(", x_axis, ")"),
        y_axis = y_axis
    ) %>%
    mutate(
        database_name = factor(database_name, levels = database_name_levles),
        cost_function_type = factor(cost_function_type, levels = cost_function_type_levles)
    )
#'
###################
# Create figure 6 #
###################
plot_data <-
    dataset %>%
    group_by(database_name, cost_function_type, payment_selection_criteria, x_axis) %>%
    summarise(y_axis = mean(y_axis))
fig6 <- plot_data %>%
    ggplot(aes(x = x_axis, y = y_axis, color = payment_selection_criteria)) +
    geom_line(size = rel(1)) +
    scale_x_continuous(x_axis, breaks = seq(0, 300, 10)) +
    scale_y_continuous(y_axis, limits = c(NA, NA), expand = c(0, 0)) +
    theme_bw() +
    theme(
        aspect.ratio = 1,
        axis.text.x = element_text(angle = 90, hjust = 1)
    ) +
    facet_grid(database_name ~ cost_function_type, scales = "free")
plot(fig6)
#'
###############
# Save figure #
###############
output_folder <- file.path(k_path_results, "figures")
output_file <- file.path(output_folder, "(fig6)" %()% x_axis %()% y_axis %+% ".png")
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

ggsave(
    file = output_file,
    plot = fig6,
    # width=8.3, height=11.7) # A4 size
    width = 8.3, height = 8.3
)
#'
cat("\nCompleted")
