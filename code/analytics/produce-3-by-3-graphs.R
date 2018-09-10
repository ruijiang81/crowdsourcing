################################################################################
## Analyze Reports - Produce Graphs
################################################################################
## Initialization
source(file.path(getwd(), "code", "scripts", "setup.R"))
cat_80("Metadata Pro Processing")
#'
################################################################################
## Preprocess the data (under ../results/)
################################################################################
source(file.path(k_path_analytics, "processing-the-report-files.R"))
source(file.path(k_path_analytics, "processing-the-metadata-files.R"))
source(file.path(k_path_analytics, "processing-the-ledger-files.R"))
#'
################
# Get the data #
################
policies_attributes <- read_csv(file.path(k_path_dictionaries, "policies-attributes.csv"))
data_report <- read_csv(file.path(k_path_results, "processed", "reports-by-dollar.csv"))
data_metadata <- read_csv(file.path(k_path_results, "processed", "metadata-by-dollar.csv"))
data_ledger <- read_csv(file.path(k_path_results, "processed", "ledgers-by-dollar.csv"))
data <- data_report
#'
###############################
# Configure ggplot attributes #
###############################
## How many rows and columns are in the ggpolt grid?
mfrow <- 3
mfcol <- 3 
## What is the text and line sizes?
text_size <- rel(0.8)
line_size <- rel(0.8)
#'
################################################################################
## Visualization
################################################################################
pairs <- data %>% select(database_name, cost_function_type) %>% unique()
for(k in seq_along(pairs)){
    ###############################
    # Prepare the data for ggplot #
    ###############################
    # Subset the data
    ggplot_data <- 
        data %>% 
        filter(database_name %in% pairs[k, "database_name"],
               cost_function_type %in% pairs[k, "cost_function_type"])
    # Aggregate the data
    ggplot_data <-
        ggplot_data %>%
        group_by(payment_selection_criteria, cost_so_far) %>%
        summarise(AUC_holdout = mean(AUC_holdout))
    # Add metadata to the data
    ggplot_data <- left_join(ggplot_data, policies_attributes,
                             by = "payment_selection_criteria")
    # Data quality assurance
    assert_all_are_not_na(ggplot_data)
    #'
    #################################
    # Pair policies with attributes #
    #################################
    # Policies colors
    policies_colors <- ggplot_data %>% select(payment_selection_criteria, color) %>% unique() %>% .$color
    names(policies_colors) <- ggplot_data %>% select(payment_selection_criteria, color) %>% unique() %>% .$payment_selection_criteria
    # Policies alias
    policies_alias <- ggplot_data %>% select(payment_selection_criteria, alias) %>% unique() %>% .$alias
    names(policies_alias) <- ggplot_data %>% select(payment_selection_criteria, alias) %>% unique() %>% .$payment_selection_criteria
    #'
    ########################
    # Create ggplot object #
    ######################## 
    fig <- 
        # Base layer
        ggplot(ggplot_data, aes(
            x = cost_so_far,
            y = AUC_holdout,
            col = payment_selection_criteria,
            shape = I(pch)
        )) + 
        # Line plot layer
        geom_line(size = line_size) +
        scale_colour_manual(values = policies_colors,
                            labels = policies_alias) + 
        # Plot labels
        labs(color = "") +
        # Theme settings
        theme_bw() + 
        theme(
            legend.background = element_rect(fill = "transparent")
        )
        
    # Adjust the legend
    reposition_legend(aplot = fig, position = 'bottom right')
        
        
    # Theme layers
    # scale_colour_manual(name = "",
    #                     values = c(myline1="red", myline2="blue"))
}

