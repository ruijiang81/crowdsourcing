################################################################################
## Analyze Reports - Produce Graphs
################################################################################
source(file.path(getwd(), "code", "scripts", "setup.R"))
cat_80("Metadata Pro Processing")
plot_type <- c("Performance vs. cost",
               "Avg label quality vs. cost",
               "No. of labels acquired vs. cost")[3]
#'
##############################
# Get the project's metadata #
##############################
policies_attributes <- read_csv(file.path(k_path_dictionaries, "policies-attributes.csv"))
analytics_attributes <- read_csv(file.path(k_path_dictionaries, "analytics-attributes.csv"))
analytics_attributes <- analytics_attributes %>% filter(name == plot_type)
#'
##########################
# Get the project's data #
##########################
if(plot_type == "Performance vs. cost"){
    source(file.path(k_path_analytics, "processing-the-report-files.R"))
    data <- read_csv(file.path(k_path_results, "processed", "reports-by-dollar.csv"))
} else if(plot_type %in% c("Avg label quality vs. cost", "No. of labels acquired vs. cost")) {
    source(file.path(k_path_analytics, "processing-the-metadata-files.R"))
    data <- read_csv(file.path(k_path_results, "processed", "metadata-by-dollar.csv"))
} else {
    stop("Plot type is unknown")
}
data <- 
    data %>%
    rename("y" = analytics_attributes[["y"]][1],
           "x" = analytics_attributes[["x"]][1])
# data %>% .$payment_selection_criteria %>% unique()
arg1 <- data %>% .$payment_selection_criteria %>% unique()
arg2 <- policies_attributes %>% .$payment_selection_criteria %>% unique()
assert_is_subset(arg1, arg2)
#'
###############################
# Configure ggplot attributes #
###############################
## How many rows and columns are in the ggpolt grid?
mfrow <- 3
mfcol <- 3 
## What is the text and line sizes?
text_size <- 10
line_size <- 1.2
## What is the x axis range?
xlim <- c(40, 150)
#'
################################################################################
## Visualization
################################################################################
pairs <- data %>% select(database_name, cost_function_type) %>% unique()
for(k in 1:nrow(pairs)){
    ###############################
    # Prepare the data for ggplot #
    ###############################
    # Subset the data
    ggplot_data <- 
        data %>% 
        filter(database_name %in% pairs[k, "database_name"],
               cost_function_type %in% pairs[k, "cost_function_type"],
               x <= max(xlim), x >= min(xlim))
    assert_all_are_not_na(ggplot_data)
    # Aggregate the data
    ggplot_data <-
        ggplot_data %>%
        group_by(payment_selection_criteria, x) %>%
        summarise(y = mean(y))
    assert_all_are_not_na(ggplot_data)
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
        ggplot_data %>%
        # Base layer
        ggplot(aes(x = x, y = y, col = payment_selection_criteria)) + 
        # Line plot layer
        geom_line(size = line_size) +
        scale_colour_manual(values = policies_colors,
                            labels = policies_alias) + 
        # X and Y axes attributes
        scale_x_continuous(breaks = seq(min(xlim), max(xlim), by = 10)) +  
        # scale_y_continuous(breaks = ybreaks) +
        # Plot labels
        labs(x = analytics_attributes$xlab,
             y = analytics_attributes$ylab,
             color = "") +
        # Theme settings
        theme_bw() + 
        theme(
            legend.background = element_rect(fill = "transparent"),
            axis.text.y = element_text(angle = 0, vjust = 0, size = text_size),
            axis.text.x = element_text(angle = 90, hjust = 1, size = text_size),
            legend.text = element_text(size = text_size),
            aspect.ratio = 1
        )
    
    # Adjust the legend
    fig <- reposition_legend(aplot = fig, position = 'bottom right', plot = FALSE)
    #'
    ##############
    # Store plot #
    ##############    
    plot(fig)
    # Store plot
    plot_name <- 
        "" %()% pairs[k,"database_name"] %()%
        pairs[k,"cost_function_type"] %()%
        tolower(plot_type) %()%
        paste0(unique(ggplot_data$alias), collapse=", ") %+%
        ".png"
    plot_path <- file.path(k_path_figures, plot_name)
    ggsave(
        file = plot_path,
        plot = fig,
        width = 8.3, height = 8.3, dpi = 100, units = "cm",
        # arrangeGrob(fig, ncol = mfcol, nrow = mfrow),
        device = "png"
    )
}

