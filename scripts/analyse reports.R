################################################################################
## Analyse Reports
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$",path="./functions/",full.names=TRUE), source)


## Get the data
reports_folder = file.path(getwd(),"reports")
reports = import.reports(reports_folder)


## Calculate AUC(Cost)
outputs = interpolate.reports(reports_folder, na.rm=FALSE)


## Visualisations
### Create plots dir
plot_dir  = file.path(getwd(),"plots")
dir.create(plot_dir,showWarnings=F)
### Plot AUC as function of number of observations
ggplot(outputs, aes(x=cost_intervals, y=average_holdout_cost_performance, group=payment_selection_criteria)) +
    # Add lines to the plot
    geom_line(aes(colour = payment_selection_criteria), size=1) +
    # Add scatter points to the plot
    geom_point(aes(colour = payment_selection_criteria), size=4) +
    # Change axis labels
    xlab("Cost of Model [$]") + ylab("AUC") + 
    # Set axis limits and ticks
    scale_x_continuous(breaks = seq(40,150,10), limits = c(40, 150)) +
    #scale_y_continuous(limits = c(NA, 1)) +
    # Theme settings
    theme_bw() + theme(strip.text.x = element_blank(),
                       strip.background = element_rect(colour="white", fill="white"),
                       legend.position=c(.1,.9))
### Export "Auc vs. Cost" plot
plot_name = paste0('(',unique(outputs$DATABASE_NAME),')',
                   '(',unique(outputs$model_inducer),')',
                   '(',unique(outputs$cost_function_type),')',
                   '(','Auc vs. Cost',')',
                   ".png")
ggsave(file=file.path(plot_dir,plot_name), width=12, height=8)
