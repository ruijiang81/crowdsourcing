################################################################################
## Analyze Reports - Produce Graphs
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$",path="./functions/",full.names=TRUE), source)


################################################################################
## Get the data
################################################################################
reports_folder = file.path(getwd(),"reports")
reports = import.reports(reports_folder,
                         # Remove the "random" rule metadata
                         random.rm=FALSE)


################################################################################
## Visualization
################################################################################
# Calculate AUC(Cost)
outputs  = interpolate.reports(reports_folder, na.rm=FALSE)
# Should min and max benchmarks be added to the plot?
benchmarks = TRUE
# Create different plots by
plot_div = c("DATABASE_NAME","model_inducer","cost_function_type")
plot_param = unique(outputs[,plot_div])


for(k in 1:nrow(plot_param))
{
    # Subset the output
    cases = !logical(nrow(outputs))
    for(p in 1:length(plot_div)) cases = (cases & outputs[,plot_div[p]] %in% plot_param[k,p])
    output = outputs[cases,]
    # Include/Exclude the min_pay and max_pay benchmarks
    if(!benchmarks) output = output[!substr(output$payment_selection_criteria, 1, 7) %in% c("min_pay","max_pay"),]
    
    # Setup
    y_range = signif(range(output$average_holdout_cost_performance, na.rm=TRUE),1)
    
    # Create plots directory
    plot_dir  = file.path(getwd(),"plots")
    dir.create(plot_dir,showWarnings=F)
    
    # Plot AUC as function of number of observations
    fig <- ggplot(output, aes(x=cost_intervals, y=average_holdout_cost_performance, group=payment_selection_criteria)) +
        # Add lines to the plot
        geom_line(aes(colour = payment_selection_criteria), size=1) +
        #geom_hline(aes(yintercept=0.5)) + 
        # Add scatter points to the plot
        geom_point(aes(colour = payment_selection_criteria), size=4) +
        # Change axis labels
        xlab("Cost of Model [$]") + ylab("AUC") + 
        # Set axis limits and ticks
        scale_x_continuous(breaks = seq(40,300,10), limits = c(40, 150)) +
        #scale_y_continuous(breaks = signif(seq(y_range[1], y_range[2], length.out=10),1)) +
        # Theme settings
        theme_bw() + theme(strip.text.x = element_blank(),
                           strip.background = element_rect(colour="white", fill="white"),
                           legend.position=c(.9,.1))
    plot(fig)
    ### Export "Auc vs. Cost" plot
    plot_name = paste0('(',unique(output$DATABASE_NAME),')',
                       '(',unique(output$model_inducer),')',
                       '(',unique(output$cost_function_type),')',
                       '(','Auc vs. Cost',')')
    if(benchmarks) plot_name = paste0(plot_name,'(With Benchmarks)')
    plot_name = paste0(plot_name,".png")            
    ggsave(file=file.path(plot_dir,plot_name), fig,width=12, height=8)
} # end for plot_param

