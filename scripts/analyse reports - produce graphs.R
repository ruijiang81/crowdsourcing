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
# Create different plots by
plot_div = c("DATABASE_NAME","model_inducer","cost_function_type")
# Control plot nuances
param <- expand.grid(
    # Define interval ticks [in $]
    interval_size=c(1,5)[1],
    # Should min and max benchmarks be added to the plot?
    benchmarks=c(T,F)[2],
    stringsAsFactors=FALSE)

for(l in 1:nrow(param))
{
    interval_size = param[l,"interval_size"]
    benchmarks    = param[l,"benchmarks"]
    
    
    # Calculate AUC(Cost)
    outputs    = interpolate.reports(reports_folder, na.rm=FALSE, interval_size)
    plot_param = unique(outputs[,plot_div])
    
    ############################
    # ggplot legend attributes #
    ############################
    # Change rules' names
    names_original = c("max_pay_per_label100", "max_ratio100",  "max_ratio1e+06", "max_total_ratio100",  "min_pay_per_label100", "random100")
    names_new      = c("Max Payment",          "Max Ratio 100", "Max Ratio",      "Max Total Ratio 100", "Minimum Payment",      "Random")
    # Change legend title
    legend_title = "Payment Selection Criteria"
    
    
    for(k in 1:nrow(plot_param))
    {
        xlim=c(40,150) # The range of x axis
        
        # Subset the output
        cases = !logical(nrow(outputs))
        for(p in 1:length(plot_div)) cases = (cases & outputs[,plot_div[p]] %in% plot_param[k,p])
        output = outputs[cases,]
        output = output[output$cost_intervals>=xlim[1] & output$cost_intervals<=xlim[2],]
        # Include/Exclude the min_pay and max_pay benchmarks
        if(!benchmarks) output = output[!substr(output$payment_selection_criteria, 1, 7) %in% c("min_pay","max_pay"),]
        # Change rules names
        for(n in 1:length(names_original))
            outputs[outputs$payment_selection_criteria %in% names_original[n],"payment_selection_criteria"] <- names_new[n]
        
        # Setup
        # y_range = signif(range(output$average_holdout_cost_performance, na.rm=TRUE),1)
        
        # Create plots directory
        plot_dir  = file.path(getwd(),"plots")
        dir.create(plot_dir,showWarnings=F)
        
        # Plot AUC as function of number of observations
        fig <- ggplot(output, aes(x=cost_intervals, y=average_holdout_cost_performance, group=payment_selection_criteria)) +
            # Add lines to the plot
            geom_line(aes(colour = payment_selection_criteria), size=2) +
            #geom_hline(aes(yintercept=0.5)) + 
            # Add scatter points to the plot
            #geom_point(aes(colour = payment_selection_criteria), size=4) +
            # X axis attributes
            ## Set axis label
            xlab("Cost of Model [$]") +  
            ## Set axis limits and ticks
            scale_x_continuous(breaks = seq(40,300,10), limits = xlim) +
            # Y axis attributes
            ## Set axis label
            ylab("AUC") +
            ## Set axis limits and ticks
            # scale_y_continuous(breaks = signif(seq(y_range[1], y_range[2], length.out=10),1)) +
            # Theme settings
            theme_bw() + theme(strip.text.x = element_blank(),
                               strip.background = element_rect(colour="white", fill="white"),
                               legend.position=c(.75,.2),
                               text=element_text(size=20)) +
            # Legend Title
            labs(colour = legend_title) 
        plot(fig)
        ### Export "Auc vs. Cost" plot
        plot_name = paste0('(',unique(output$DATABASE_NAME),')',
                           '(',unique(output$model_inducer),')',
                           '(',unique(output$cost_function_type),')',
                           '(','Auc vs. Cost',')')#,
        #'(','interval size=',interval_size,')')
        if(benchmarks) plot_name = paste0(plot_name,'(With Benchmarks)')
        plot_name = paste0(plot_name,'(','interval size=',interval_size,')')
        plot_name = paste0(plot_name,".png")            
        ggsave(file=file.path(plot_dir,plot_name), fig,width=12, height=8)
    } # end for plot_param
} # end for param
