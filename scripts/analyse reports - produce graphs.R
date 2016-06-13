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
## Define interval ticks [in $]
interval_size = 1
## Calculate AUC(Cost)
reports = interpolate.reports(reports_folder, na.rm=FALSE, interval_size)


#####################
# ggplot attributes #
#####################
## Change legend title
#legend_title = "Payment Selection Criteria"
legend_title = "" # No title

## x # The range of x axis
xlim=c(40,150) 
#xlim=c(140,300)


################################################################################
## Visualization
################################################################################
# Create different plots by
plot_div = c("DATABASE_NAME","model_inducer","cost_function_type")
# Control plot nuances
param <- expand.grid(
    # Should min and max benchmarks be added to the plot?
    benchmarks=c("Yes",              # 1 Every rule
                 "No",               # 2 Every rule except for Minimum Payment & Maximum Payment
                 "Min-Max-Random",   # 3 Random, Minimum Payment, Maximum Payment
                 "MR",               # 4 Random, Max Ratio, Max Ratio 100
                 "MTR",              # 5 Random, Max Total Ratio, Max Total Ratio 100
                 "Main results")[5], # 6 Random, Max Ratio 100, Max Total Ratio 100
    stringsAsFactors=FALSE)

for(l in 1:nrow(param))
{
    # Aggregate the output
    outputs   = reports
    col_names = colnames(outputs)
    outputs$repetition = 0
    outputs = aggregate(average_holdout_cost_performance ~ . -cost_intervals,
                        outputs, 
                        function(x) mean(x, na.rm=T))
    outputs = outputs[col_names]
    
    
    plot_param = unique(outputs[,plot_div])
    for(k in 1:nrow(plot_param))
    {
        # Subset the output
        cases = !logical(nrow(outputs))
        for(p in 1:length(plot_div)) cases = (cases & outputs[,plot_div[p]] %in% plot_param[k,p])
        output = outputs[cases,]
        output = output[output$cost_intervals>=xlim[1] & output$cost_intervals<=xlim[2],]
        # Include/Exclude the min_pay and max_pay benchmarks
        benchmarks = param[l,"benchmarks"]
        if(benchmarks=="No") 
            output = output[!substr(output$payment_selection_criteria, 1, 6) %in% c("min_pa","max_pa"),]
        else if(benchmarks=="Min-Max-Random")
            output = output[substr(output$payment_selection_criteria, 1, 6) %in% c("random","min_pa","max_pa"),]
        else if(benchmarks=="MR")
            output = output[substr(output$payment_selection_criteria, 1, 6) %in% c("random","max_ra"),]
        else if(benchmarks=="MTR")
            output = output[substr(output$payment_selection_criteria, 1, 6) %in% c("random","max_to"),]
        else if(benchmarks=="Main results")
            output = output[output$payment_selection_criteria %in% c("random100","max_ratio100","max_total_ratio100"),]
        # Change policies names (source: Environment Variables)
        for(p in 1:nrow(policies_metadata))
        {
            original_name = policies_metadata[p,"names_original"]
            new_name      = policies_metadata[p,"names_new"]
            output[output$payment_selection_criteria %in% original_name,"payment_selection_criteria"] = new_name
        }# end changing policies names
        
        # Add plots attributes
        output$linetype = "solid" # default value
        output$color    = "black" # default value
        for(p in 1:nrow(policies_metadata))
        {
            policy_name     = policies_metadata[p,"names_new"]
            policy_linetype = policies_metadata[p,"linetype"] 
            policy_color    = policies_metadata[p,"color"]
            output[output$payment_selection_criteria %in% policy_name, "linetype"] = policy_linetype
            output[output$payment_selection_criteria %in% policy_name, "color"]    = policy_color
        }# end setting policies attributes
        
        # Convert character 2 factor
        output$payment_selection_criteria = factor(output$payment_selection_criteria)
        
        
        ####################
        # Render the plots #
        ####################
        # Create plots directory
        plot_dir = file.path(getwd(),"plots")
        dir.create(plot_dir,showWarnings=F)
        
        # Plot AUC as function of number of observations
        fig <- ggplot(output, aes(x=cost_intervals, 
                                  y=average_holdout_cost_performance, 
                                  group=payment_selection_criteria,
                                  col=payment_selection_criteria,
                                  linetype=payment_selection_criteria)) +
            
            # Add lines to the plot
            geom_line(size=2,show.legend=T) +
            scale_linetype_manual(values = unique(output[,c("payment_selection_criteria","linetype")])$linetype) +
            scale_colour_manual(values = unique(output[,c("payment_selection_criteria","color")])$color) + 
            # Add scatter points to the plot
            #geom_point(aes(colour = payment_selection_criteria), size=4) +
            
            # X axis attributes
            ## Set axis label
            #xlab("Cost of Model [$]") + 
            xlab("Cost") +
            ## Set axis limits and ticks
            scale_x_continuous(breaks=seq(0,300,10), limits=xlim) +
            
            # Y axis attributes
            ## Set axis label
            ylab("AUC") +
            ## Set axis limits and ticks
            # scale_y_continuous(breaks = signif(seq(y_range[1], y_range[2], length.out=10),1)) +
            
            # Theme settings
            theme_bw() + 
            theme(strip.text.x = element_blank(),
                  strip.background = element_rect(colour="white", fill="white"),
                  # legend.position = "bottom",
                  legend.position=c(.75,.2),
                  text=element_text(size=20)) +
            # Legend title
            labs(colour=legend_title, linetype=legend_title)
        
        plot(fig)
        ### Export "Auc vs. Cost" plot
        plot_name = paste0('(',unique(output$DATABASE_NAME),')',
                           '(',unique(output$model_inducer),')',
                           '(',unique(output$cost_function_type),')',
                           '(','Auc vs. Cost',')')
        #'(','interval size=',interval_size,')')
        if(benchmarks=="Yes") 
            plot_name = paste0(plot_name,'(With Benchmarks)')
        else if(benchmarks=="No")
            plot_name = paste0(plot_name,'(Without Benchmarks)')
        else if(benchmarks=="Min-Max-Random")
            plot_name = paste0(plot_name,'(',benchmarks,')')
        else if(benchmarks=="Only")
            plot_name = paste0(plot_name,'(benchmarks)')
        else if(benchmarks=="Main results")
            plot_name = paste0(plot_name,'(Main results)')
        else if(benchmarks=="MR")
            plot_name = paste0(plot_name,'(MR)')
        else if(benchmarks=="MTR")
            plot_name = paste0(plot_name,'(MTR)')
        
        
        plot_name = paste0(plot_name,'(','interval size=',interval_size,')')
        plot_name = paste0(plot_name,".png")            
        ggsave(file=file.path(plot_dir,plot_name), fig,width=12, height=8)
    } # end for plot_param
} # end for param
