################################################################################
## Analyse Reports
################################################################################
## Initialization
cat("\014"); rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$",path="./functions/",full.names=TRUE), source)


################################################################################
## Get the data
################################################################################
### import metadata
metadata_folder = file.path(getwd(),"reports","metadata")
metadata = import.reports(metadata_folder, 
                          # Remove the "random" rule metadata
                          random.rm=TRUE)

metadata = aggregate(pay ~ batch + repetition + payment_selection_criteria, 
                     metadata, mean)
metadata = arrange(metadata, payment_selection_criteria, repetition, batch)
### import reports
reports_folder = file.path(getwd(),"reports")
reports = import.reports(reports_folder,
                         # Remove the "random" rule metadata
                         random.rm=FALSE)


################################################################################
## Discrete Markov Chains
################################################################################
rules = unique(metadata$payment_selection_criteria)
sequence = subset(metadata, 
                  repetition==1 & payment_selection_criteria %in% rules[1],
                  select=pay)
states = paste0(t(sequence),'$')
library(markovchain)
mcFit <- markovchainFit(data=states)
## transition matrix
tm = mcFit$estimate@transitionMatrix
#colnames(tm) = rownames(tm) = paste0(colnames(tm),"$")
cat("The probability from row payment into column payment")
round(tm,2)
##
dtmcA <- new("markovchain",transitionMatrix=tm,
             #states=c(),
             name="Payment transition matrix") #create the DTMC
plot(dtmcA)


################################################################################
## Distribution of payments per repetition (Batch-Wise)
################################################################################
#round(prop.table(table(states)),2)
library(manipulate)
library(pander)

manipulate(
    {
        cat("\014")
        rules = c("delta_auc_div_total_cost","max_quality","max_ratio","max_total_ratio")
        flags = c(f1,f2,f3,f4)
        
        sub_dataset = metadata[metadata$payment_selection_criteria %in% rules[flags],]
        sub_dataset = subset(sub_dataset, select=c(batch,pay))
        
        long_dataset = table(sub_dataset)
        
        colnames(long_dataset) = paste0(colnames(long_dataset),"$")
        barplot(colSums(long_dataset), main="Payment selection distribution (Batch-Wise)")
        head(long_dataset,n_batch)
    },
    n_batch = slider(1,max(metadata$batch), 10, step=1),
    f1 = checkbox(F, rules[1]), 
    f2 = checkbox(F, rules[2]),
    f3 = checkbox(T, rules[3]),
    f4 = checkbox(F, rules[4])
)# end payment distribution manipulate








# manipulate(
#     {
#         rules = c("delta_auc_div_total_cost","max_quality","max_ratio","max_total_ratio")
#         flags = c(f1,f2,f3,f4)
#         sequence = c()
#         for(flag in flags){
#             if(flag){
#                 sequence = rbind(sequence,subset(metadata,
#                                                  payment_selection_criteria %in% rules[flag],
#                                                  select=pay)) 
#             } # end if
#         } # end for
#         
#         states = paste0(t(sequence),'$')
#         library(markovchain)
#         mcFit <- markovchainFit(data=states)
#         ## transition matrix
#         tm = mcFit$estimate@transitionMatrix
#         cat("\n","The probability from row payment into column payment")
#         # performance.table <- table(some_table)
#         pandoc.table(round(tm,2), style = "grid", justify = 'left', caption = 'Performance table')
#         
#         ##
#         dtmcA <- new("markovchain",transitionMatrix=tm,
#                      #states=c(),
#                      name="Payment transition matrix") #create the DTMC
#         plot(dtmcA)
#     },
#     f1 = checkbox(F, rules[1]), 
#     f2 = checkbox(F, rules[2]),
#     f3 = checkbox(T, rules[3]),
#     f4 = checkbox(F, rules[4])
# ) # end manipulate





# ## Calculate AUC(Cost)
# outputs = interpolate.reports(reports_folder, na.rm=FALSE)
# 
# 
# ## Visualisations
# ### Create plots dir
# plot_dir  = file.path(getwd(),"plots")
# dir.create(plot_dir,showWarnings=F)
# ### Plot AUC as function of number of observations
# ggplot(outputs, aes(x=cost_intervals, y=average_holdout_cost_performance, group=payment_selection_criteria)) +
#     # Add lines to the plot
#     geom_line(aes(colour = payment_selection_criteria), size=1) +
#     # Add scatter points to the plot
#     geom_point(aes(colour = payment_selection_criteria), size=4) +
#     # Change axis labels
#     xlab("Cost of Model [$]") + ylab("AUC") + 
#     # Set axis limits and ticks
#     scale_x_continuous(breaks = seq(40,150,10), limits = c(40, 150)) +
#     #scale_y_continuous(limits = c(NA, 1)) +
#     # Theme settings
#     theme_bw() + theme(strip.text.x = element_blank(),
#                        strip.background = element_rect(colour="white", fill="white"),
#                        legend.position=c(.1,.9))
# ### Export "Auc vs. Cost" plot
# plot_name = paste0('(',unique(outputs$DATABASE_NAME),')',
#                    '(',unique(outputs$model_inducer),')',
#                    '(',unique(outputs$cost_function_type),')',
#                    '(','Auc vs. Cost',')',
#                    ".png")
# ggsave(file=file.path(plot_dir,plot_name), width=12, height=8)
