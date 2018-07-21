library(plyr)

rm(list = ls())
directory <- "C:\\Users\\user\\Dropbox\\economic labelling\\shared_results\\2015-10-29"
setwd(directory)

# input_file <- "[synthetic_balanced][J48][asymptotic][random][2015-10-28].csv"
# input_file <- "[synthetic_balanced][J48][asymptotic][max_total_ratio][2015-10-28].csv"
# input_file <- "[synthetic_balanced][J48][asymptotic][max_ratio][2015-10-28].csv"
# input_file <- "[synthetic_balanced][J48][asymptotic][max_quality][2015-10-28].csv"
input_file <- "[synthetic_balanced][J48][asymptotic][delta_auc_div_total_cost][2015-10-28].csv"

# num_files=length(filenames)
# print (num_files)



# generate cost table
interval_size <- 1 # usually 1, or 2
min_cost <- 1 # dont start from zero. Start with 0+interval_size (or desired value+interval size)
max_cost <- 150
cost_intervals <- seq(from = min_cost, to = max_cost, by = interval_size)
num_cost_intervals <- length(cost_intervals)
input <- read.csv(input_file, header = TRUE)

#### code to avoid duplicity in input report lines
## Mean the subset_AUC column
input <- aggregate(
    subset_AUC ~ .,
    input, mean
)

input <- arrange(input, repetition, instance_num)
#######

num_repeations <- input$repetition[nrow(input)] # number of repeation is taken from the repeation number of the last row in the input data
print("num_repeations")
print(num_repeations)

for (repeation_counter in 1:num_repeations) {
    # reading in the instance number from the first file and checking for instance number consistency across files
    # file_counter=1
    # current_repeatition <- read.csv(filenames[file_counter], header = TRUE)
    current_repeatition <- input[which(input$repetition == repeation_counter), ]
    num_lines <- nrow(current_repeatition)


    ##### generating calculated performance for fixed cost intervals
    interval_cost_performance <- numeric()

    for (i in 1:num_cost_intervals) {
        # print (i)
        line_counter <- 1
        current_interval_cost <- cost_intervals[i]

        while ((line_counter <= num_lines) & (current_interval_cost >= current_repeatition$cost_so_far[line_counter])) {
            interval_cost_performance[i] <- current_repeatition$AUC_holdout[line_counter]
            line_counter <- line_counter + 1
        }
    }
    ###########
    if (repeation_counter == 1) {
        sum_interval_cost_performance <- interval_cost_performance # contains the performance per cost intervals
    } else {
        sum_interval_cost_performance <- sum_interval_cost_performance + interval_cost_performance # contains the performance per cost intervals
    }
}

average_holdout_cost_performance <- sum_interval_cost_performance / num_repeations
output <- data.frame(cost_intervals, average_holdout_cost_performance)
output_file <- paste("costs", input_file, sep = " ")
write.table(output, output_file, sep = ",")


print("num_repeations")
print(num_repeations)
