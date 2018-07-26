#' decide_price_per_label
#'
#' @description
#' What does it do?
#' (1) There are two sets of rules:
#'     (a) non data-driven (i.e. min_pay_per_label), and
#'     (b) data-driven (i.e. max_total_ratio).
#' According to the selected rule, the function returns a payment for the next
#' iteration.
#' (2) If a non data-driven rule was chosen, no model fitting is required and
#'     the returning value would be a number from the available costs values.
#' (3) If a data-driven rule was chosen, cross validation per each available
#'     cost value is performed. In essence, we estimate the changes in AUC when
#'     omitting serval instances (a batch) bought at a particular payment
#'     value. We call these calculation “partial_model_performance”
#' (4) We fit a model to all the data, estimate it’s performance by K-fold CV
#'     and call it “full_model_CV_performance”.
#' (5) The detla_performance_improvement is the result of subtracting
#'     partial_model_performance from full_model_CV_performance.
#' (6) According to the data-driven rule, we decide what should be the next
#'     payment, that is, the return value.
#'
#' INPUTS:
#' @param train; labeled set
#' @param pay_criteria; By which rule to decide how much to pay for the next
#'        batch?
#' @param payment_options; Available costs values
#' @param cur_instance_num; How many instances are in the model?
#' @param meta_data; Simulation log
#' @param repeatition_num; The current repetition value
#' @param inducer; What inducer should be used to fit models?
#'
#' OUTPUT:
#' @return A scalar – the payment for the next iteration.
#'
decide_price_per_label <- function(train,
                                   # By which rule to decide how much to pay for the next batch?
                                   pay_criteria,
                                   # Available costs values
                                   payment_options,
                                   # How many instances are in the model ?
                                   cur_instance_num,
                                   # Simulation log
                                   meta_data,
                                   # The current repetition value
                                   repeatition_num,
                                   # What inducer should be used to fit models?
                                   inducer = c("RF", "GLM", "J48")) {
    ####################
    # Input Validation #
    ####################
    assertive::assert_all_are_existing(c("k_path_temporary", "k_batch_size"))
    assertive::assert_all_are_existing(c("logger_append", "logger_as_data_frame", "logger_create"))
    #'
    #########
    # Setup #
    #########
    logger_create()
    output_full <- file.path(k_path_temporary, "full_performance_improvements.txt")
    output_delta <- file.path(k_path_temporary, "delta_performance_improvements.txt")
    if (file.exists(output_full)) file.remove(output_full)
    if (file.exists(output_delta)) file.remove(output_delta)
    #'
    if (pay_criteria == "random") {
        set.seed(cur_instance_num * global_seed)
        pay <- sample(payment_options, 1)
    } else if (pay_criteria == "min_pay_per_label") {
        pay <- payment_options[which.min(payment_options)]
    } else if (pay_criteria == "avg_pay_per_label") {
        pay <- payment_options[(payment_options - mean(payment_options)) %>% abs() %>% which.min()]
    } else if (pay_criteria == "max_pay_per_label") {
        pay <- payment_options[which.max(payment_options)]
    } else if (substr(pay_criteria, 1, 6) == "always") {
        pay <- substr(pay_criteria, 8, nchar(pay_criteria))
    } else if (pay_criteria %in% c("max_quality", "max_ratio", "max_total_ratio", "delta_AUC_div_total_cost")) {
        columns_names <- c(payment_options)
        summary_partial_model_performance <- read.table(text = "", col.names = columns_names)

        num_payment_options <- length(payment_options)

        full_model_CV_performance <-
            cross_validation(
                cv_data = train,
                num_folds = cross_validation_folds,
                num_reruns = cross_validation_reruns,
                inducer = inducer
            )

        logger_append(list(AUC_full_train_set = full_model_CV_performance))
        #'
        #' Initialize Progress Bar
        cat("\n")
        pb <- txtProgressBar(0, num_payment_options * number_batch_omissions, style = 3)
        for (i in 1:num_payment_options) {
            # print (i)
            for (j in 1:number_batch_omissions) {
                # print (j)
                payment_row_numbers <- which(meta_data$pay == payment_options[i]) # row number with this payments

                if (length(payment_row_numbers) > max_instances_in_history) {
                    payment_row_numbers <- tail(payment_row_numbers, max_instances_in_history)
                }

                set.seed(cur_instance_num * global_seed + i + j * 1000)
                random_rows_to_remove_with_payment <- sample(payment_row_numbers, k_batch_size)
                randomly_remaining_instances <- train[-random_rows_to_remove_with_payment, ]
                summary_partial_model_performance[j, i] <-
                    cross_validation(
                        cv_data = randomly_remaining_instances,
                        num_folds = cross_validation_folds,
                        num_reruns = cross_validation_reruns,
                        inducer = inducer
                    )
                setTxtProgressBar(pb, (i - 1) * number_batch_omissions + j)
            }
        }
        #' Vector with the average performance of the partial models per payment
        #' option
        partial_model_performance <-
            colMeans(summary_partial_model_performance, na.rm = FALSE, dims = 1)
        logger_append(list(AUC_partial_train_set = partial_model_performance))
        #' Vector with average delta improvement over a the full model
        delta_performance_improvement <-
            full_model_CV_performance - partial_model_performance
        logger_append(list(AUC_train_set_delta = delta_performance_improvement))
        #'
        #################
        # Store results #
        #################
        out_delta <- toString(c(cur_instance_num, delta_performance_improvement))
        out_full <- toString(c(cur_instance_num, full_model_CV_performance))
        cat(out_full, file = output_full, sep = "\n", append = TRUE)
        cat(out_delta, file = output_delta, sep = "\n", append = TRUE)
        #'
        if (pay_criteria == "max_quality") {
            pay <- payment_options[which.max(delta_performance_improvement)]
        }

        if (pay_criteria == "max_ratio") {
            if (max(delta_performance_improvement) < 0) {
                #' In this case all values are negative so falling back to the
                #' max quality rule
                cat("\nused_safety_net")
                logger_append(list(safety_net = T))
                #' Select payment
                pay <- payment_options[which.max(delta_performance_improvement)]
            } else {
                ratio_performance <- delta_performance_improvement / payment_options
                logger_append(list(ratio = ratio_performance, safety_net = F))
                #' Select payment
                pay <- payment_options[which.max(ratio_performance)]
            }
        }

        if (pay_criteria == "max_total_ratio") {
            if (max(delta_performance_improvement) < 0) {
                #' In this case all values are negative so falling back to the
                #' max quality rule
                cat("\nused_safety_net")
                logger_append(list(safety_net = T))
                #' Select payment
                pay <- payment_options[which.max(delta_performance_improvement)]
            } else {
                for (t in 1:num_payment_options) {
                    if (delta_performance_improvement[t] < 0) {
                        #' Using a large negative number to eliminate out of
                        #' consideration negative delta improvements (in case
                        #' part of them are negative and part positive)
                        delta_performance_improvement[t] <- -1e8
                    }
                }
                expected_performance <- delta_performance_improvement + full_model_CV_performance
                expected_total_cost <- meta_data$cost_so_far[cur_instance_num - 1] + k_batch_size * payment_options
                total_ratio_performance <- expected_performance / expected_total_cost
                logger_append(list(
                    expected_performance = expected_performance,
                    expected_total_cost = expected_total_cost,
                    total_ratio = total_ratio_performance,
                    safety_net = F
                ))
                #' Select payment
                pay <- payment_options[which.max(total_ratio_performance)]
            }
        }

        if (pay_criteria == "delta_AUC_div_total_cost") {
            if (max(delta_performance_improvement) < 0) {
                # in this case all values are negative so falling back to the max quality rule
                cat("\nused_safety_net")
                pay <- payment_options[which.max(delta_performance_improvement)]
            } else {
                expected_total_cost <- meta_data$cost_so_far[cur_instance_num - 1] + k_batch_size * payment_options
                ratio_delta_AUC_div_total_cost <- delta_performance_improvement / expected_total_cost
                #' Select payment
                pay <- payment_options[which.max(ratio_delta_AUC_div_total_cost)]
            }
        }
    }

    else {
        stop("WRONG CRITERIA in decide_price_per_label")
    }

    return(as.numeric(pay))
} # end decide_price_per_label
