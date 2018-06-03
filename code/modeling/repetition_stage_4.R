################################################################################
# Repetition Stage 4: Running the rest of the simulation                       #
################################################################################
repetition_stage_4 <- function(){
    ####################
    # Input validation #
    ####################
    #' Check if argumenta are in the global environment
    assertive::assert_all_are_existing(envir = globalenv(), c("counter_batches",
                                                              "secondary_cost_function_flag",
                                                              "current_report_line",
                                                              "rep_metadata",
                                                              "unlabeled_data",
                                                              "holdout_data",
                                                              "k_path_temporary",
                                                              "decide_price_per_label",
                                                              "cost_so_far",
                                                              "current_instance_num",
                                                              "training_set",
                                                              "rep_report"))
    #'
    #########
    # Setup #
    #########
    path_repetition <- file.path(k_path_temporary, "report")
    dir.create(path_repetition, show = FALSE, recursive = TRUE) 
    #'
    ################
    # Stage Kernel #
    ################
    #' Check the report quality
    report_quality_assurance(rep_report)
    report_quality_assurance(rep_metadata)
    #'
    while((cost_so_far <= max_total_cost) & 
          (Sys.time() - start.time < watchdog_simulation)){
        cat("\n","Total model cost",paste0(round(cost_so_far,1),"$"))
        cat("\n","Total instances in the model",current_instance_num)
        #'
        #################
        # Sanity Checks #
        #################
        # Handle the "run out of instances" issue
        if(current_instance_num + batch_size > max_size_training_data){
            cat("\n", "Out of unlabeled instances")
            break
        } 
        #'
        ###########################
        # Choose next cost to pay #
        ###########################
        pay_per_label <- decide_price_per_label(training_set,
                                                payment_selection_criteria,
                                                price_per_label_values,
                                                current_instance_num,
                                                rep_metadata,
                                                current_repetition,
                                                inducer = model_inducer)
        #'
        #############################################
        # Purchase new labels in the selected price #
        #############################################
        #' Instance-wise purchase 
        for (k in 1:batch_size) {
            #' Append one new record to training set (without its label)
            if (current_instance_num==1){
                training_set <- unlabeled_data[1,]
            } 
            else {
                training_set <- rbind(training_set, 
                                      unlabeled_data[current_instance_num,])
            }
            #' Check if the condition for changing cost function is applicable
            if (secondary_cost_function_flag & (cost_so_far>model_cost_for_changing_cost_function))
                cost_function_type = secondary_cost_function
            #' Set the quality level for the given cost-function and payment
            #' configuration
            labeling_accuracy <- labelingCostQualityTradeoff(cost_function_type,
                                                             pay_per_label,
                                                             fixProbability)
            #' Get the record's label (the mechanical turk generator)
            set.seed(current_instance_num * global_seed)
            random_number <- runif(1)
            if (random_number>labeling_accuracy){
                training_set$y[current_instance_num] <- 
                    change_level_value(training_set$y[current_instance_num])
                # Flag that the label is changed
                change <- 1
            }
            else {
                # Flag that the label is unchanged
                change <- 0
            }
            #'
            # Instance-wise logger
            cost_so_far <- cost_so_far + pay_per_label
            new_entry <- data.frame("instance_num"=current_instance_num,
                                    "pay"=pay_per_label,
                                    "change"=change,
                                    "cost_so_far"=cost_so_far,
                                    "updated_label"=training_set$y[current_instance_num],
                                    "batch"=counter_batches,
                                    "svm_bug"=NA)
            # Repetition-wise logger
            rep_metadata = merge(rep_metadata, new_entry, all=TRUE)
            #'
            # Update the instance counter
            current_instance_num <- current_instance_num+1
        }
        #'
        #################################
        # Evaluate model on unseen data #
        #################################
        ## AUC
        calculated_AUC = predict_set(training_set,
                                     holdout_data,
                                     inducer=model_inducer)
        cat('\n',"AUC =",calculated_AUC)
        
        # Fix SVM bug
        if(tolower(model_inducer)=="svm"){
            rep_metadata[current_instance_num-1,"svm_bug"] = calculated_AUC < 1-calculated_AUC
            calculated_AUC = max(calculated_AUC,1-calculated_AUC)
        }
        
        
        ## Store iteration metadata in the report
        rep_metadata[current_instance_num-1,"AUC_holdout"] = calculated_AUC
        new_item            = rep_metadata[current_instance_num-1,]
        new_item$repetition = current_repetition
        new_item$batch      = counter_batches
        
        if(payment_selection_criteria %in% c("max_quality","max_ratio","max_total_ratio","delta_AUC_div_total_cost")){
            new_item$full_AUC = NA
            new_item$subset_AUC = NA
            ## Add data from text file
            delta_performance = read.csv(file.path(k_path_temporary,"delta_performance_improvements.txt"), header = FALSE)
            full_performance  = read.csv(file.path(k_path_temporary,"full_performance_improvements.txt"), header = FALSE)
            ## Add full performance
            new_item$full_AUC = full_performance[1,"V2"]
            ## Add delta performance
            dn = nrow(delta_performance)
            dm = ncol(delta_performance)
            ### Duplicate new_item
            for(i in 2:(dm-1)) new_item[i,] = new_item[i-1,]
            ### Store subset AUC
            for(i in 2:dm) new_item[i-1,"subset_AUC"] = delta_performance[i]
        } else {
            new_item$full_AUC   = NA
            new_item$subset_AUC = NA
        }
        
        rep_report = rbind(rep_report,new_item)
        counter_batches = counter_batches+1 # updating the batch counter
        current_report_line <- current_report_line+1
    } # end Running the rest of the simulation
    rep_metadata$repetition <- current_repetition
    #'
    ################
    # Save Results #
    ################
    path_output <- file.path(path_repetition, paste0(current_repetition,".csv"))
    write_csv(rep_report %>% filter(repetition == current_repetition), path_output)
    #'
    ##########
    # Return #
    ##########
    report <<- rbind(report, rep_report)
    rep_metadata <<- rep_metadata
    metadata <<- merge(metadata, rep_metadata, all = TRUE)
    return(invisible())
}
