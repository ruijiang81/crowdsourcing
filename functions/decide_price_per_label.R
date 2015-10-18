#' decide_price_per_label
#' 

decide_price_per_label <- function(train,pay_criteria,
                                   payment_options,
                                   cur_instance_num,
                                   meta_data,
                                   repeatition_num)
    {
    if (pay_criteria =="random"){
        set.seed(cur_instance_num*global_seed)
        pay<-sample(payment_options,1)
    } 
    else if (pay_criteria =="min_pay_per_label"){
        pay<-payment_options[which.min(payment_options)]
    } 
    else if (pay_criteria =="max_pay_per_label"){
        pay<-payment_options[which.max(payment_options)]
    } 
    else if ((pay_criteria =="max_quality")|(pay_criteria =="max_ratio")|(pay_criteria =="max_total_ratio")|(pay_criteria=="delta_AUC_div_total_cost")){
        # for testing  
        #payment_options<-price_per_label_values
        #train<-training_set
        #meta_data<-metadata
        #cur_instance_num<-250
        #
        columns_names<- c(payment_options)
        summary_partial_model_performance <- read.table(text = "", col.names = columns_names)
        
        num_payment_options<-length(payment_options) 
        full_model_CV_performance<-cross_validation(train,cross_validation_folds,cross_validation_reruns)
        full_model_CV_performance
        for (i in 1:num_payment_options) {
            #print (i)
            for (j in 1:number_batch_omissions){
                #print (j)
                payment_row_numbers<-which(meta_data$pay == payment_options[i]) #row number with this payments 
                set.seed(cur_instance_num*global_seed+i+j*1000)
                random_rows_to_remove_with_payment <- sample(payment_row_numbers, batch_size) 
                randomly_remaining_instances <- train[-random_rows_to_remove_with_payment, ]; 
                summary_partial_model_performance[j,i]<-cross_validation(randomly_remaining_instances,cross_validation_folds,cross_validation_reruns)
            }
        }  
        partial_model_performance<-colMeans(summary_partial_model_performance, na.rm = FALSE, dims = 1) #vector with the average performance of the partial models per payment option
        delta_performance_improvement<-full_model_CV_performance-partial_model_performance #vector with average delta improvement over a the full model    
        
        out<-toString(c(cur_instance_num,delta_performance_improvement))
        
        file_path<-paste0(directory,"/",repeatition_num,"delta_performance_improvements.txt")
        cat(out,file=file_path, sep="\n", append=TRUE) 
        
        if (pay_criteria =="max_quality"){ 
            pay<-payment_options[which.max(delta_performance_improvement)]
        } 
        
        if (pay_criteria =="max_ratio"){
            if (max(delta_performance_improvement)<0){
                # in this case all values are negative so falling back to the max quality rule
                print ("used_safety_net")
                pay<-payment_options[which.max(delta_performance_improvement)]
            } else {
                ratio_performance<-delta_performance_improvement/payment_options
                pay<-payment_options[which.max(ratio_performance)]
            }
        }
        
        if (pay_criteria =="max_total_ratio"){
            if (max(delta_performance_improvement)<0){
                # in this case all values are negative so falling back to the max quality rule
                print ("used_safety_net")
                pay<-payment_options[which.max(delta_performance_improvement)]
            } else {
                for (t in 1:num_payment_options){
                    if (delta_performance_improvement[t]<0)       #using a large negative number to eliminate out of consideration negative delta improvements (in case part of them are negative and part positive)
                    {delta_performance_improvement[t]=-100000000}
                }
                expected_performance<-delta_performance_improvement+full_model_CV_performance
                expected_total_cost<-meta_data$cost_so_far[cur_instance_num-1]+batch_size*payment_options
                total_ratio_performance<-expected_performance/expected_total_cost
                pay<-payment_options[which.max(total_ratio_performance)]
            }
        }
        
        if (pay_criteria =="delta_AUC_div_total_cost"){
            if (max(delta_performance_improvement)<0){
                # in this case all values are negative so falling back to the max quality rule
                print ("used_safety_net")
                pay<-payment_options[which.max(delta_performance_improvement)]
            } else {
                expected_total_cost<-meta_data$cost_so_far[cur_instance_num-1]+batch_size*payment_options
                ratio_delta_AUC_div_total_cost<-delta_performance_improvement/expected_total_cost
                
                pay<-payment_options[which.max(ratio_delta_AUC_div_total_cost)]
            }
        }
        
    }
    
    else {
        stop("WRONG CRITERIA in decide_price_per_label")  
    }
    
    return(pay)
} # end decide_price_per_label
