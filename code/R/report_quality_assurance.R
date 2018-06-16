report_quality_assurance <- function(report){
    column_names <- c("instance_num", "pay", "assert_all_are_greater_than_or_equal_to", "change", "cost_so_far", 
                      "AUC_holdout", "full_AUC", "subset_AUC")
    assertive::assert_are_intersecting_sets(report %>% colnames, column_names)
    assertive::assert_all_are_not_na(report %>% select(instance_num, batch))
    assertive::assert_all_are_greater_than_or_equal_to(report %>% .$batch %>% diff(), 0)
    assertive::assert_all_are_greater_than_or_equal_to(report %>% .$instance_num %>% diff(), 0)
}
