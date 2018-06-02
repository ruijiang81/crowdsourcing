report_quality_assurance <- function(report){
    column_names <- c("batch")#, "repetition")
    assertive::assert_are_intersecting_sets(report %>% colnames, column_names)
    assertive::assert_all_are_greater_than(rep_report %>% .$batch %>% diff(), 0)
    assertive::assert_all_are_not_na(rep_report %>% select(instance_num, AUC_holdout))
}
