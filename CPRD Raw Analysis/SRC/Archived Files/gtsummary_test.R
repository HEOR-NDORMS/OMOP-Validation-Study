iris = iris

(tb_summary <- iris %>% 
    gtsummary::tbl_summary() %>% 
    gtsummary::modify_header(label = "**Variable**") %>% 
    gtsummary::modify_footnote(everything() ~ NA) %>%  # removes everything, i want Median (Q1, Q3); n(%) to be retained
    gtsummary::modify_footnote(stat_0 = "Median (Q1, Q3); n(%)") # doing returns the superscript 1
    )
