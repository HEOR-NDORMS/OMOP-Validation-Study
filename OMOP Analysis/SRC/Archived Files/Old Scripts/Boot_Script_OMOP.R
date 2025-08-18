# Visits ----

###### Unadjusted User ----
# Load the boot package
library(boot)  # Required for bootstrapping functions

# Define the statistic function
# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_user <- function(d, i){
  unadjusted_yearly_visits <- d[i, ]               # Sample the data with replacement
  return(mean(unadjusted_yearly_visits$total_patient_visits))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(626)

# Run bootstrap with 1000 resamples
boot_result_user <- boot(unadjusted_yearly_visits, fc_user, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_user <- boot.ci(boot.out = boot_result_user, type = c("norm", "basic", "perc"))

mean_boot_visits_user_n <-round(as.numeric(boot_result_user$t0),3)
lower_ci_boot_visits_user_n <-round(as.numeric(boot_ci_user$normal[2]), 3)
upper_ci_boot_visits_user_n <-round(as.numeric(boot_ci_user$normal[3]), 3)

mean_boot_visits_user_b <-round(as.numeric(boot_result_user$t0),3)
lower_ci_boot_visits_user_b <-round(as.numeric(boot_ci_user$basic[4]), 3)
upper_ci_boot_visits_user_b <-round(as.numeric(boot_ci_user$basic[3]), 3)

# mean_boot_visits_user_bca <-round(as.numeric(boot_result_user$t0),3)
# lower_ci_boot_visits_user_bca <-round(as.numeric(boot_ci_user$bca[2]), 3)
# upper_ci_boot_visits_user_bca <-round(as.numeric(boot_ci_user$bca[3]), 3)

mean_boot_visits_user_p <-round(as.numeric(boot_result_user$t0),3)
lower_ci_boot_visits_user_p <-round(as.numeric(boot_ci_user$perc[4]), 3)
upper_ci_boot_visits_user_p <-round(as.numeric(boot_ci_user$perc[5]), 3)

boot_user_summary_statistics_overall_n <- tibble(
  mean_boot_visits_user_n = mean_boot_visits_user_n,
  lower_ci_boot_visits_user_n = lower_ci_boot_visits_user_n,
  upper_ci_boot_visits_user_n = upper_ci_boot_visits_user_n
)

boot_user_summary_statistics_overall_b <- tibble(
  mean_boot_visits_user_b = mean_boot_visits_user_b,
  lower_ci_boot_visits_user_b = lower_ci_boot_visits_user_b,
  upper_ci_boot_visits_user_b = upper_ci_boot_visits_user_b
)

# boot_user_summary_statistics_overall_bca <- tibble(
#   mean_boot_visits_user_bca = mean_boot_visits_user_bca,
#   lower_ci_boot_visits_user_bca = lower_ci_boot_visits_user_bca,
#   upper_ci_boot_visits_user_bca = upper_ci_boot_visits_user_bca
# )

boot_user_summary_statistics_overall_p <- tibble(
  mean_boot_visits_user_p = mean_boot_visits_user_p,
  lower_ci_boot_visits_user_p = lower_ci_boot_visits_user_p,
  upper_ci_boot_visits_user_p = upper_ci_boot_visits_user_p
)

# Costs -----

# This function calculates the mean of the variable of interest (e.g., 'visits' or 'costs')
fc_user_costs <- function(d, i){
  unadjusted_user_yearly_costs <- d[i, ]               # Sample the data with replacement
  return(mean(unadjusted_user_yearly_costs$total_patient_visits_cost))   # This is your variable of interest (e.g., visits, or costs)
}

# Set a seed for reproducibility
set.seed(626)

# Run bootstrap with 1000 resamples
# boot_result_user_costs <- boot(unadjusted_user_yearly_costs, fc_user_costs, R = 1000)
# Run bootstrap with 1000 resamples
boot_result_user <- boot(unadjusted_user_yearly_costs, fc_user_costs, R = 1000)

# Compute 95% percentile-based confidence interval
boot_ci_user <- boot.ci(boot.out = boot_result_user, type = c("norm", "basic", "perc"))

mean_boot_visits_user_n <-round(as.numeric(boot_result_user$t0),3)
lower_ci_boot_visits_user_n <-round(as.numeric(boot_ci_user$normal[2]), 3)
upper_ci_boot_visits_user_n <-round(as.numeric(boot_ci_user$normal[3]), 3)

mean_boot_visits_user_b <-round(as.numeric(boot_result_user$t0),3)
lower_ci_boot_visits_user_b <-round(as.numeric(boot_ci_user$basic[4]), 3)
upper_ci_boot_visits_user_b <-round(as.numeric(boot_ci_user$basic[5]), 3)

# mean_boot_visits_user_bca <-round(as.numeric(boot_result_user$t0),3)
# lower_ci_boot_visits_user_bca <-round(as.numeric(boot_ci_user$bca[2]), 3)
# upper_ci_boot_visits_user_bca <-round(as.numeric(boot_ci_user$bca[3]), 3)

mean_boot_visits_user_p <-round(as.numeric(boot_result_user$t0),3)
lower_ci_boot_visits_user_p <-round(as.numeric(boot_ci_user$perc[4]), 3)
upper_ci_boot_visits_user_p <-round(as.numeric(boot_ci_user$perc[5]), 3)

boot_user_summary_statistics_overall_n <- tibble(
  mean_boot_visits_user_n = mean_boot_visits_user_n,
  lower_ci_boot_visits_user_n = lower_ci_boot_visits_user_n,
  upper_ci_boot_visits_user_n = upper_ci_boot_visits_user_n
)

boot_user_summary_statistics_overall_b <- tibble(
  mean_boot_visits_user_b = mean_boot_visits_user_b,
  lower_ci_boot_visits_user_b = lower_ci_boot_visits_user_b,
  upper_ci_boot_visits_user_b = upper_ci_boot_visits_user_b
)

# boot_user_summary_statistics_overall_bca <- tibble(
#   mean_boot_visits_user_bca = mean_boot_visits_user_bca,
#   lower_ci_boot_visits_user_bca = lower_ci_boot_visits_user_bca,
#   upper_ci_boot_visits_user_bca = upper_ci_boot_visits_user_bca
# )

boot_user_summary_statistics_overall_p <- tibble(
  mean_boot_visits_user_p = mean_boot_visits_user_p,
  lower_ci_boot_visits_user_p = lower_ci_boot_visits_user_p,
  upper_ci_boot_visits_user_p = upper_ci_boot_visits_user_p
)