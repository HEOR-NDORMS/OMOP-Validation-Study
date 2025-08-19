bootstrap.mean <- function(data, B = 1000, confidence = 0.95) {
  n <- length(data)
  means <- rep(0, B)
  for (i in 1:B) {
    means[i] <- mean(sample(data, size = n, replace = TRUE))
  }
  mean.obs <- mean(means)
  c.l <- round((1 - confidence) / 2 * B, 0)
  c.u <- round(B - (1 - confidence) / 2 * B, 0)
  l <- sort(means)[c.l]
  u <- sort(means)[c.u]
  cat(c.l / B * 100, "-percentile:      ", l, "\n")
  cat("Mean: ", mean.obs, "\n")
  cat(c.u / B * 100, "-percentile:      ", u, "\n")
  return(means)
}

bootstrap.mean(unadjusted_yearly_visits$total_patient_visits, B = 1000, confidence = 0.95)
bootstrap.mean(unadjusted_user_yearly_costs$total_patient_visits_cost, B = 1000, confidence = 0.95)
