#' Calculate binomials and significance for multiple baselines.
#'
#'@description Function to calculate the lower CI, upper CI, percentages and 
#' counts, and significance of difference from one or multiple baseline 
#' percentages, given supplied confidence level using
#'@param data_df, dataframe in output from \code{\link{reshape_for_binomials}}
#' containing the columns: female, male, which contain the integer counts of 
#' males and females respectively and must be a numeric vector greater than 0.
#'@param baseline_female, female baseline in percentage from \code{\link{baseline}}.
#'@param confidence_level, confidence level to use for significance calculation,
#' default is 0.95
#'@returns This function returns a dataframe with additional columns than the 
#'input one:
#'
#' lower_CI = lower confidence level of confidence interval expressed as a 
#' percentage
#'       
#' upper_CI = upper confidence level of confidence interval expressed as a 
#' percentage
#'        
#' lower_CI_count = lower confidence level of confidence interval expressed as a
#'  count
#'        
#' upper_CI_count = upper confidence level of confidence interval expressed as a
#'  count
#'        
#' significance = flag indicating whether difference of female percentage with 
#' baseline percentage is significant for the row in consideration. It has
#' values "significant" or "" if not.
#'@export

calculate_binom_baseline <- function(data_df, baseline_female,
                                     confidence_level = 0.95) {
  
  
  outdf <- data_df 
  outdf$lower_CI <- 0
  outdf$upper_CI <- 0
  outdf$lower_CI_count <- 0
  outdf$upper_CI_count <- 0
  outdf$adjusted_p_value <- 0
  outdf$significance <- ""
  outdf$baseline <- baseline_female
  outdf
  
  rr_row <- sapply(seq_len(length(outdf$female)), 
                   function(x) {
                     .calculate_binom_proportions(
                       no_First = as.numeric(outdf$female[x]),
                       no_Second = as.numeric(outdf$male[x]),
                       expected_Proportion = as.numeric(outdf$baseline[x]) / 100
                       )
  })
  rr_row

  for (x in seq_len(length(outdf$female))) {
    outdf[x, "lower_CI"] <- round(100 * as.numeric(rr_row["Lower_CI", x]), 2)
    outdf[x, "lower_CI_count"] <- round(
      outdf$female[x] * rr_row["Lower_CI", x] / rr_row["Actual_Proportion", x],
      2)
    outdf[x, "upper_CI"] <- round(100 * as.numeric(rr_row["Upper_CI", x]), 2)
    outdf[x, "upper_CI_count"] <- round(
      outdf$female[x] * rr_row["Upper_CI", x] / rr_row["Actual_Proportion", x],
      2)
    outdf[x, "adjusted_p_value"] <- rr_row["Adjusted_PValue", x][[1]]
    outdf[x, "significance"] <- ifelse(
      rr_row["Adjusted_PValue", x][[1]] < (1 - 0.95), "Significant", "")
    outdf
  }
  return(outdf)
}
