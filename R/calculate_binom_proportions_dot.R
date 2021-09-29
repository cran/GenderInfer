#' Function to calculate the binomial proportions
#'
#' @description \code{calculate_binom_proportions} calls Exact Binomial Test
#'  function \code{binom.test}
#' @param no_First, number of successes, numeric vector
#' @param no_Second, number of failures, numeric vector
#' @param expected_Proportion, background population
#' @param no_Tests, for multinomial test default value 1
#' @param confidence_level, confidence level for the returned confidence interval
#'  default is 0.95 using account method mechanism.
#' @return The output will be a vector of the form:
#'
#'   no_First = first input parameter,
#'
#'   no_Second = second input parameter
#'
#'   AdjustedPValue = binom test P-value corrected for number of tests
#'
#'   PValue = binom test P-value,
#'
#'   ActualProportion = binom test estimated probability of success
#'
#'   expected_Proportion = binom test probability of success under the null
#'
#'   LowerCI = binom test lower confidence level of returned confidence interval
#'
#'   UpperCI = binom test upper confidence level of returned confidence interval
#'\url{https://stat.ethz.ch/R-manual/R-patched/library/stats/html/binom.test.html}
#' @examples
#' \dontrun{
#' femaleNumber = 24
#' maleNumber = 36
#' baseFemalePercentage = 23.5
#' maleAndfemaleTotalNumber = 60
#' calculate_binom_proportions(as.numeric(femaleNumber), as.numeric(maleNumber),
#'  baseFemalePercentage/100, maleAndfemaleTotalNumber,0.95)
#'}
#' @importFrom binom binom.confint
#' @importFrom stats binom.test
#' @noRd

.calculate_binom_proportions <- function(no_First, no_Second, 
                                         expected_Proportion,
                                         no_Tests = 1, 
                                         confidence_level = 0.95) {
  rr <- vector("numeric", 8)
  result_binom <- binom.test(x = no_First, n = no_Second + no_First,
                             p = expected_Proportion,
                             conf.level = confidence_level)
  rr[1] <- no_First
  rr[2] <- no_Second
  #simple aka conservative multitest correction
  rr[3] <- result_binom$p.value * no_Tests
  if (rr[3] > 1) {
      rr[3] <- 1
      }
  rr[4] <- result_binom$p.value
  rr[5] <- result_binom$estimate
  rr[6] <- result_binom$null.value
  #we use AC as we are assuming this is high number count data
  result_confidence <- binom.confint(no_First, no_Second + no_First,
                                     methods = "agresti-coull",
                                     conf.level = confidence_level)
  rr[7] <- result_confidence$lower
  rr[8] <- result_confidence$upper

  names(rr) <- c("no_First", "no_Second", "Adjusted_PValue", "P_Value",
               "Actual_Proportion", "expected_Proportion", "Lower_CI",
               "Upper_CI")
  rr
}
