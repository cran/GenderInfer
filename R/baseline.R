#' Calculate the female baseline
#'
#' @name baseline
#' @description \code{baseline} calculate the female baseline giving a dataframe
#' containing the gender information.
#' @param data_df, dataframe containing the gender column.
#' @param gender_col, the name of the column containing the gender information.
#' @return The function returns a numeric vector containing the baseline values
#' @examples
#' ## df is the dataframe in output from the function assign_gender
#' df <- data.frame(first_name = c("anna", "john", "ernest", "colin", "aileen"),
#'                  gender = c("F", "M",  "M", "M", "F"),
#'                  stringsAsFactors = FALSE)
#' baseline <- baseline(df, gender_col = "gender")
#' @export

baseline <- function(data_df, gender_col) {
  gender_count <- table(data_df[, gender_col])
  female <- gender_count["F"][[1]]
  male <- gender_count["M"][[1]]
  baseline <- round(100 * (female / (female + male)), 1)
  baseline
}
