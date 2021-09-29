#' Reshape the dataframe to make it easier to carry out binomial calculations.
#'
#' @name reshape_for_binomials
#' @description reshape dataframe from long format to wide format.
#' @param data_df, dataframe containing the columns gender and counts
#' @param level, variable to compare for the baseline.
#' @param gender_col, the name of the column containing the gender values.
#' @return The output is a dataframe containing more columns than the input one,
#' such as:
#'
#' level : the variable used to perform the binomials
#' total_for_level: the total amount of each gender including unknowns
#' total_female_male: the total amount of male and female
#' female_percentage: the percentage of female in the total_female_male
#' male_percentage: the percentage of male in the total_female_male
#'
#' @examples
#' authors_df <- assign_gender(data_df = authors, first_name_col = "first_name")
#' female_count <- dplyr::count(authors_df, gender)
#'
#' ## create a new data frame to be used for the binomial calculation.
#' df_gender <- reshape_for_binomials(data = female_count, gender_col = "gender",
#'                                   level = 2020)
#' @importFrom stats reshape
#' @export


reshape_for_binomials <- function(data_df, gender_col, level) {
  F <- M <- U <- NULL
  data_df$gender <- factor(data_df[, gender_col])
  if (ncol(data_df) == 2) {
    names(data_df)
    data_df$level <- factor(level)
    wide <- reshape(data_df, timevar = gender_col, idvar = "level",
                    direction = "wide")
  }else if (ncol(data_df) > 2) {
    data_df$level <- unique(data_df[, level])
    data_df[, level] <- NULL
    names(data_df)
    wide <- reshape(data_df, timevar = gender_col, idvar = "level",
                    direction = "wide")
  }else {
    stop(paste0(level, " is not a valid colum of the dataframe"))
  }

  colnames(wide) <-  c("level", "F", "M", "U")
  wide$female <- as.numeric(wide$F)
  wide$male <- as.numeric(wide$M)
  wide$unknown <- as.numeric(wide$U)
  wide$level <- factor(wide$level, levels = wide$level)
  wide$total_for_level <- wide$female + wide$male + wide$unknown
  wide$total_female_male <- wide$female + wide$male
  wide$female_percentage <- round((wide$female / wide$total_female_male) * 100,
                                 1)
  wide$male_percentage <- round((wide$male / wide$total_female_male) * 100, 1)
  wide <- subset(wide, select = -c(F, M, U))

  return(wide)
}
