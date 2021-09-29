#' Create a dataframe that will be the input to generate the bar chart of
#' the full amount of female and male
#'
#' @name total_gender_df
#' @param data_df, dataframe from \code{\link{calculate_binom_baseline}}
#' containing Level, LCI, UCI,
#' Significance and Male and Female percentages
#' @param level, name of level
#' @returns The output is a dataframe with the columns x_values, 
#' total_female_male, gender, y_values. This data frame is the input to create 
#' the bar chart for \code{\link{bar_chart}}
#' @export


total_gender_df <- function(data_df, level) {

  x_values <- y_values <- gender <- total_female_male <- NULL
  long <- reshape(data_df, direction = "long", v.names = "y_values",
                  varying = 2:4, times = names(data_df)[2:4],
                  timevar = "gender", idvar = level)
  names(long)[names(long) == level] <- "x_values"
  long <- subset(long, select = c(x_values, total_female_male, gender,
                                  y_values))
  long <- long[order(rev(long$gender)), ]
  long

}
