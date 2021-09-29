#' Function to create a bar chart of the total number by gender
#'
#' @name bar_chart
#'
#' @param data_df, dataframe from \code{\link{total_gender_df}} 
#' @param x_label, label for x axis.
#' @param y_label, label for y axis.
#' @returns A bar chart as ggplot2 object showing on the y axis the
#'  total number per gender and on the x axis the level previously defined in
#'  \code{\link{total_gender_df}}.
#' @export


bar_chart <- function(data_df, x_label, y_label) {

  ## define global variable to NULL
  x_values <- y_values <- gender <- total_female_male <- NULL
  plot <- ggplot() +
    geom_bar(aes(x = x_values, y = y_values, fill = gender),
             data = {{data_df}},
             stat = "identity", position = "dodge") +
    scale_fill_manual(values = c(alpha("#DCDCDC", .7), alpha("#2A7886", 0.7),
                                 alpha("#512B58", .7)),
                      breaks = c("unknown", "male", "female")) +
    scale_x_discrete(limits = rev(levels(droplevels(data_df$x_values)))) +
    geom_text(data = data_df, aes(x = x_values,
                                  y = total_female_male,
                                  label = paste("Number of male and female:",
                                                total_female_male, sep = " "),
                                  size = 13 / .pt), show.legend = FALSE) +
    xlab({{x_label}}) + ylab({{y_label}}) + theme_gd()
  plot
}
