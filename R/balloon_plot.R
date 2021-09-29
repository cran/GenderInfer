#' Function to create the balloon plot for gender first name
#' @param data_df, data frame containing `first name` and `gender` columns from 
#' \code{\link{assign_gender}}
#' @param gender_var, gender possible values are F for female, M for male and U 
#' for unknown
#' @param cutoff, numerical value indicating where to cut the counting data
#' @return The output is a gg object from ggplot2 which shows the most frequent
#' names as a balloon plot.
#' @examples
#' gender <- assign_gender(authors, "first_name")
#' bp <- balloon_plot(gender, "M", cutoff = 5)
#' @importFrom stats reorder
#' @importFrom ggplot2 scale_alpha_continuous
#' @importFrom ggplot2 scale_size_area
#' @importFrom ggplot2 unit
#' @export

balloon_plot <- function(data_df, gender_var, cutoff) {
  n <- gender <- first_name <- NULL
  df <- subset(data_df, length(data_df$first_name) > 1 & gender == gender)
  df <- as.data.frame(table(df[, c("first_name")]))
  df <- df[order(df$Freq, decreasing = TRUE), ]
  df$row <- sort(order(df$Freq, decreasing = TRUE), decreasing = FALSE)
  df$col <- 1
  names(df)[names(df) == "Var1"] <- "first_name"
  names(df)[names(df) == "Freq"] <- "n"
  ## subset the dataframe to include only data greater of a certain value
  df <- subset(df, n >= cutoff)
  ## Create balloon plot
  gg_m <- ggplot(df, aes(x = factor(col), y = reorder(factor(first_name), n),
                         size = n, colour = n, alpha = n)) +
    geom_point() +
    geom_text(aes(label = n, x = col + 0.1), alpha = 1.0, size = 3) +
    scale_alpha_continuous(range = c(0.3, 0.7)) +
    scale_size_area(max_size = 6) +
    theme_gd() +
    theme(axis.line = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          plot.margin = unit(c(0, 2, 0, 2), "cm"),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          axis.text.y  = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          text = element_text(color = "navy"),
          panel.background = element_rect(fill = "white"),
          legend.position = "none") +
    ylab("First name") + xlab("")
  gg_m
}
