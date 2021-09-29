#' Create a bullet chart with significance bars to compare different baselines
#' in percentage for gender analysis
#'
#' @name bullet_chart
#' @param data_df, dataframe in output from \code{\link{percent_df}}
#' @param baseline_female, numeric vector containing the baseline for each level
#' @param x_label, label for x axis
#' @param y_label, label for y axis
#' @param baseline_label, label used to define the baseline name.
#' @return This function create a bullet chart containing the percentage of
#' submission with the corresponding baseline for the level defined in
#' \code{\link{percent_df}}.
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 alpha
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 element_text
#' @export


bullet_chart <- function(data_df, baseline_female, x_label, y_label,
                                    baseline_label) {
  ## define global variable
  x_values <- y_values <- gender <- pos <- lower_CI <- upper_CI <- level <- NULL
  ## create the dataframe for generating the bullet chart.
  data_df$gender <- factor(gsub("_percentage", "", data_df$gender),
                                 levels = c("male", "female"))
  data_df$pos <- ifelse(data_df$gender == "male", 90, data_df$y_values)
  data_df$labels <- paste(data_df$y_values, "%")
  data_df$labels <- ifelse(data_df$gender == "male", data_df$significance,
                            data_df$labels)
  baseline_df <- data.frame(level = unique(data_df$x_values),
                            baseline = baseline_female)
  ## Create the bullet chart
  plot <- ggplot() +
    geom_bar(aes(x = level, y = baseline, fill = "#D7191C"),
             data = {{baseline_df}}, width = 0.85, stat = "identity",
             show.legend = FALSE) +
    geom_bar(aes(x = x_values, y = y_values, fill = gender),
             data = {{data_df}}, stat = "identity", width = 0.4) +
    scale_fill_manual(values = c(alpha("#D7191C", .4), alpha("#512B58", .7),
                               alpha("#2A7886", 0.7)),
                      labels = c({{baseline_label}}, "Female", "Male"),
                      name = "") +
    guides(fill = guide_legend(override.aes = list(alpha = 0.5))) +
    scale_x_discrete(limits = rev(levels(droplevels(data_df$x_values)))) +
    geom_text(data = {{data_df}}, aes(x = x_values, y = pos, label = labels),
              size = 15 / .pt, vjust = -0.2) +
    geom_errorbar(data = {{data_df}}, aes(x = x_values, ymin = lower_CI,
                                          ymax = upper_CI), width = 0.3) +
    theme_gd() + xlab({{x_label}}) + ylab({{y_label}}) + coord_flip()
  plot
}
