#' This function create a gender diversity theme for chart based on ggplot2
#'
#' @name theme_gd
#' @return an object of the class theme defined in ggplot2 own class system.
#' @examples
#'  require(ggplot2)
#'  ggplot(authors, aes(x = publication_years)) + geom_bar() + theme_gd()
#' @export


theme_gd <- function() {
  theme(axis.title.x = element_text(size = 20),
          axis.text.x  = element_text(size = 16),
          axis.title.y = element_text(size = 20),
          axis.text.y  = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          text = element_text(color = "navy"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.x = element_line(color = "grey"),
          panel.grid.minor.x = element_line(color = "grey",
                                          linetype = "dashed"),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_rect(fill = "white", color = "grey"),
          legend.position = "bottom", legend.direction = "horizontal")
}
