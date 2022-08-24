#' A custom GGplot2 theme for mpaviewer
#'
#' @return A custom ggplot2::theme()
#' @export
ggplot_mpatheme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme( # use theme_get() to see available options
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(), # switch off the rectangle around symbols in the legend
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_blank(),
      # legend.position = "top",
      text = ggplot2::element_text(size = 12),
      strip.text.y = ggplot2::element_text(size = 12, angle = 0),
      axis.title.x = ggplot2::element_text(vjust = 0.3, size = 12),
      axis.title.y = ggplot2::element_text(vjust = 0.6, angle = 90, size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.line.x = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
      axis.line.y = ggplot2::element_line(colour = "black", size = 0.5, linetype = "solid"),
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold.italic")
    )
}
