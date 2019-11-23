#' very minimal theme
#' @export
theme_vmin = function(){
  ggplot2::theme(
    legend.key = ggplot2::element_rect(fill='transparent', colour = NA),
    legend.background = ggplot2::element_rect(fill='transparent', colour = NA),
    panel.background = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    axis.text.x = ggplot2::element_text(colour='gray50'),
    axis.text.y = ggplot2::element_text(colour='gray50')
  )
}