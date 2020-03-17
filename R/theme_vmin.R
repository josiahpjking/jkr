#' very minimal theme
#' @export
theme_vmin = function(mode="light"){
  if(mode=="dark"){
    ggplot2::theme(
      text = ggplot2::element_text(colour='white'),
      axis.text.x = ggplot2::element_text(colour='white'),
      axis.text.y = ggplot2::element_text(colour='white'),
      legend.key = ggplot2::element_rect(fill='transparent', colour = NA),
      legend.background = ggplot2::element_rect(fill='transparent', colour = NA),
      panel.background = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "#002a35",colour = NA),
      panel.grid.major.x  = ggplot2::element_line(color = NA), panel.grid.minor  = element_line(color = "NA")
    )
  } else {
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
}