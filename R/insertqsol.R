#' Insert \%in\%.
#'
#' Call this function as an addin to insert \code{ \%in\% } at the cursor position.
#'
#' @export
insertqsol <- function() {
  rstudioapi::insertText("
  `r qbegin()`

  `r qend()`
  `r solbegin(show=params$SHOW_SOLS, toggle=params$TOGGLE)`

  `r solend()`
  ")
}
