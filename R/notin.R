#' negation of %in% base function
#' @param x the values to be matched
#' @param y the values to be matched against
#' @export
#' @examples
#' c(1,2,3) %notin% c(2,4,6) returns TRUE, FALSE, TRUE
`%notin%` <- function(x,y){
  !(x %in% y)
} 