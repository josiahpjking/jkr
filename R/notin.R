#' negation of %in% base function
#' @param x the values to be matched
#' @param y the values to be matched against
#' @export
#' @examples
#' 1:4 %notin% 2:5 returns TRUE, FALSE, FALSE, FALSE
"%notin%" <- function(x,y){
  !(x %in% y)
} 
