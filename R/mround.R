#' rounding to the nearest
#' @param x numeric value to round
#' @param base to the nearest
#' @export
#' @examples
#' mround(1002,20)
mround <- function(x,base){
  base*round(x/base)
}	