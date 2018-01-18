#' stop execution if unable to calculate variance for a given family and link
#' @export
#' @examples
#' 

family_link.stop <- function(family, link){
  stop(paste("Don't know how to calculate variance for",
             family, "family and", link, "link."))
}
