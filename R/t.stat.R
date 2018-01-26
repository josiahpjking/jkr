#' extract t values for fixed effects of lmer object
#' @param x a linear mixed effects model object.
#' @export
#' @examples
#' t.stat(fittedmodel1)
t.stat <- function(x) fixef(x)/sqrt(diag(vcov(x)))