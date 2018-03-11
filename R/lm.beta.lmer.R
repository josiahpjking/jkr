#' equivalent of lm.beta for mixed models.
#' @param mod linear mixed model object
#' @export
#' @examples
#' lm.beta.lmer(fit1)
lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}