#' return OR/RR and confidence intervals for logistic/poisson respectively.
#' @param mod a glm(er) object
#' @param method confint method (for mixed models), passed to \code{\link{confint.merMod}}.
#' @export
glmratio <- function(mod, method="profile") {
  #get model type
  if(grepl("merMod",class(mod)[1])){
    meth="mixed"
    fam=mod@resp$family$family
  } else {
    meth="fixed"
    fam=mod$family$family
  }

  #get ratio
  if(fam=="binomial"){
    ratio_type="OR"
  }else if(fam == "poisson"){
    ratio_type="RR"
  }else{stop("wrong type of model")}

  if(meth=="mixed"){est=c(theta=mod@theta,lme4::fixef(mod))}else{est=stats::coef(mod)}
  if(meth=="mixed"){int=lme4::confint.merMod(mod,method=method)}else{int=stats::confint(mod)}
  tab = exp(cbind(est, int))
  colnames(tab)[1] = ratio_type
  tab
}

