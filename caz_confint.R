# works with glm and brglm
# with glm it should return pretty much the same as the medcalc thing
# brglm is a little different. 
# if ever need to describe it, then it's is literally just 1.96* the SE's calculated by a model.
# give it your model, and predictor term if you like. (if not it will just spit out all of them)
caz_confint<-function(model,term="",rnd=3,level=.95){
  idx=grepl(term,rownames(summary(mod1)$coefficients))
  b=summary(model)$coefficients[idx,1]
  ci=summary(model)$coefficients[idx,2]*qnorm(1-((1-level)/2))
  matrix(
    c(round(exp(b),digits=rnd),round(exp(b-ci),digits=rnd),round(exp(b+ci),digits=rnd)),
    ncol=3,
    dimnames=list(c(rownames(summary(model)$coefficients)[idx]),c("OR","lwr","upr"))
  )
}
caz_confint(mod1)
