#' returns a permutation based distribution of possible changepoint values of X.
#' fits gam with n knots, find changepoint, loop(shuffles residuals, fits new gam, find changepoint)
#' 
#' I guess if you want to do some hypothesis testing, then do a z test of your theorised changepoint & permuted distribution?
#' or of your permuted dist vs prior dist?
#' 
#' requires mgcv, tidyverse
#'
#' @param df dataframe or tibble object
#' @param y y variable 
#' @param x x variable (numeric)
#' @param npoints number of changepoints to look for
#' @param iter number of permutations/iterations
#' @export
#' @examples
#' permute_changepoint(dd,rate,year,npoints=3,iter=1000)
permute_changepoint<-function(df,y,x,npoints,iter){
  xvar = df %>% pull({{x}})
  yvar = df %>% pull({{y}})
  
  #observed
  m1 = gam(yvar ~ s(xvar, bs="cs", k=npoints))
  #predict y based on length of x
  py = predict(m1, tibble(xvar = seq(min(xvar), max(xvar), length.out = length(xvar)*4))) #JK check what diff does length.out make to the calcs? - obv it makes a smoother density curve, bcos more variability in x values model is predicted on
  #plot(diff(py))
  cp_obs = cumsum(rle(as.numeric(diff(py)>0))$lengths) #run along the length of py, and get change points (cumsum preserves x)
  cp_obs = cp_obs[-length(cp_obs)]
  
  
  cp_permute<-function(mod){
    perm_y=sample(resid(mod),replace=T)+fitted(mod)
    perm_m=gam(perm_y ~ s(xvar, bs="cs", k=npoints))
    perm_py=predict(perm_m, tibble(xvar = seq(min(xvar), max(xvar), length.out = length(xvar)*4)))
    perm_cp=cumsum(rle(as.numeric(diff(perm_py)>0))$lengths)
    perm_cp[-length(perm_cp)]
  }
  
  cp_dist = map(1:iter,~cp_permute(m1)) %>% unlist %>%
    map_dbl(.,~seq(min(xvar), max(xvar), length.out = length(xvar)*4)[.])
  
  
  cp_plot<-function(yv,xv,dist,mod){
    ggplot(mapping=aes(x=xv,y=yv))+geom_point()+
      geom_line(aes(y=fitted(mod)),col="slateblue")+
      geom_density(inherit.aes=FALSE,aes(x=dist,y=max(yvar)*..density..), col=NA,fill="magenta4",alpha=.4,)+
      theme_minimal()+
      labs(x=enexpr(x),y=enexpr(y))+
      NULL
  }
  print(cp_plot(yvar,xvar,cp_dist,m1))
  return(cp_dist)
}