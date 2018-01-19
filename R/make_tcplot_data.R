#' time course plots for eye tracking
#' @param objects vector of string names of AOIs (for eyelink trackers, the RIGHT_1_P, RIGHT_2_P,... columns)
#' @param predictor variable name of experimental condition.
#' @param bin BIN variable (for eyelink, this is the CURRENT_BIN column). Defaults to CURRENT_BIN.
#' @param data dataset
#' @export
#' @examples
#' plottingdata <- make_tcplot_data(df=e6data, objects=c("easy_fix","diff_fix","vid_fix"),predictor=gesture)

make_tcplot_data<-function(df, objects, predictor, bin=CURRENT_BIN, bin_interval=20){
  require(tidyverse)
  require(lazyeval)
  predictor=eval(substitute(predictor),df)
  bin=eval(substitute(bin),df)
  plotdat<-as.data.frame(list())
  for (i in objects){
    x<-aggregate(df[,i],by=list(bin, predictor),FUN=mean)
    x$se<-aggregate(df[,i],by=list(bin, predictor),FUN=plotrix::std.error)[,3]
    x$low<-x$x-x$se
    x$up<-x$x+x$se
    x$object<-i
    plotdat<-rbind(plotdat,x)
  }
  names(plotdat)<-c("bin","condition","mean","se","low","up","object")
  plotdat$time<-plotdat$bin*bin_interval
  plotdat$object<-factor(plotdat$object)
  return(plotdat)
}

