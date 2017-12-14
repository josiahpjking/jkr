#' time course plots for eye tracking
#' @param objects vector of proportions of fixations to visual world AOIs (for eyelink trackers, the RIGHT_1_P, RIGHT_2_P,... columns)
#' @param predictor variable name (string) of experimental conditions.
#' @param bin BIN variable (for eyelink, this is the CURRENT_BIN column). Defaults to "CURRENT_BIN".
#' @param data dataset
#' @export
#' @examples
#' plottingdata <- make_tcplot_data(objects=c("easy_fix","diff_fix","vid_fix"),predictor="g_condition",data=e6data)

make_tcplot_data<-function(objects,predictor,bin="CURRENT_BIN",bin_interval=20,data){
  require(ggplot2)
  require(plotrix)
  plot_data<-as.data.frame(list())
  for (i in objects){
    x<-aggregate(data[,i],by=list(data[,bin],data[,predictor]),FUN=mean)
    x$se<-aggregate(data[,i],by=list(data[,bin],data[,predictor]),FUN=std.error)[,3]
    x$low<-x$x-x$se
    x$up<-x$x+x$se
    x$object<-i
    assign(paste0(i,"props"), x)
    plot_data<-rbind(plot_data,x)
  }
  names(plot_data)<-c("bin","condition","mean","se","low","up","object")
  plot_data$time<-plot_data$bin*bin_interval
  plot_data$object<-factor(plot_data$object)
  rm(x)
  return(plot_data)
}
