#' time course plots for eye tracking
#' @param objects vector of proportions of fixations to visual world AOIs (for eyelink trackers, the RIGHT_1_P, RIGHT_2_P,... columns)
#' @param predictor variable name (string) of experimental conditions.
#' @param xmin_time start time of plot. Defaults to 0.
#' @param xmax_time end time of plot. Defaults to 2000.
#' @param bin BIN variable (for eyelink, this is the CURRENT_BIN column). Defaults to "CURRENT_BIN".
#' @param bin_interval duration of each bin in ms. Defaults to 20.
#' @param data dataset
#' @param facet_var The variable by which to facet plots. Defaults to "none".
#' @export
#' @examples
#' plot_eyes(objects=c("easy_fix","diff_fix","vid_fix"),predictor="g_condition",xmin_time=0,xmax_time=4000,data=e6data,facet_var="predictor")

plot_eyes<-function(objects,predictor,xmin_time=0,xmax_time=2000,bin="CURRENT_BIN",bin_interval=20,data,facet_var="none"){
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
  if (facet_var=="object"){
    ggplot() +
      xlim(xmin_time,xmax_time) +
      ylim(0, 1) +
      ggtitle("") +
      xlab("Time (ms)") +
      ylab("Proportion of fixations to obj.") +
      geom_line(data=plot_data, aes(x=time,y=mean,color=condition), lwd=1.5)+
      geom_ribbon(data=plot_data, aes(x=time,ymin=low,ymax=up,fill=condition),color=NA,alpha=0.2, lwd=1.5)+
      theme(text = element_text(size=20))+
      facet_wrap(~object)
  } else if (facet_var=="predictor"){
    ggplot() +
      xlim(xmin_time,xmax_time) +
      ylim(0, 1) +
      ggtitle("") +
      xlab("Time (ms)") +
      ylab("Proportion of fixations to obj.") +
      geom_line(data=plot_data, aes(x=time,y=mean,color=object), lwd=1.5)+
      geom_ribbon(data=plot_data, aes(x=time,ymin=low,ymax=up,fill=object),color=NA,alpha=0.2, lwd=1.5)+
      theme(text = element_text(size=20))+
      facet_wrap(~condition)
  } else if (facet_var=="none"){
    ggplot() +
      xlim(xmin_time,xmax_time) +
      ylim(0, 1) +
      ggtitle("") +
      xlab("Time (ms)") +
      ylab("Proportion of fixations to obj.") +
      geom_line(data=plot_data, aes(x=time,y=mean,color=object), lwd=1.5)+
      geom_ribbon(data=plot_data, aes(x=time,ymin=low,ymax=up,fill=object),color=NA,alpha=0.2, lwd=1.5)+
      theme(text = element_text(size=20))
  }
}
