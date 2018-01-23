#' time course plots for eye tracking
#' @param df a dataframe in long format from the make_tcplot_data function
#' @param xmin,xmax min and max time boundaries. (ROI)
#' @param lcol = line colours variable.
#' @param ltype = line type variable.
#' @export
#' @examples
#' tcplot(plotting_data, -1000, 4000)
#' tcplot(plotting_data, -1000, 4000)+facet_wrap(~condition)
tcplot<-function(df,xmin=0,xmax=2000,lcol="darkred",ltype="solid"){
  lcol=eval(substitute(lcol),df)  
  ltype=eval(substitute(ltype),df)
  
  tplot = ggplot(data = df, aes(x = time, y = mean_prop, colour=lcol, fill=lcol, lty=ltype))+
    xlim(xmin, xmax) + ylim(0, 1) + 
    xlab("Time (ms)") + ylab("Proportion of fixations to AOIs.") + 
    geom_line(lwd = 1.5) + 
    geom_ribbon(data = df, aes(x = time, ymin = low, ymax = up), colour = NA, alpha = 0.2, lwd = 1.5)

  return(tplot)
}

