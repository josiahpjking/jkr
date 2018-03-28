mod_plot2 <- function(df, dv, iv, mdl){
  require(AICcmodavg)
  elog = df %>% make_tcplot_data(., AOIs=c(dv),predictor=iv) %>% 
    mutate(
      time_s = time/1000
    )
  preds<-predictSE(mdl, elog, se.fit = TRUE, print.matrix = TRUE, level = 0, type = "response")
  fits = elog %>%
    mutate(
      AOI = "fitted",
      mean_prop = preds[,1],
      low = preds[,1]-preds[,2],
      up = preds[,1]+preds[,2]
    )
  new_plot = bind_rows(elog, fits) %>% 
    tcplot(.,xmin=min(.[,"time"]), xmax=(max(.[,"time"])),ltype="AOI",lcol=iv)+ylim(-2,2)
  return(new_plot)
}

