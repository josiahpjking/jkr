mod_plot <- function(df, dv, iv, mdl){
  new_plot <- df %>% mutate(
    fitted = fitted(mdl)
  ) %>% make_tcplot_data(., AOIs=c("fitted",dv),predictor=iv) %>% 
    tcplot(.,xmin=min(.[,"time"]), xmax=(max(.[,"time"])),ltype="AOI",lcol=iv)+ylim(-2,2)
  
  return(new_plot)
}
