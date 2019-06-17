require(tidyverse)
e8_et<-readRDS("phd_working-master/phd_working-master/e7e8_gcd/e8_gcdv2/gcd_eyes.RDS") %>% filter(clicked!="none") %>%
  mutate(ref=referent,vid_type=fct_relevel(gesture,"No Gesture"))

make_tcplotdata(e8_et, c(ref_fix,dis_fix,vid_fix),sub,gesture) %>%
  tcplot(lty=gesture)


make_tcplotdata2(e8_et, c(ref_fix,dis_fix,vid_fix),sub,gesture) %>%
  tcplot(lty=gesture)



e8_et %>% filter(CURRENT_BIN<=100) -> e8_et

make_tcplotdata2<-function(df,AOIs,subj,n=100,...,bin=CURRENT_BIN, bin_interval=20){
  subj=enquo(subj)
  bin=enquo(bin)
  AOIs=enquo(AOIs)
  
  summy = function(x) x %>% group_by(AOI,...,!!bin) %>% 
    summarize(smean=mean(prop))
  
  samples = tibble(
      subjsample=map(1:n, ~sample(distinct(df,sub) %>% pull(sub),replace=T)),
      data=map(subjsample,~map_dfr(.,~filter(df,sub==.x))),
      tidydata=map(data,~gather(.,key="AOI",value="prop",!!AOIs)),
      meanprop=map(tidydata,summy)
  )
  
  samples$meanprop %>% plyr::ldply() %>%
    group_by(AOI,...,!!bin) %>%
    summarise(
      bs_SE=sd(smean)
    ) %>%
    left_join(.,
              df %>%
                gather(key="AOI",value="prop",!!AOIs) %>%
                group_by(AOI,...,!!bin) %>%
                summarise(
                  mean_prop=mean(prop)
                )
    ) %>%
    mutate(
      low=mean_prop-bs_SE,
      up=mean_prop+bs_SE,
      time=CURRENT_BIN*bin_interval
    )
}


make_tcplotdata2(e8_et,c(ref_fix,dis_fix,vid_fix),sub,n=500,gesture) -> tc
tcplot(tc,lty=gesture)


make_tcplotdata(e8_et,c(ref_fix,dis_fix,vid_fix),sub,gesture) -> tc2
tcplot(tc2,lty=gesture)


