require(tidyverse)
require(jkr)
e8_et<-readRDS("Desktop/git_repositories/phd_working/e7e8_gcd/e8_gcdv2/gcd_eyes.RDS") %>% filter(clicked!="none") %>%
  filter(!(sub %in% c(1,8,18))) %>%
  mutate(ref=referent,vid_type=fct_relevel(gesture,"No Gesture"))

make_tcplotdata(e8_et, c(ref_fix,dis_fix,vid_fix),sub,gesture) %>%
  tcplot(lty=gesture)


e8_et %>% filter(CURRENT_BIN<=100) -> e8_et

df<-e8_et1

summy = function(x) x %>% group_by(AOI,gesture,CURRENT_BIN) %>%
  summarize(smean=mean(prop))

tibble(
  subjsample=future_map(1:10, ~sample(distinct(df,sub) %>% pull(sub),replace=T)),
  data=map(subjsample,~map_dfr(.,~filter(df,sub==.x)) %>%
             gather(key="AOI",value="prop",ref_fix,dis_fix,vid_fix) %>%
             group_by(AOI,gesture,CURRENT_BIN) %>%
             summarize(smean=mean(prop)))
) %>% pull(data) %>% plyr::ldply()


BSmake_tcplotdata<-function(df,AOIs,subj,n=100,...,bin=CURRENT_BIN, bin_interval=20){
  subj=enquo(subj)
  bin=enquo(bin)
  AOIs=enquo(AOIs)

  summy <- function(x){x %>% group_by(AOI,...,!!bin) %>% summarise(smean=mean(prop))}

  samples <- tibble(
      subjsample=map(1:n, ~sample(distinct(df,!!subj) %>% pull(!!subj),replace=T)),
      meanprop=map(subjsample,~map_dfr(.,~filter(df,!!subj==.x)) %>%
                     gather(key="AOI",value="prop",!!AOIs) %>%
                     summy)
  ) %>%
    pull(meanprop) %>%
    plyr::ldply() %>%
    group_by(AOI,...,!!bin) %>%
    summarise(
      bs_SE=qnorm(.975)*sd(smean)
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
      time=!!bin*bin_interval
    )
}


e8_et1<-e8_et %>% filter(CURRENT_BIN<=10)

system.time(make_tcplotdata2(e8_et,c(ref_fix,dis_fix,vid_fix),sub,n=50,gesture) -> tcf)
tcf$meanprop
tcplot(tc)+facet_wrap(~gesture)

make_tcplotdata(e8_et,c(ref_fix,dis_fix,vid_fix),subj=sub,gesture) ->tc2
tcplot()+facet_wrap(~gesture)


left_join(tc2,tc %>% select(gesture,CURRENT_BIN,AOI,bs_SE)) %>% mutate(
  high=ifelse(bs_SE>se,1,0),
  low=ifelse(bs_SE<se,1,0)
) %>%
  ggplot(.,aes(x=se,y=bs_SE))+geom_point()


tc2 %>% mutate(orse=se,ormp=mean_prop) %>% select(-low,-up,-mean_prop) %>%
  left_join(.,tc) -> compd
plot(compd$ormp,compd$mean_prop)
plot(compd$orse,compd$bs_SE)

BlandAltmanLeh::bland.altman.plot(compd$orse,compd$bs_SE)
tcplot(tc2)+facet_wrap(~gesture)

make_tcplotdata(e8_et,c(ref_fix,dis_fix,vid_fix),sub,gesture) -> tc2

