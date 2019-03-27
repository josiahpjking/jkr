require(tidyverse)
tibble(
  p_id = factor(seq(1:50)),
  p_score = round(runif(50,1,100))
) %>% print -> df

nr_rels = sample(1:4,50,prob=c(0.2,0.4,0.3,0.1),replace=T)
df<-df[rep(seq_len(nrow(df)), nr_rels),]

df %>% mutate(
  f_score = p_score+rnorm(nrow(df),10,25)
) -> df
df %>% group_by(p_id) %>% mutate(rel_nr = letters[(sequence(n()))]) -> df

df %>% spread(.,rel_nr,f_score) -> df_wd

###########
#mkay, so we've now got our data in two formats, long n wide. 
#not sure which is better really..

ggplot(df,aes(x=f_score,y=p_score))+geom_point()+stat_smooth(method=lm,se=F)
lm(p_score~f_score,df) %>% summary
#no grouping by patient/family, so essentially double counting data? or is it?.. 

ggplot(df,aes(x=f_score,y=p_score,col=p_id))+geom_point()+stat_smooth(method=lm,se=F)+
  theme(legend.position = "none")

df_wd %>% rowwise() %>%
  mutate(av = mean(c(a,b,c,d),na.rm=T)) %>%
  ggplot(.,aes(x=av,y=p_score))+geom_point()+stat_smooth(method=lm,se=F)
#problem with this is that relatives are not of the same degree (e.g. child/sibling/cousin)

#a weighted average maybe?
#or a WLS regression?
