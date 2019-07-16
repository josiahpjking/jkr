require(tidyverse)
tibble(
  p_id = factor(seq(1:50)),
  p_score = round(runif(50,1,100))
) %>% print -> df
nr_rels = sample(14:19,50,prob=c(0.2,0.4,0.3,0.1),replace=T)
#nr_rels = sample(17:20,50,prob=c(0.2,0.4,0.3,0.1),replace=T)
df<-df[rep(seq_len(nrow(df)), nr_rels),]

df %>% mutate(
  f_score = p_score+rnorm(nrow(df),10,25)
) -> df
df %>% group_by(p_id) %>% mutate(rel_nr = letters[(sequence(n()))]) %>% ungroup -> df

df %>% spread(.,rel_nr,f_score) -> df_wd

###########
#mkay, so we've now got our data in two formats, long n wide.
#not sure which is better really..

ggplot(df,aes(x=f_score,y=p_score))+geom_point()+stat_smooth(method=lm,se=F)
lm(p_score~f_score,df) %>% summary
#no grouping by patient/family, so essentially double counting data? or is it?..
ggplot(df,aes(x=f_score,y=p_score,col=p_id))+geom_point()+stat_smooth(method=lm,se=F)+
  theme(legend.position = "none")
ggplot(df,aes(y=f_score,x=p_score,col=p_id))+geom_point()+stat_smooth(method=lm,se=F)+
  theme(legend.position = "none")


df %>%
  group_by(p_id) %>%
  summarise(
    m=mean(f_score),
    v=var(f_score)
  ) -> mvs
mvs %>% ggplot(aes(x=v))+geom_density()
mvs %>% arrange(desc(v))
mvs %>% filter(v<700) %>% pull(p_id) -> pp
mvs %>% filter(v<700) %>% ggplot(aes(x=v))+geom_density()

#require(lme4)
#df$p_score<-sort(runif(nrow(df),50,100))
m0<-lm(f_score~p_score,df)
summary(m0)$coefficients
require(lme4)
m1<-lmer(f_score~p_score+(1|p_id),df)
summary(m1)$coefficients
m1b<-blme::blmer(f_score~p_score+(1|p_id),df,control=lmerControl(optimizer="Nelder_Mead"))
#allFit(m1b) %>% summary
summary(m1b)$coefficients
jkr::ggCaterpillar(ranef(m1b,condVar=T))

#df$p_score=runif(nrow(df),50,100)
ggplot(df,aes(x=f_score,y=p_score))+geom_point()+stat_smooth(method=lm,se=F)
m0<-lm(p_score~f_score,df)
require(nlme)
mm<-gls(p_score~f_score,data=df,correlation=corSymm(form=~1|p_id),weights=varIdent(~1|p_id))
summary(mm)
summary(m0)


model2 <- gls(p_score ~ f_score,
              weights = varIdent(form = ~ 1 | p_id),data = df, method = "REML",control = list(singular.ok = TRUE))
summary(model2)

df %>% ungroup %>% mutate(
  f0=fitted(m0),
  fm=fitted(mm)
) %>%
  ggplot(aes(y=p_score))+
  facet_wrap(~p_id)+
  theme(legend.position = "none")+
  geom_point(aes(x=f_score),alpha=.4,col="green")+
  geom_vline(aes(xintercept=f0),col="black")+
  geom_vline(aes(xintercept=fm),col="red",lty="dashed")+
  NULL



m2<-lmer(f_score~p_score+(1|p_id),filter(df,p_id%in%pp))
summary(m1)$coefficients
summary(m2)$coefficients
jkr::ggCaterpillar(ranef(m1,condVar=T))
jkr::ggCaterpillar(ranef(m2,condVar=T))

ggplot(df,aes(y=f_score,x=p_score,col=p_id))+geom_point()+stat_smooth(method=lm,se=F)+
  theme(legend.position = "none")
ggplot(filter(df,p_id%in%pp),aes(y=f_score,x=p_score,col=p_id))+geom_point()+stat_smooth(method=lm,se=F)+
  theme(legend.position = "none")






df_wd %>% rowwise() %>%
  mutate(av = mean(c(a,b,c,d),na.rm=T)) %>%
  ggplot(.,aes(x=av,y=p_score))+geom_point()+stat_smooth(method=lm,se=F)
#problem with this is that relatives are not of the same degree (e.g. child/sibling/cousin)

#a weighted average maybe?
#or a WLS regression?
