require(tidyverse)

caz_facplot<-function(df,outcome,lev,groups){
  outcome=enquo(outcome)
  groups=enquo(groups)
  df %>% group_by(!!groups) %>%
    summarise(
      nyes=sum(!!outcome==lev),
      n=n(),
      pyes=nyes/n
    ) %>% mutate(
      ptest=map2(nyes,n,prop.test),
      cis=map(ptest,"conf.int"),
      lwr=map_dbl(cis,1),
      upr=map_dbl(cis,2)
    ) %>%
    ggplot(.,aes(x=!!groups,y=pyes,ymin=lwr,max=upr,fill=!!groups))+
    ylab(outcome)+
    geom_bar(stat="identity")+
    geom_errorbar()
}

caz_contplot<-function(df,outcome,groups){
  outcome=enquo(outcome)
  groups=enquo(groups)
  df %>% group_by(!!groups) %>%
    summarise(
      mean_val=mean(!!outcome),
      se_m=sd(!!outcome)/sqrt(n()),
      lwr=mean_val-se_m,
      upr=mean_val+se_m
    ) %>%
    ggplot(.,aes(x=!!groups,y=mean_val,ymin=lwr,max=upr,fill=!!groups))+
    ylab(outcome)+
    geom_errorbar()+
    geom_point(shape=21,size=3)
}

#example plots.
df2<-tibble(
  ynum=runif(100,1,10),
  yfac=factor(sample(0:1,100,replace=T)),
  g=rep(letters[1:4],25)
)
#the %>% print bits are just so you can see it as well as save it as an object.
plot_numeric <- caz_contplot(df2,ynum,g) %>% print
plot_fact <- caz_facplot(df2,yfac,"1",g) %>% print

