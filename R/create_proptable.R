#' proportion tibbles
#' @param freq categorical response variable (e.g. frequency data)
#' @param var1 grouping variable accross which frequency variable gets proportioned of a total n in that group.
#' @export
#' @examples
#' proptables<-create_proptable("stroke","age_group",data))
#' ggplot(proptables, aes(x=stroke, y=prop,fill=stroke)) + 
#' ylim(0,1)+
#' geom_bar(position=position_dodge(), stat="identity")+facet_wrap(~(age_group))+
#' geom_errorbar(aes(ymin=intlow, ymax=inthigh),width=.2,position=position_dodge(.9))
#' 
#' or, just for response of 1 (i.e. just plot bar for occurrences of stroke, not no-occurrence of stroke)
#' ggplot(proptables[proptable$stroke==1,], aes(x=age_group, y=prop)) + 
#' ylim(0,1)+
#' geom_bar(position=position_dodge(), stat="identity")+
#' geom_errorbar(aes(ymin=intlow, ymax=inthigh),width=.2,position=position_dodge(.9))

create_proptable<-function(freq,var1,data){
  table.t <- data %>% group_by(get(var1)) %>% summarise (totaln=n())
  table.p <- data %>% group_by(get(var1),get(freq)) %>%
    summarise (n=n()) %>%
    mutate(
      prop = n/sum(n))
  table.f <- left_join(table.t,table.p)
  table.f$intlow<-0
  table.f$inthigh<-0
  names(table.f)[1]<-var1
  names(table.f)[3]<-freq
  for (i in 1:nrow(tf)){
    table.f$intlow[i]<-prop.test(table.f$n[i],table.f$totaln[i])$conf[1]
    table.f$inthigh[i]<-prop.test(table.f$n[i],table.f$totaln[i])$conf[2]
  }
  return(table.f)
}
