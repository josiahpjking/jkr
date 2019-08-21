#' time course plots for eye tracking computes avg proportion and BOOTSTRAPPED by subject 95\% CIs (for Dale).
#' @param df dataframe
#' @param AOIs columnnames of AOIs (for eyelink trackers, the RIGHT_1_P, RIGHT_2_P,... columns)
#' @param subj participant column
#' @param ... variable names of experimental conditions
#' @param bin BIN variable (for eyelink, this is the CURRENT_BIN column). Defaults to "CURRENT_BIN".
#' @param bin_interval the time of each bin
#' @param n bootstrap samples. so.. R really..
#' @param level confidence level, .95 by default
#' @export
#' @examples
#' plottingdata <- BSmake_tcplotdata(df,AOIs=c(refprop,disprop),subj=Participant,fluency,gesture,n=1000)
#' tcplot(plottingdata,0,2000,lty=fluency)+facet_wrap(~gesture)
BSmake_tcplotdata<-function(df,AOIs,subj,n=100,...,bin=CURRENT_BIN, bin_interval=20,level=.95){
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
      bs_SE=qnorm(1-((1-level)/2))*sd(smean)
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
    ) %>% ungroup
}
