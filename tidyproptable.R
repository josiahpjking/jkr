#' This a function which returns in group proportions for levels of outcome variables.
#' returns counts as a proportion of the lowest group level. 
#' e.g. If you have multiple grouping variables (age and gender), then it will return the proportion of outcome levels (A & B) from the total number for each grouping of age and gender. (so proportion of As in male, 16-24, then proportion of As in Female 16-24, etc..)
#' 
#' @param outcome string or character vector of outcome variable names
#' @param group_vars string or character vector of grouping variable names
#' @param df data frame
#' @param confint Whether or not you want 95% confidence intervals produced (uses prop.test). defaults to TRUE
#' @param useNA whether or not NAs should be included. useNA controls if the table includes counts of NA values: the allowed values correspond to never ("no"), only if the count is positive ("ifany") and even for zero counts ("always").
#' @export
#' @examples
#' new_proptable("stroke",c("age","gender"),mydata)
#' new_proptable(c("stroke","diabetes"),c("age","gender"),mydata)
new_proptable<-function(outcome, group_vars, df, confint=TRUE,useNA="no"){
  bind_rows(lapply(outcome, function(x)
    df %>% group_by_(.dots = group_vars) %>%
      summarise_at(vars(one_of(x)),
                   funs(proportion = list(prop.table(table(.,useNA=useNA))),
                        level = list(names(table(.,useNA=useNA))),
                        groupN = sum(table(.,useNA=useNA))
                   )
      ) %>% 
      mutate(outcome=x) %>% 
      unnest()
  )) -> ptab
  if(confint==TRUE){
    ptab %>%
    rowwise() %>%
      mutate(
        ci_low = prop.test(proportion*groupN, groupN)$conf.int[1],
        ci_high = prop.test(proportion*groupN, groupN)$conf.int[2]
      ) -> ptab
  }
  return(ptab)
}