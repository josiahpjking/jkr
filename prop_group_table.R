#' This a function which returns in group proportions for levels of outcome variables.
#' returns counts as a proportion of the lowest group level. 
#' e.g. If you have multiple grouping variables (age and gender), then it will return the proportion of outcome levels (A & B) from the total number for each grouping of age and gender. (so proportion of As in male, 16-24, then proportion of As in Female 16-24, etc..)
#' 
#' @param df data frame
#' @param outcome outcome variable
#' @param ... grouping variable names
#' @param confint Whether or not you want 95% confidence intervals produced (uses prop.test). defaults to TRUE
#' @param useNA whether or not NAs should be included. passed to table(). useNA controls if the table includes counts of NA values: the allowed values correspond to never ("no"), only if the count is positive ("ifany") and even for zero counts ("always").
#' @export
#' @examples
#' prop_group_table(mydata, stroke, age, gender, ... , includeNA="ifany")

prop_group_table<-function(df, outcome,...,includeNA="no",confint=TRUE){
	outcome = enquo(outcome)
	group_variables = enquos(...)
	
	df %>% group_by(!!!group_variables) %>%
	dplyr::summarise(
	proportion = list(prop.table(table(!!outcome,useNA=includeNA))),
	level = list(names(table(!!outcome,useNA=includeNA))),
	groupN = sum(table(!!outcome,useNA=includeNA))
	) %>%
	tidyr::unnest() -> ptab
	
	if(confint==TRUE){
	    ptab %>%
	    rowwise() %>%
	      mutate(
	        ci_low = prop.test(proportion*groupN, 	groupN)$conf.int[1],
	        ci_high = prop.test(proportion*groupN, 	groupN)$conf.int[2]
	      ) -> ptab
	}
  return(ptab)
}
