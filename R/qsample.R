#' quick sample of .. see https://www.tjmahr.com/sample-n-groups/
#' @param df dataframe/tibble
#' @param size size of sample, passed to sample() function.
#' @param ... grouping variables. samples from all available combinations
#' @export
#' @examples
#' qsample(df,4,sub_id) %>% ggplot(......
#' qsample(df,4,sub_id, trial_id) %>% ggplot(......
qsample <- function(data, size, ...) {
  group_ids <- data %>%
    group_by(...) %>%
    group_indices()

  sampled_groups <- sample(unique(group_ids), size)

  data %>%
    filter(group_ids %in% sampled_groups)
}
