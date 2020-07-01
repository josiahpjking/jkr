#' get a quick list of all functions used in Rmd. Will exclude echo=FALSE chunks on premise that students aren't presented with these, so we often put more complicated plotting stuff in there.
#' @param filepath rmd document for lab/lecture
#' @export
#' @examples
#' uoepsy_rmd_funcs("01-slr.Rmd")
uoepsy_rmd_funcs <- function(filepath, excludechunks="echo=FALSE") {

  dropchunks <- function(scriptname, what.to.drop){
    script <- readLines(scriptname)
    script <- do.call(paste, list(script, collapse = "\n") )
    subpattern = paste0("(", do.call(paste, list(what.to.drop, collapse="|")), ")")
    mainpattern <- paste('(?s)## ((?!##).)*?', subpattern, '.*?((?=##)|$)', sep="")
    script<- gsub(pattern = mainpattern, replacement = "", x = script, perl=TRUE)
    writeLines(text = script, con= scriptname)
  }

  knitr::purl(filepath, output="tmp.R")
  dropchunks("tmp.R", excludechunks)
  code <- parse("tmp.R")
  tokens <- as.list(code)
  calls <- c()
  while (TRUE) {
    any_unpacked <- FALSE
    for (ii in seq_along(tokens)) {
      part <- tokens[[ii]]
      # Calls always have the function name as the first element
      if (is.call(part)) {
        fun_token <- part[[1]]
        calls <- c(calls, deparse(fun_token))
      }
      # Expressions have a length
      if (length(part) > 1) {
        tokens[[ii]] <- as.list(part)
        any_unpacked <- TRUE
      }
    }
    tokens <- unlist(tokens)
    if (!any_unpacked) break
  }
  file.remove("tmp.R")
  unique(calls)
}

