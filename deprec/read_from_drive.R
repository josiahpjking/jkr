#' read's csv from drive file.
#' @param id_pattern regex pattern to partial match filename in drive.
#' @export
#' @examples
#' read_from_drive("E6")
read_from_drive <- function(id_pattern="null"){
  require(googledrive)
  listfiles=drive_find(pattern = id_pattern)
  lapply(listfiles$id, function(x) drive_download(file=as_id(x), overwrite=TRUE))
  lapply(listfiles$name, function(x) assign(x, read.csv(x),envir = .GlobalEnv))
  lapply(listfiles$name, function(x) file.remove(x))
}





