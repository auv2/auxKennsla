#' read_md
#'
#' @param file_name
#'
#' @return
#' @export
#'
#' @examples
read_md <- function(file_name){

  breakFun <- function(x){
    #function to replace empty lines with newline.
    if(nchar(x) == 0){
      return("\n\n") #double newline to give sam\e space as in the .md-file
    } else {
      return(x)
    }
  }

  text <- readLines(file_name)

  #cat(paste0(lapply(text, FUN=function(x) breakFun(x)), collapse=""))
  text
}

#' get_files
#'
#' @param path
#' @param exclude_pattern
#' @param include_pattern
#'
#' @return
#' @export
#'
#' @examples
get_files <- function(path,
                      exclude_pattern = "Drawing",
                      include_pattern = "0[0-9]-") {
  files <- list.files(path, full.names = T)
  files <- files[!grepl(files, pattern = exclude_pattern)]
  files <- files[grepl(files, pattern = include_pattern)]
}


#text <- purrr::map(get_files(path), ~read_md(.x))

