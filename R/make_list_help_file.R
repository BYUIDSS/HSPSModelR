#' Make a list of models
#'
#' Compiles the contents of a folder into a list. Intended for machine learning models, and for use with the BYUImachine::make_table function
#'
#' @param x source or file path
#' @importFrom stringr str_c
#' @importFrom rlist list.append
#' @importFrom readr read_rds
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @return This function returns a \code{list} of contents at specified location
#'
#' @author "Chad Schaeffer <sch12059@@byui.edu>, Brad Borget <bor13001@@byui.edu>"
#' @export

make_list <- function(x) {
  file.names <- dir(x, pattern = ".rds")
  list <- list()
  for (i in 1:length(file.names)) {

    file <- read_rds(str_c(x, file.names[i], collapse = ""))
    list <- list.append(list, file)

  }
  return(list)
}
