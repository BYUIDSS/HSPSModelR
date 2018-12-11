#' Extract model performance
#'
#' Generates tibble of performance measures by model, measure and arranged by score
#'
#' This is the details section
#'
#' @param models List of models of class \code{train}
#' @param test_x `data.frame` or `tibble` of explanitory variables
#' @param test_y vector of target variable
#'
#' @importFrom dplyr arrange desc select
#' @importFrom purrr map_dfr
#' @importFrom tidyr gather unnest
#' @importFrom rlist list.clean
#' @importFrom tibble is_tibble
#' @importFrom magrittr %>%
#' @importFrom caret varImp
#' @export
#'
#' @return This function returns a \code{tibble} of model performance including columns:
#' \itemize{
#'  \item model
#'  \item measure
#'  \item score
#' }
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[BYUImachine]{extract_measures}
get_performance <- function(models, test_x, test_y) {

  if (!is.list(models)) {
    stop("x needs to be a list of models")
  }

  result <- map_dfr(models, extract_measures, test_x, test_y) %>%
    arrange(measure, desc(score)) %>%
    select(method, measure, score)

  return(result)
}
