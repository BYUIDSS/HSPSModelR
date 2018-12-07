#' Extract model performance
#'
#' Generates tibble of performance measures by model, measure and arranged by score
#'
#' This is the details section
#'
#' @param list_of_models List of models of class \code{train}
#' @param pred data.frame or tibble of predictor variables
#' @param target vector of target variable
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
get_performance <- function(list_of_models, 
                            #measure = "Accuracy",
                            pred  = test_x,
                            target  = test_y) {
  
  #measure_sym <- rlang::sym(measure) 
  
  if (!is.list(list_of_models)) {
    stop("x needs to be a list of models")
  }
  if (!is.data.frame(pred) | !is_tibble(pred)) {
    stop("x needs to be a data.frame or tibble")
  }
  
  result <- map_dfr(list_of_models, extract_measures) %>% 
    arrange(measure, desc(score)) %>% 
    select(method, measure, score)
  
  return(result)
  
}