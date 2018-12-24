#' Extract model performance
#'
#' Generates tibble of performance measures by model, measure and arranged by
#'   score
#'
#' @param models List of models of class \code{train}
#' @param test_x `data.frame` or `tibble` of explanitory variables
#' @param test_y vector of target variable
#' @param format "long" for long format (default), or "wide" for wide format
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
#'  \item model model name
#'  \item measure machine learning metrics
#'  \item score double, usually a range between 0 and 1
#' }
#' When \code{format = "wide"}, the function will return a \code{tibble} including columns:
#' \itemize{
#'  \item Type
#'  \item Accuracy - Overall performance of model
#'  \item Accuracy Lower
#'  \item Accuracy Null
#'  \item Accuracy P value
#'  \item Accuracy Upper
#'  \item Balanced Accuracy
#'  \item Detection Prevalence
#'  \item Detection Rate
#'  \item F1 - Hybrid metric usefull fr unbalanced classes
#'  \item Kappa - Compares an observed accuracy with an expected accuracy
#'  \item McNemar P Value
#'  \item Negagive Prediction Value
#'  \item Positive Predictive Value
#'  \item Precision - How accurate the positive predictions are
#'  \item Prevalence
#'  \item Recall - True positive rate, number of instances from the positive class
#'    that actually predictoed correctly
#'  \item Sensitivity - Same as recall
#'  \item Specificty - Number of instances from the negative class that were
#'    actually predicted correctly
#'  \item Method the algorithm used to train each particular model
#' }
#'

#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[HSPSModelR]{extract_measures}
#'
#' @examples
#' \dontrun{
#' # Long format
#' p <- get_performance(models_list, test_x, test_y)
#' p
#' p %>% filter(measure == "F1")
#'
#' # Wide format
#' get_performance(models_list, test_x, test_y, format = "wide")
#' }
get_performance <- function(models, test_x, test_y, format = "long") {

  if (!is.list(models)) {
    stop("x needs to be a list of models")
  }

  result <- map_dfr(models, extract_measures, test_x, test_y) %>%
    arrange(measure, desc(score)) %>%
    select(method, measure, score)

  if (format == "wide") {
    result <- result %>% spread(measure, score)
  }

  return(result)
}
