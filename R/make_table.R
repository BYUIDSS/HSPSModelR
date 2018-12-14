#' Make a machine learning diagnostics table
#'
#' Takes a list of trained machine learning models and returns diagnostics as a data frame as to compare the effectiveness of algorithms. Measures include Accuracy, Prevalence, Detection Rate, F1, Cohen's Kappa, McNemar P-Value, Negative and Positive Predictive value, Precision, Recall, Sensitivity, and Specificity
#'
#' @param models A list of models of class `train`
#' @param test_x `data.frame` or `tibble`. explanitory variables
#' @param test_y `vector` target variable
#'
#' @importFrom caret confusionMatrix
#' @importFrom dplyr select filter mutate rename_at bind_rows
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom stats predict
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @export
#'
#' @return This function returns a \code{data.frame} including columns:
#' \itemize{
#'  \item Type
#'  \item Accuracy
#'  \item Accuracy Lower
#'  \item Accuracy Null
#'  \item Accuracy P value
#'  \item Accuracy Upper
#'  \item Balanced Accuracy
#'  \item Detection Prevalence
#'  \item Detection Rate
#'  \item F1
#'  \item Kappa
#'  \item McNemar P Value
#'  \item Negagive Prediction Value
#'  \item Positive Predictive Value
#'  \item Precision
#'  \item Prevalence
#'  \item Recall
#'  \item Sensitivity
#'  \item Specificty
#'  \item Method; the algorithm used to train each particular model
#' }
#'
#' @author "Chad Schaeffer <sch12059@@byui.edu>
#'
#' @examples
#'
#' \dontrun{
#' make_table(models_list, test_x, test_y)
#' }
make_table <- function(models, test_x, test_y) {

  if (!is.list(models)) {
    stop("x needs to be a list of models")
  }

  make_row <- function(i) {
    p <- predict(i, test_x)

    if (!is.factor(p)) {
      stop("all models must be able to produce predictions using stats::predict()")
    }

      t <- confusionMatrix(p, test_y)

      as.data.frame(t[4]) %>%
        rownames_to_column(var = "measure") %>%
        rename_at("byClass", ~ "name") %>%
        bind_rows((as.data.frame(t[3])) %>%
                    rownames_to_column(var = "measure") %>%
                    rename_at("overall", ~ "name")) %>%
        mutate(method = i[1]) %>%
        spread(measure, name)
    }

    return(map_dfr(models, make_row) %>% as_tibble())
}
