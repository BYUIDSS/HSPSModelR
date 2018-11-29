#' Make a machine learning diagnostics table
#'
#' Takes a list of trained machine learning models and returns diagnostics as a data frame, as to compare the effectiveness of algorithms. Measures include Accuracy, Prevalence, Detection Rate, F1, Cohen's Kappa, McNemar P-Value, Negative and Positive Predictive value, Precision, Recall, Sensitivity, and Specificity
#'
#' @param x A list of models
#' @param test_data portion of data you are using to test your predictions.
#' @param predict_var the true values which you are comparing to your predicted values.
#'
#' @import caret
#' @import dplyr
#' @import tibble
#' @import stats
#'
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
#'  \item Method
#' }
#'
#' @author "Chad Schaeffer <sch12059@@byui.edu>

make_table <- function(x, test_data, target_column) {
  if (!is.list(x)) {
    stop("x needs to be a list of models")
  }

  if (!is.data.frame(test_data) | !is_tibble(test_data)) {
    stop("x needs to be a data.frame or tibble")
  } else {
    test <- test_data
  }

  if (class(target_column) == "character" | class(target_column) == "string") {
    stop("target column must be from test data. Use test_data$target_column")
  } else if (!is.factor(target_column)) {
    stop("targets must be a vector of factors the same length as test data")
  } else {
    targets <- target_column
  }

  make_row <- function(i) {
    p <- predict(i, test)

    if (!is.factor(p)) {
      stop("all models must be able to produce predictions using stats::predict()")
    }

    if (length(p) == length(targets)) {
      t <- confusionMatrix(p, targets)
    } else {
      stop("test data and target vector must be the same length")
    }

    as.data.frame(t[4]) %>%
      rownames_to_column(var = "measure") %>%
      rename_at("byClass", ~ "name") %>%
      bind_rows((as.data.frame(t[3])) %>%
                  rownames_to_column(var = "measure") %>%
                  rename_at("overall", ~ "name")) %>%
      mutate(method = i[1]) %>%
      spread(measure, name)
      }

  return(map_dfr(x, make_row))
}

