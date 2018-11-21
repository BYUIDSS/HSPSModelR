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
#' @author "Chad Schaeffer <sch12059@@byui.edu>"
#' @export

make_table <- function(x, test_data, predict_var) {
  dat <- test_data
  truth <- predict_var
  make_row <- function(i) {
    p <- predict(i, dat)
    t <- confusionMatrix(table(p, truth))

    as.data.frame(t[4]) %>%
      rownames_to_column(var = "measure") %>%
      rename_at("byClass", ~ "name") %>%
      bind_rows((as.data.frame(t[3])) %>%
                  rownames_to_column(var = "measure") %>%
                  rename_at("overall", ~ "name")) %>%
      mutate(type = "name") %>%
      spread(measure, name) %>%
      mutate(method = i[1]) %>%
      select(-type)
  }
  row_list <- lapply(x, make_row)
  table <- data.frame(matrix(unlist(row_list),
                             nrow = length(row_list),
                             ncol = max(vapply(row_list, length, 0)),
                             byrow = TRUE))
  names <- c("Accuracy", "AccuracyLower", "AccuracyNull", "AccuracyPValue", "AccuracyUpper",
             "Balanced Accuracy", "Detection Prevalence", "Detection Rate", "F1",
             "Kappa", "McnemarPValue", "Neg Pred Value", "Pos Pred Value",
             "Precision", "Prevalence", "Recall", "Sensitivity", "Specificity", "Method")
  colnames(table) <- names
  return(table)
}
make_table(list, test, test$Truth)
