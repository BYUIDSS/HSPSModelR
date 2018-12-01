#' Find at risk customers
#'
#' Makes predictions using a list of models and returns a vector where each value represents a customer which has been predicted to drop by a pre-specified percentage of models.
#'
#' @param x A list of models
#' @param test_data a df, the portion of data you are using to test your predictions.
#' @param threshold a value between 0 and 1. What ratio of columns do you need to agree on the "Dropped" value
#'
#' @importFrom stringr str_replace_all
#' @importFrom stats predict
#' @importFrom purrr map_dfc
#' @importFrom dplyr transmute filter
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
#'
#' @return This function returns a \code{tibble}, a single column reporting the ratio of like predictions for dropped clients meeting the prediction threshold
#'
#' @author "Chad Schaeffer <sch12059@@byui.edu>"



get_likely_drops <- function(x, test_data, threshold) {
  dat <- test_data

  make_predictions <- function(i) {
    predict(i, dat, type = "raw") %>%
      str_replace_all(c("Healthy" = "0", "Dropped" = "1")) %>%
      as.integer()
  }

  predictions_array <- map_dfc(x, make_predictions)
  likely_drops <- predictions_array %>%
    transmute(percent_dropped = (rowSums(predictions_array) / ncol(predictions_array))) %>%
    filter(.data$percent_dropped >= threshold)
  return(likely_drops)
}
