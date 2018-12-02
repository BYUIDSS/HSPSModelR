#' Find at risk customers
#'
#' Makes predictions using a list of models and returns a vector where each value represents the ratio of models which agree on a specified classification for that observation.
#'
#' @param x A list of models
#' @param factor the specific target for which wou want to pull the strongest common predictions.
#' @param test_data a df, the portion of data you are using to test your predictions.
#' @param threshold a value between 0 and 1. What ratio of columns do you need to agree your target factor?
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


get_common_predictions <- function(x, test_data, factor, threshold) {
  if (!is.data.frame(test_data) | !is_tibble(test_data)) {
    stop("test_data needs to be a data.frame or tibble")
  } else {
    test <- test_data
  }

  make_predictions <- function(i) {
    p <- predict(i, test, type = "raw")

    if (!is.factor(p)) {
      stop("all models must be able to produce predictions using stats::predict()")
    }

    if (str_detect(str_c(p, collapse = "|"), factor) == FALSE) {
      stop("factor must be in the target column you used to train your models")
    } else {
      p %>%
        as.factor() %>%
        as.tibble() %>%
        transmute(new = case_when(
          value %in% factor ~ 1,
          TRUE ~ 0
        ))
    }
  }

  predictions_array <- map_dfc(x, make_predictions)

  if (threshold > 1 | threshold < 0) {
    stop("threshold must be an integer between 0 and 1")
  }
  else {
    common_predictions <- predictions_array %>%
      transmute(percent_common = (rowSums(predictions_array) / ncol(predictions_array))) %>%
      filter(percent_common >= threshold)
  }

  return(common_predictions)
}


