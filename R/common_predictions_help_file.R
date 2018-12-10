#' Find at risk customers
#'
#' Makes predictions using a list of models and returns a vector where each value represents the ratio of models which agree on a specified classification for that observation.
#'
#' @param x A list of models
#' @param factor the specific target for which wou want to pull the strongest common predictions.
#' @param test_x a df, the portion of data you are using to test your predictions.
#' @param threshold a value between 0 and 1. What ratio of columns do you need to agree your target factor?
#' @param id_col identify your ID column. If there are no IDs, row numbers will be assigned before filtering out uncertain rows.
#'
#' @importFrom stringr str_replace_all str_detect
#' @importFrom stats predict
#' @importFrom purrr map_dfc
#' @importFrom dplyr transmute filter case_when
#' @importFrom magrittr %>%
#' @importFrom tibble is_tibble as_tibble
#' @export
#'
#' @return This function returns a \code{tibble}, a single column reporting the ratio of like predictions for dropped clients meeting the prediction threshold
#'
#' @author "Chad Schaeffer <sch12059@@byui.edu>",


get_common_predictions <- function(x,
                                   test_x,
                                   factor,
                                   threshold,
                                   id_col = NULL) {

  if (!is.data.frame(test_x) | !is_tibble(test_x)) {
    stop("test_data needs to be a data.frame or tibble")
  } else {
    test <- test_x
  }

  if (is.null(id_col)) {
    ID <- c(1:nrow(test)) %>% as.tibble()
  } else {
    ID <- test %>% select(id_col)
    test <- test %>% select(-id_col)
  }

  ## Testing space, can we do below in a few maps?
  models_list %>% map(predict, test)
  predictions_array <- predict(models_list, newdata = test)
  ##

  # make_predictions <- function(i) {
  #   p <- predict(i, test)
  #
  #   if ( !is.factor(p[[1]]) ) {
  #     stop("all models must be able to produce predictions using stats::predict()")
  #   }
  #
  #
  #   if (str_detect(str_c(p[[1]], collapse = "|"), factor) == FALSE) {
  #     stop("factor must be in the target column you used to train your models")
  #   } else {
  #     p %>%
  #       as_tibble() %>%
  #       transmute(new = case_when(
  #         value == factor ~ 1,
  #         TRUE ~ 0
  #         ))
  #   }
  # }

  predictions_array <- map_dfc(models_list, make_predictions)

  if (threshold > 1 | threshold < 0) {
    stop("threshold must be an integer between 0 and 1")
  }
  else {
    ratios <- predictions_array %>%
      mutate(percent_common = (rowSums(predictions_array) / ncol(predictions_array))) %>%
      dplyr::select(percent_common)
  }

  common_predictions <- ratios %>%
    bind_cols(ID) %>%
    filter(percent_common >= threshold)

  return(common_predictions)
}

#get_common_predictions(models_list, test_x, "Dropped", 1)


