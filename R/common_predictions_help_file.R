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

  if (!is.data.frame(test_x)) {
    stop("test_data needs to be a data.frame or tibble")

  } else test <- test_x


  if (is.null(id_col)) {
    ID <- c(1:nrow(test)) %>% as_tibble()
    names(ID) <- "ID"

  } else {
    ID   <- test %>% select(id_col)
    test <- test %>% select(-id_col)
  }


  if (threshold > 1 | threshold < 0) {
    stop("threshold must be an integer between 0 and 1")
  }


  predictions_array <- caretEnsemble:::predict.caretList(x, newdata = test) %>% as_tibble()
  result <- predictions_array %>%
    mutate(agreeance = (rowSums(predictions_array) / ncol(predictions_array))) %>%
    bind_cols(ID) %>%
    select(ID, agreeance, everything()) %>%
    filter(agreeance >= threshold)

  return(result)
}
