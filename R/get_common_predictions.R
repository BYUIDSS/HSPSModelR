#' Find at risk ID's
#'
#' Makes predictions using a list of models and returns a vector where each value
#'   represents the ratio of models which agree on a specified classification for
#'   that observation.
#'
#' When ID's arent present in the data, example ID's are generated.
#'
#' @param models A list of models of class `train`
#' @param test_x `data.frame` or `tibble`. explanitory variables from the test set
#' @param threshold a numeric value between 0 and 1. What ratio of columns do you
#'   need to agree your target factor?
#' @param id_col string of ID column. If there are no IDs, row numbers will be
#'   assigned before filtering out uncertain rows.
#'
#' @importFrom stringr str_replace_all str_detect
#' @importFrom stats predict
#' @importFrom purrr map_dfc
#' @importFrom dplyr transmute filter case_when
#' @importFrom magrittr %>%
#' @importFrom tibble is_tibble as_tibble
#' @export
#'
#' @return \code{tibble}
#'
#' @author "Chad Schaeffer <sch12059@@byui.edu>",
#'
#' @examples
#' \dontrun{
#'
#' get_common_predictions(models    = models_list,
#'                        test_x    = test_x,
#'                        threshold = 0.70)
#'
#' }
get_common_predictions <- function(models,
                                   test_x,
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


  predictions_array <-
    caretEnsemble:::predict.caretList(models, newdata = test) %>%
    as_tibble()

  result <- predictions_array %>%
    mutate(agreeance = (rowSums(predictions_array) / ncol(predictions_array))) %>%
    bind_cols(ID) %>%
    select(ID, agreeance, everything()) %>%
    filter(agreeance >= threshold) %>%
    arrange(desc(agreeance))

  return(result)
}
