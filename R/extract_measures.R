#' Extract measures from list of models
#'
#' Generates tibble of measures
#'
#' This is the details section
#'
#' @param models List of models of class \code{train}
#' @param test_x `data.frame` or `tibble`. explanitory variables
#' @param test_y `vector` target variable
#' @importFrom dplyr rename bind_rows mutate
#' @importFrom tibble rownames_to_column as_tibble
#' @importFrom stats predict
#' @importFrom caret confusionMatrix
#'
#' @return This function returns a \code{tibble} of measures by model
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[caret]{confusionMatrix}
extract_measures <- function(models, test_x, test_y) {
  p <- predict(models, test_x)

  if (!is.factor(p)) {
    stop("all models must be able to produce predictions using stats::predict()")
  }

  cm <- confusionMatrix(p, test_y)

  t3 <- cm[[3]] %>%
    as.data.frame() %>%
    rownames_to_column(var = "measure") %>%
    as_tibble() %>%
    rename(score = ".")

  t4 <- cm[[4]] %>%
    as.data.frame() %>%
    rownames_to_column(var = "measure") %>%
    as_tibble() %>%
    rename(score = ".")

  table <- t3 %>%
    bind_rows(t4) %>%
    mutate(method = models[[1]][[1]])

  return(table)
}
