#' Pre-process cognostic data
#'
#' Generates a tibble with features optimized for machine learning
#'
#' This is the details section
#'
#' @param x A data frame or tibble.
#' @param y Class column
#' @param corr.cutoff Correlation coeff to exclude variables having correlation
#'   above but not to exceed 1
#' @param freqCut [Insert def from caret]
#' @param uniqueCut [Insert def from caret]
#' @param impute.method Impute by "knn" or "mean"
#' @import dplyr
#' @import caret
#' @import DMwR
#' @import naniar
#' @export
#' @examples
#' data_summary(iris)
#' data_summary(airquality, na.rm = FALSE)
#'
#' @return This function returns a \code{tibble} of optimized features
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[base]{summary}
numeric_summary <-
  function(x, na.rm = FALSE) {

    # Include an error if x is not numeric
    if (!is.numeric(x)) {
      print("Data must be numeric")
      stop()
    }

    # Create data frame
    data.frame( min = min(x, na.rm = na.rm),
                median = median(x, na.rm = na.rm),
                sd = sd(x, na.rm = na.rm),
                max = max(x, na.rm = na.rm))
  }
