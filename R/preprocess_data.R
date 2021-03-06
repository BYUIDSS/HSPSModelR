#' Pre-process cognostic data
#'
#' Generates a tibble with features optimized for machine learning
#'
#' Data is often messy and needs to be cleaned prior to use in machine learning.
#'   \code{preprocess_data} can help with this but isn't a complete solution.
#'   Reguardless of argument specification, this function will ungroup data if
#'   grouped, and turn \code{Inf} values into NA. Beyond that, the user can
#'   specify whether to convert their target variable into a factor (default),
#'   or convert to 0 and 1 with \code{factor_y = FALSE}; whether to impute NA's
#'   using mean, knn, or replace with 0 (default) using \code{impute}; and whether
#'   to reduce columns with \code{reduce_cols}. When \code{reduce} is set to
#'   \code{TRUE}, \code{freq_cut} and \code{unique_cut} can also bet set to
#'   exclude more or less columns. See the argument definitions in
#'   \link[caret]{nearZeroVar} for further information.
#'
#' @param x data frame or tibble.
#' @param target classifier column
#' @param reduce_cols lgl `TRUE`: Columns are reduced based on near zero
#'   variance and correlation; FALSE = Nothing
#' @param factor_y `FALSE`: Recodes pred to 0 and 1; `TRUE` = Recodes pred to
#'   factor
#' @param impute character Impute NA by "knn","mean","zero"
#' @param corr_cutoff Corelation coefficient level to cut off highly correlated
#'   columns, devaulted to .90
#' @param freq_cut the cutoff for the ratio of the most common value to the
#'   second most common value
#' @param unique_cut the cutoff for the percentage of distinct values out of the
#'   number of total samples (knn takes substantially longer to compute, zero
#'  replaces NA with 0)
#' @param k the number of nearest neighbours to use for impute (defaults to 10)
#' @param prepro_methods string or vector of strings of preprocessing methods
#' @importFrom DMwR knnImputation
#' @importFrom caret findCorrelation nearZeroVar
#' @importFrom naniar impute_mean_if all_complete any_na
#' @importFrom purrrlyr dmap
#' @importFrom wrapr let
#' @importFrom tibble is_tibble as_tibble rownames_to_column
#' @importFrom dplyr select filter ungroup na_if bind_cols everything select_if
#' recode
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom tidyr gather spread
#' @importFrom magrittr %>%
#' @importFrom stats cor
#' @export
#'
#' @return This function returns a \code{tibble} of optimized features
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[caret]{preProcess}, \link[caret]{nearZeroVar},
#'   \link[caret]{findCorrelation}, \link[stats]{cor}
#'
#' @examples
#'
#' \dontrun{
#' \donttest{
#' library(caret)
#' data(dhfr)
#'
#' dhfr_reduced <- preprocess_data(dhfr, target = "Y", reduce_cols = TRUE)
#'
#' dhfr_reduced <- preprocess_data(dhfr,
#'                                 target      = "Y",
#'                                 reduce_cols = TRUE,
#'                                 impute      = "mean",
#'                                 freq_cut    = 2,
#'                                 unique_cut  = 20,
#'                                 prepro_methods = c("center","scale","BoxCox"))
#' }
#' }
preprocess_data <- function(x,
                            target      = "Truth",
                            reduce_cols = FALSE,
                            factor_y    = TRUE,
                            impute      = "zero",
                            corr_cutoff = .90,
                            freq_cut    = 95/5,
                            unique_cut  = 10,
                            k           = 10,
                            prepro_methods  = NULL) {
  if (sum(!is.data.frame(x), !is_tibble(x)) == 0) {
    message("x needs to be a data.frame or tibble")
  }

  if (sum(class(x) == "grouped_df") > 0) {
    x <- x %>% ungroup()
    message("Data has been ungrouped")
  }

  if (sum(names(x) %>% str_detect("ID")) > 0) {
    ids <- x %>% select(ID)
    x   <- x %>% select(-ID)
    has_id <- TRUE
    message("ID column has been removed")

  } else {
    has_id <- FALSE
  }

  if ((x %>% map(~ sum(is.infinite(.x)) > 0) %>% unlist() %>% sum()) > 0) {

    num_col_inf <- x %>%
      map(~ sum(is.infinite(.x)) > 0) %>%
      unlist() %>%
      sum()

    x <- x %>% na_if(Inf)

    num_inf <- x  %>%
      dmap(is.na) %>%
      dmap(sum)   %>%
      gather(features, num_inf) %>%
      {.$num_inf} %>% sum()

    msg <- paste("  There were", num_col_inf, "columns and", num_inf,
                 "data points containing Inf values converted to NA")

    message(msg)
  }
  target_column <- x %>% select(target)

  if (!impute %in% c("knn","Knn","KNN","mean","Mean",
                     "MEAN","zero","Zero","ZERO","PCA","pca")) {
    stop("Argument 'impute' must be either 'knn' or 'mean'", call. = FALSE)
  } else if (impute == "knn") {
    message("Imputing NAs by knn")
    x <- x %>%
      select(-target) %>%
      select_if(naniar::any_na) %>%
      as.data.frame() %>%
      knnImputation(k = k) %>%
      as_tibble() %>%
      bind_cols(x %>% select_if(all_complete))

  } else if (impute == "mean") {
    message("Imputing NAs by column means")
    x <- x %>%
      select(-target) %>%
      impute_mean_if(any_na)

  } else if (impute == "zero") {
    message("Replacing NAs with 0")
    x <- x %>%
      select(-target) %>%
      replace(is.na(.), 0)
  }

  x <- x %>% bind_cols(target_column)

  if (target_column %>% is.factor() == TRUE) {
    message("Target is already a factor")

  } else if (factor_y == TRUE) {
    VALUE <- NULL
    wrapr::let(
      c(VALUE = target),
      x <- x %>% mutate(VALUE = as.factor(VALUE))
    )
    message("Converted ", target, " into a factor")

  } else if (factor_y == FALSE) {
    VALUE <- NULL
    class <- x %>% select(target) %>%
      unique(.) %>%
      unlist() %>%
      unname()

    class_1 <- class[1]
    class_2 <- class[2]

    wrapr::let(
      c(VALUE = target,
        c1 = class_1,
        c2 = class_2),
      x <- x %>% mutate(VALUE = recode(VALUE, c1 = 0, c2 = 1))
    )
    message("Converted ", target, " into binary, ",
            class_1, " = 0, ", class_2, " = 1")
  }

  if (reduce_cols == TRUE) {
    message("Finding columns with variance near zero...")

    nzv <- nearZeroVar(x,
                       saveMetrics    = T,
                       freqCut        = freq_cut,
                       uniqueCut      = unique_cut) %>%
      rownames_to_column() %>%
      select(rowname, nzv) %>%
      filter(nzv == TRUE) %>%
      filter(rowname != "Truth") %>%
      {.$rowname}
    num_nzv_columns <- length(nzv)

    x <- x %>% select(-nzv)

    message("  Done! Removed ", num_nzv_columns, " columns with variance near zero")
    message("Finding columns that have a correlation coefficient higher than ",
            corr_cutoff, "...")

    vars_cor  <- cor(x %>% select(-target), use = "complete.obs")
    cor_caret <- findCorrelation(vars_cor, cutoff = corr_cutoff)
    num_col   <- length(cor_caret)
    x         <- x %>% select(-cor_caret)

    message("  Done! Removed ", num_col, " highly correlated columns")


    message("DONE!")
  }

  if (!is.null(prepro_methods)) {
    pp_ob <- caret::preProcess(x, methods = prepro_methods)
    x <- pp_ob %>% predict(newdata = x)

  }

  if (has_id == TRUE) {
    x <- x %>%
      bind_cols(ids) %>%
        select(ID, everything())
    }

    return(x)

}
