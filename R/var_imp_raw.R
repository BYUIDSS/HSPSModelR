#' Extract feature importance within models
#'
#' Generates tibble of features ranked by importance by each model
#'
#' This is the details section
#'
#' @param models List of models of class \code{train}
#' @importFrom dplyr select filter mutate arrange top_n row_number rename count group_by percent_rank
#' @importFrom purrr map possibly
#' @importFrom tidyr gather unnest
#' @importFrom rlist list.clean
#' @importFrom tibble tibble rownames_to_column column_to_rownames
#' @importFrom magrittr %>%
#' @importFrom caret varImp
#' @export
#'
#' @return This function returns a \code{tibble} of ranked features by model including columns:
#' \itemize{
#'  \item model
#'  \item feature
#'  \item Overall scaled score produced by \code{caret::varImp()}
#'  \item rank ordered rank within each model
#' }
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[caret]{varImp}
var_imp_raw <- function(models) {

  if (class(models) != "list" | class(models[[1]]) != "train") {
    stop("models argument must be a list of models of class 'train'")
  }

  initial_length <- length(models)


  varImp_possibly <- possibly(varImp, otherwise = "Non-optimised model")
  suppressWarnings(
    imp_vars <- models %>%
      purrr::map(varImp_possibly) %>%
      rlist::list.clean(fun = is.character)
  )

  post_length <- length(imp_vars)

  message(post_length, " out of ", initial_length, " models selected by",
          " caret::varImp()")

  suppressMessages(
    for (i in seq_along(imp_vars)) {

      if (ncol(imp_vars[[i]]$importance) > 1) {
        cleaned <- imp_vars[[i]]$importance %>%
          rownames_to_column() %>%
          gather(d, Overall, -rowname) %>%
          filter(d == "Dropped") %>%
          select(-d) %>%
          top_n(35) %>%
          column_to_rownames()
        imp_vars[[i]]$importance <- cleaned
      }

    }
  )

  model_names <- map(imp_vars, "model") %>% unlist() %>% unname()

  imp <- imp_vars %>%
    map(1) %>%
    map(rownames_to_column) %>%
    map(~ arrange(., desc(Overall))) %>%
    map(~ mutate(., Overall = round(Overall, 2),
                 rank    = row_number()) )

  result <- tibble(model    = model_names,
                   imp_list = imp) %>%
    unnest(imp_list) %>%
    rename(features = rowname)

  return(result)
}
