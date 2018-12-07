#' Run many models on train data
#'
#' Generates list of models of class train
#'
#' This is the details section
#'
#' @param train_x data.frame or tibble of predictor variables
#' @param train_y vector of target variable
#' @param trim_models Logical. If TRUE, excess will be omited from models
#' @param seed Integer. Seed for caretEnsemble::caretList()
#' @param num_folds Integer. Number of folds.
#' @importFrom caretEnsemble caretList
#' @importFrom caret trainControl createFolds
#' @importFrom purrr map
#' @export
#'
#' @return This function returns a \code{list} of trained models:
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[caretEnsemble]{caretList}
run_models <- function(train_x, train_y, seed = 1, num_folds = 2, trim_models = TRUE) {
  folds_index <- caret::createFolds(train_y, k = num_folds)
  myControl <- caret::trainControl(
    method = "cv",
    number = 2,
    trim          = trim_models,
    classProbs    = TRUE,
    verboseIter   = TRUE,
    allowParallel = TRUE,
    savePredictions = "final",
    index = folds_index,
    summaryFunction = twoClassSummary)

  methods <- c("parRF",
               "xgbLinear",
               "svmLinearWeights2",
               "extraTrees",
               "naive_bayes")

  set.seed(seed)
  model_list <- caretEnsemble::caretList(
    x                = train_x,
    y                = train_y,
    trControl        = myControl,
    methodList       = methods,
    continue_on_fail = T
  )

  if (trim_models == TRUE) {
    result <- model_list %>% purrr::map(caret:::trim.train)
    return(result)

  } else return(model_list)

}
