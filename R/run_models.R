#' Run many models on train data
#'
#' Generates list of models of class train
#'
#' @param train_x data.frame or tibble of predictor variables
#' @param train_y vector of target variable
#' @param trim_models Logical. If TRUE, excess will be omited from models
#' @param seed Integer. Seed for caretEnsemble::caretList()
#' @param num_folds Integer. Number of folds.
#' @param light `logical.` FALSE data will train on only 5 algorithms. if TRUE
#' (default), data will train on all 68 models
#' @param preproc_methods prepro_methods string or vector of strings of
#'   preprocessing methods. See also \link[caret]{preProcess}
#' @importFrom caretEnsemble caretList
#' @importFrom caret trainControl createFolds
#' @importFrom purrr map
#' @export
#'
#' @return This function returns a \code{list} of trained models:
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[caretEnsemble]{caretList}
#'
#' @examples
#'
#' \dontrun{
#'
#' models_list <- run_models(train_x     = train_x,
#'                           train_y     = train_y,
#'                           seed        = 1,
#'                           num_folds   = 2,
#'                           trim_models = TRUE,
#'                           light       = TRUE)
#'
#'
#' }
run_models <- function(train_x, train_y,
                       seed            = 1,
                       num_folds       = 2,
                       trim_models     = TRUE,
                       light           = FALSE,
                       preproc_methods = NULL) {

  folds_index <- caret::createFolds(train_y, k = num_folds)

  myControl <- caret::trainControl(
    method          = "cv",
    number          = 2,
    trim            = trim_models,
    classProbs      = TRUE,
    verboseIter     = TRUE,
    allowParallel   = TRUE,
    savePredictions = "final",
    index           = folds_index,
    summaryFunction = twoClassSummary)

  if (light == TRUE) {
    methods <- c("glmboost", "pls", "rf", "earth", "rotationForestCp")
  }

  if (light == FALSE) {
    methods <- c(
        "pda",
        "slda",
        "wsrf",
        "knn",
        "glm",
        "ada",
        "svmLinear",
        "bayesglm",
        "rpart2",
        "glmStepAIC",
        "mda",
        "nbSearch",
        "ranger",
        "spls",
        "binda",
        "mlpWeightDecay",
        "stepQDA",
        "plsRglm"  ,
        "sparseLDA",
        "evtree",
        "lda",
        "rf",
        "naive_bayes",
        "treebag",
        "glmboost",
        "cforest",
        "hda",
        "mlpWeightDecayML",
        "ordinalNet",
        "rotationForest",
        "svmBoundrangeString",
        "bstSm",
        "nodeHarvest",
        "rfRules",
        "svmLinear2",
        "polr",
        "svmLinearWeights",
        "fda",
        "msaenet",
        "glmnet",
        "bagFDA",
        "C5.0",
        "ctree2",
        "hdda",
        "monmlp",
        "plr",
        "rpartScore",
        "svmLinear3",
        "dwdLinear",
        "partDSA",
        "rocc",
        "svmPoly",
        "sdwd",
        "svmRadialCost",
        "gamSpline",
        "null",
        "lvq",
        "bagEarth",
        "rpart1SE",
        "gcvEarth",
        "lda2",
        "nb",
        "pls",
        "sda",
        "xgbDART",
        "earth",
        "protoclass",
        "rotationForestCp",
        "svmRadialWeights"
        )
  }

  set.seed(seed)

  model_list <- caretEnsemble::caretList(
    x                = train_x,
    y                = train_y,
    trControl        = myControl,
    methodList       = methods,
    continue_on_fail = T,
    preProcess = preproc_methods
  )

  if (trim_models == TRUE) {
    result <- model_list %>% purrr::map(caret:::trim.train)
    return(result)

  } else return(model_list)

}
