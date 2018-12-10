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

  methods <- c("pda"                , "pslRglm"             ,"polr"                ,"sdwd"               ,
               "slda"               , "sparseLDA"           ,"svmLinearWeights"    ,"svmRadialCost"      ,
               "wsrf"               , "evtree"              ,"fda"                 ,"gamSpline"          ,
               "knn"                , "lda"                 ,"msaenet"             ,"null"               ,
               "glm"                , "rf"                  ,"glmnet"              ,"lda"                ,
               "ada"                , "naive_bayes"         ,"knn"                 ,"lvq"                ,
               "svmLinear"          , "treebag"             ,"bagFDA"              ,"bagEarth"           ,
               "bayesglm"           , "glmboost"            ,"C5.0"                ,"rpart1SE"           ,
               "rpart2"             , "cforest"             ,"ctree2"              ,"gcvEarth"           ,
               "glmStepAIC"         , "hda"                 ,"hdda"                ,"lda2"               ,
               "mda"                , "mlpWeightDecayML"    ,"monmlp"              ,"nb"                 ,
               "nbSearch"           , "ordinalNet"          ,"plr"                 ,"pls"                ,
               "ranger"             , "rotationForest"      ,"rpartScore"          ,"sda"                ,
               "spls"               , "svmBoundrangeString" ,"svmLinear3"          ,"xgbDART"            ,
               "binda"              , "bstSm"               ,"dwdLinear"           ,"earth"              ,
               "glmboost"           , "nodeHarvest"         ,"partDSA"             ,"protoclass"         ,
               "mlpWeightDecay"     , "rfRules"             ,"rocc"                ,"rotationForestCp"   ,
               "stepQDA"            , "svmLinear2"          ,"svmPoly"             ,"svmRadialWeig")


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
