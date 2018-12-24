#' Get packages needed for ensemble learning
#'
#' Install and / or load all libraries used in the HSPSModelR::run_models function
#'
#' @importFrom pacman p_load
#'
#' @export
#'
#' @author "Chad Schaeffer <sch12059@@byui.edu>"
#'
#' @examples
#'
#' \dontrun{
#' install_train_packages()
#' }

install_train_packages <- function() {
  p_load("mda", "ipred", "wsrf", "ada", "kernlab", "arm", "rpart", "MASS", "partDSA",
         "bnclassify", "e1071", "ranger", "spls", "binda", "mboost", "RSNNS",
         "klaR", "MASS", "plsRglm", "sparseLDA", "evtree", "randomForest", "naivebayes",
         "C50", "party", "hda", "ordinalNet", "rotationForest", "bst", "nodeHarvest",
         "inTrees", "earth", "msaenet", "glmnet", "Matrix", "class", "HDclassif", "monmlp",
         "stepPlr", "rpartScore", "LiblineaR", "kerndwd", "protoclass", "proxy", "rocc",
         "sdwd", "gam", "pls", "sda", "xgboost", "rotationForest")
}
