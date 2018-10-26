#' Pre-process cognostic data
#'
#' Generates a tibble with features optimized for machine learning
#'
#' This is the details section
#'
#' @param x data frame or tibble.
#' @param pred classifier column
#' @param corr.cutoff A numeric value for the pair-wise absolute correlation cutoff (defaults to .90)
#' @param freqCut the cutoff for the ratio of the most common value to the second most common value
#' @param uniqueCut the cutoff for the percentage of distinct values out of the number of total samples
#' @param impute.method Impute NA by "knn","mean","zero", or "pca"
#' (knn takes substantially longer to compute, zero replaces NA with 0)
#' @param k the number of nearest neighbours to use for imputate (defaults to 10)
#' @param binary_y TRUE = Recodes pred to 0 and 1; FALSE = Recodes pred to factor
#' @param pca TRUE = PCA is computed instead of corr
#' @param
#' @export
#' @examples
#' insert examples here
#'
#' @return This function returns a \code{tibble} of optimized features
#'
#' @author "Dallin Webb <dallinwebb@@byui.edu>"
#' @seealso \link[base]{summary}

# Step 0: Function initialization ----------------------------------------------
preprocess_data <- function(x,
                            pred        = "Truth",
                            corr_cutoff = .90,
                            freq_cut    = 95/5, # Caret's default setting
                            unique_cut  = 10,   # Caret's default setting
                            impute      = "zero",
                            k           = 10,
                            binary_y    = FALSE,
                            pca         = FALSE) {

# Step 1:  Required packages (don't include in final function) -----------------
require(tidyverse, quietly = T)
require(naniar,    quietly = T)
require(DMwR,      quietly = T)
require(caret,     quietly = T)
require(purrrlyr,  quietly = T)
require(seplyr,    quietly = T)

# Step 2:  Make sure data is a data.frame or tibble ----------------------------
if (!is.data.frame(x) | !is_tibble(x)) {
  message("x needs to be a data.frame or tibble")
}

# Step 3:  Remove grouped data frame if it's grouped ----------------------------
if (sum(class(x) == "grouped_df") > 0) {
  x <- x %>% ungroup()
  message("Data has been ungrouped")
}

# Step 4:  Remove ID column if present -----------------------------------------
if (sum(names(cogs) %>% str_detect("ID")) > 0) {
  x <- x %>% select(-ID)
  message("ID column has been removed")
}

# Step 5:  Filter out any NA in pred column ------------------------------------
if (is.na(x[pred]) %>% sum() > 0) {

  # New method using library(seplyr)
  # https://stackoverflow.com/questions/45261356/why-doesnt-dplyr-filter-work-within-function-i-e-using-variable-for-column
  x <- x %>% filter_se(paste0("!is.na(", pred, ")"))

  # NAs <- x %>% select(pred) %>% where_na() %>% {.[,1]}
  # x <- x[-NAs, ]

  message(paste("NAs in column", pred, "have been removed"))

}

# Step 6:  Convert Inf values to NA, if any -------------------------------------------
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
# Step 7:  Impute with knn|mean|zero -------------------------------------------
if (!impute %in% c("knn","Knn","KNN","mean","Mean",
                   "MEAN","zero","Zero","ZERO","PCA","pca")) {

  stop("Argument 'impute' must be either 'knn' or 'mean'", call. = FALSE)

} else if (impute == "knn") {

  message("Imputing NAs by knn")
  x <- x %>%
    select_if(naniar::any_na) %>%
    as.data.frame() %>%
    knnImputation(k = k) %>%
    as_tibble() %>%
    bind_cols(x %>% select_if(all_complete))


} else if (impute == "mean") {
  message("Imputing NAs by column means")
  x <- x %>% impute_mean_if(any_na)


} else if (impute == "zero") {
  message("Replacing NAs with 0")
  x <- x %>% replace(is.na(.), 0)
}
# Step 8:  Mutate y into a factor or 0|1 ------------------------------------------

if (binary_y == FALSE) {
  message("Converted ", pred, " into a factor")

  VALUE <- NULL
  wrapr::let( # This function allows for standard evaluation
    c(VALUE = pred),
    x <- x %>% mutate(VALUE = factor(VALUE))
  )

}

if (binary_y == TRUE) {


  VALUE <- NULL
  class <- x %>% select(pred) %>% unique(.) %>% unlist() %>% unname()
  class_1 <- class[1]
  class_2 <- class[2]

  wrapr::let(
    c(VALUE = pred,
      c1 = class_1,
      c2 = class_2),
    x <- x %>% mutate(VALUE = recode(VALUE, c1 = 0, c2 = 1))
  )
  message("Converted ", pred, " into binary, ",
          class_1, " = 0, ", class_2, " = 1")
}


# Step 9:  Remove columns with variance near zero ------------------------------
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

# Step 10.1: If pca == FALSE, Remove highly correlated columns -------------------
if (pca == FALSE) {
  message("Finding columns that have a correlation coefficient higher than ",
          corr_cutoff, "...")

  vars_cor  <- cor(x %>% select(-pred), use = "complete.obs")
  cor_caret <- findCorrelation(vars_cor, cutoff = corr_cutoff)
  num_col   <- length(cor_caret)
  x         <- x %>% select(-cor_caret)

  message("  Done! Removed ", num_col, " highly correlated columns")

  # Step 10.2: If pca == FALSE, Center and scale data using caret ------------------
  message("Transforming variables to a range of 0 - 1")

  x <- x %>% select(-pred) %>%
    preProcess(method      = "range",
               rangeBounds = c(0, 1)) %>%
    predict(newdata = x)

# Step 10.2: If pca = TRUE, perform pca analysis----------------------------------
} else if (pca == TRUE) {
  message("Performing pca reduction")

  x <- x %>% select(-pred) %>%
    preProcess(method = "pca") %>%
    predict(newdata = x) %>%
    as_tibble()

}
# Step 12: Return final data frame ---------------------------------------------
message("DONE!")
return(x)

}
