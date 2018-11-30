# optimal_models <- function(x,
#                            target,
#                            measure = Accuracy,
#                            length = 5,
#                            train_size = .08,
#                            seed = 1,
#                            folds = 3
#                            #write = FALSE,
#                            #write_wd,
#                            ) {
#
#   # prep data
#   set.seed(seed)
#   hs <- x
#   rows <- hs %>% nrow() %>% sample()
#   # rows <- BYUImachine::preprocess_data(hs, "Truth", impute = "zero") %>% nrow() %>% sample()
#   hs <- hs[rows, ]
#   split <- round(nrow(hs)* train_size)
#   train <- hs[1:split, ] %>% na.omit()
#   test <- hs[(split + 1):nrow(hs), ] %>% na.omit()
#   target_column <- test %>% select(target)
#
#
#
#   # create control
#   myFolds <- createFolds(target_column, k = folds)
#   myControl <- trainControl(
#     summaryFunction = twoClassSummary,
#     index  = myFolds,
#     method = "ROC",
#     number = 2,
#     classProbs  = TRUE,
#     verboseIter = TRUE)
#
#   # make train_model function
#   train_model <- function(i) {
#     model <- train(
#       target ~ .,
#       data = train,
#       tuneLength = 1,
#       method = i,
#       trControl = myControl,
#       preProcess = c("YeoJohnson"))
#   }
#
#   # methods list
#   methods <- list("parRF", "pcaNNet")
#
#   # train models
#   models <- map(methods, train_model)
#
#   # write models, conditional on parameters, write = TRUE or FALSE & wd = path
#   #if (write = TRUE) {
#    # setwd(path)
#     #for (i in models) {
#      # write_csv(models[[i]], methods[[i]][1])
#     #}
#   #}
#
#   # get diagnostics
#   diagnostics <- BYUImachine::make_table(models, test, target)
#
#   # get optimal models
#   optimal_models <- diagnostics %>%
#     select(measure) %>%
#     arrange(desc(measure)) %>%
#     filter(measure = c(1:length))
#
# return(optimal_models)
# }
#
# #, "pda", "pslRglm", "polr", "sdwd", "slda",
# #"sparseLDA", "svmLinearWeights", "svmRadialCost", "wsrf",
# #"evtree", "fda", "gamSpline", "knn", "lda", "msaenet", "null"
#
# optimal_models(hs, "Truth")
