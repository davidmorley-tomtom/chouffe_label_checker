

execute.pg <- function(con, sql) {
  q <- dbSendStatement(con, sql)
  dbClearResult(q)
}


fit.tuned.svm <- function(x) {
  x$human <- as.factor(x$human)
  x <- x %>% mutate(human = factor(human, labels = make.names(levels(human))))
  train(human ~ azi_diff + common + intr + min_sep + frc_diff,
        data = x, 
        verbose = TRUE,
        method = "svmLinear",
        preProc = c("center", "scale"),
        trControl = trainControl(method = "cv", 
                                 number = 5, 
                                 search = "random",
                                 classProbs =  TRUE))
}

get.metrics <- function(m, x) {
  pred.test <- predict(m, x)
  confusionMatrix(pred.test, as.factor(x$human))
}
