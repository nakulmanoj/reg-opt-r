optimizers <- c("BFGS", "L-BFGS-B", "CG", "Nelder-Mead")

regularizers <- list(
  none = list(lambda = 0, alpha = 0),
  l2   = list(lambda = 0.01, alpha = 0),
  l1   = list(lambda = 0.01, alpha = 1),
  enet = list(lambda = 0.01, alpha = 0.5)
)

seeds <- c(1, 2, 3, 4, 5)

accuracy <- function(y_true, y_pred) {
  mean(y_true == y_pred)
}

f1_score <- function(y_true, y_pred) {
  y_true <- as.numeric(as.character(y_true))
  
  tp <- sum(y_true == 1 & y_pred == 1)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)
  
  precision <- tp / (tp + fp + 1e-8)
  recall <- tp / (tp + fn + 1e-8)
  
  2 * precision * recall / (precision + recall + 1e-8)
}

run_one <- function(train, test, optimizer, lambda, alpha, seed) {
  set.seed(seed)
  
  X_train <- as.matrix(train[, -1])
  y_train <- train$diagnosis
  
  X_test <- as.matrix(test[, -1])
  y_test <- test$diagnosis
  
  start <- Sys.time()
  
  res <- train_logreg(
    X_train, y_train,
    lambda = lambda,
    alpha = alpha,
    method = optimizer
  )
  
  f1 <- f1_score(y_test, pred)
  
  end <- Sys.time()
  
  pred <- predict_logreg(res$par, X_test)
  
  acc <- mean(pred == as.numeric(as.character(y_test)))
  
  return(data.frame(
    optimizer = optimizer,
    lambda = lambda,
    alpha = alpha,
    accuracy = acc,
    time = as.numeric(end - start, units = "secs"),
    iters = sum(res$counts),
    f1 = f1
  ))
}

results <- data.frame()

for (opt in optimizers) {
  for (reg in names(regularizers)) {
    lambda <- regularizers[[reg]]$lambda
    alpha  <- regularizers[[reg]]$alpha
    
    for (s in seeds) {
      out <- run_one(train, test, opt, lambda, alpha, s)
      out$regularizer <- reg
      out$seed <- s
      results <- rbind(results, out)
    }
  }
}

write.csv(results, "results/wdbc_results.csv", row.names = FALSE)

head(results)
aggregate(accuracy ~ optimizer + regularizer, data = results, mean)
