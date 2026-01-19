# Sigmoid function
sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# Regularized logistic loss
logistic_loss_reg <- function(w, X, y, lambda = 0, alpha = 0) {
  z <- X %*% w
  p <- sigmoid(z)
  
  y_num <- as.numeric(as.character(y))
  
  # Negative log-likelihood
  nll <- -mean(y_num * log(p + 1e-8) + (1 - y_num) * log(1 - p + 1e-8))
  
  # Regularization terms (exclude bias w[1])
  w_no_bias <- w[-1]
  
  l1 <- sum(abs(w_no_bias))
  l2 <- sum(w_no_bias^2)
  
  penalty <- lambda * (alpha * l1 + (1 - alpha) * l2)
  
  return(nll + penalty)
}

train_logreg <- function(X, y, lambda = 0, alpha = 0, method = "BFGS") {
  # Add bias
  Xb <- cbind(1, X)
  
  # Init weights
  w_init <- rep(0, ncol(Xb))
  
  # Optimize
  res <- optim(
    par = w_init,
    fn = logistic_loss_reg,
    X = Xb,
    y = y,
    lambda = lambda,
    alpha = alpha,
    method = method,
    control = list(maxit = 1000)
  )
  
  return(res)
}

predict_logreg <- function(w, X) {
  Xb <- cbind(1, X)
  p <- sigmoid(Xb %*% w)
  return(ifelse(p > 0.5, 1, 0))
}

# Prepare data
X_train <- as.matrix(train[, -1])
y_train <- train$diagnosis

X_test <- as.matrix(test[, -1])
y_test <- test$diagnosis

# Train model (no regularization, BFGS)
res <- train_logreg(X_train, y_train, lambda = 0, alpha = 0, method = "BFGS")

# Predict
pred <- predict_logreg(res$par, X_test)

# Accuracy
mean(pred == as.numeric(as.character(y_test)))

