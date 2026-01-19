data = read.csv("dataset/bcwd/wdbc.data",header=FALSE)

colnames(data) <- c("id", "diagnosis", paste0("f", 1:30))

data$id <- NULL

data$diagnosis <- ifelse(data$diagnosis == "M", 1, 0)
data$diagnosis <- as.factor(data$diagnosis)

X <- data[, -1]          
y <- data$diagnosis      

X_scaled <- scale(X)

data_scaled <- data.frame(diagnosis = y, X_scaled)
summary(data_scaled)

set.seed(42)
n <- nrow(data_scaled)
idx <- sample(1:n, size = 0.8 * n)

train <- data_scaled[idx, ]
test  <- data_scaled[-idx, ]

table(train$diagnosis)
table(test$diagnosis)

write.csv(data_scaled, "dataset/bcwd/processed_wdbc.csv", row.names = FALSE)