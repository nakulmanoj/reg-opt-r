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
