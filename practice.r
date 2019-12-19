data <- read.csv("2.csv", header=TRUE, sep=",")

x1 <- data$experience
x2 <- data$training
y <- data$publications

x0 = rep(1, length(x1))
X = as.matrix(cbind(x0, x1, x2))
B = solve(t(X) %*% X) %*% t(X) %*% Y

b0 = B[1]
b1 = B[2]
b2 = B[3]
data$pred = b0 + b1 * x1 + b2 * x2


model <- lm(y ~ x1 + x2, data = data)
summary(model)
data$pred2 <- predict(model, data)


plot(data$pred, y, xlab = "Predicted", ylab = "Actual")
abline(a = 0, b = 1)