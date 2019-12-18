data <- read.csv("5.csv", header=TRUE, sep=",")
x1 <- data$TV
x2 <- data$radio
y <- data$sales

x1_mean <- mean(x1)
x2_mean <- mean(x2)
y_mean <- mean(y)

num1 = sum((x1 - x1_mean) * (y - y_mean))
dem1 = sum((x1 - x1_mean) ^ 2)
b1 = num1/dem1

num2 = sum((x2 - x2_mean) * (y - y_mean))
dem2 = sum((x2 - x2_mean) ^ 2)
b2 = num2/dem2

b0 = y_mean - b1 * x1_mean - b2 * x2_mean

data$pred = b0 + b1*x1 + b2*x2

rss = sum((y - data$pred) ^ 2)
tss = sum((y - y_mean) ^ 2)
rse = sqrt(rss/(nrow(data) - 2))
se = 1 -  (rss/tss)

model <- lm(y ~ x1 + x2, data = data)
summary(model)
data$pred2 <- predict(model, data)

plot(data$pred, y)
plot(data$pred, y, xlab="Predicted", ylab="Actual")
abline(a = 0, b = 1)