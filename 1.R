data <- read.csv("1.csv", header=TRUE, sep=",")
x <- data$experience
y <- data$publications

x_mean <- mean(x)
y_mean <- mean(y)

num = sum((x - x_mean) * (y - y_mean))
dem = sum((x - x_mean) ^ 2)

b1 = num/dem
b0 = y_mean - b1 * x_mean
b1
b0

data$pred = b0 + b1 * x

rss = sum((y - data$pred) ^ 2)
tss = sum((y - y_mean) ^ 2)
rse = sqrt(rss/(nrow(data) - 2))
se = 1 -  (rss/tss)
se

model <- lm(y~x, data)
summary(model)

data$pred2 = predict(model, data.frame(x))

plot(x, y)
lines(x, data$pred)
lines(x, data$pred2)