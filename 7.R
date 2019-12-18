library(MASS)
library(ggplot2)

data = read.csv("8.csv")
dim(data)
summary(data)

set.seed(1)
row.number = sample(1:nrow(data), 0.6*nrow(data))

train = data[row.number,]
test = data[-row.number,]
dim(train)
dim(test)

attach(data)
attach(train)
attach(test)

model = qda(default ~ student + balance + income, data = train)
model
summary(model)

pred1 = predict(model, data = train)
table(pred1$class, default)

pred2 = predict(model, newdata = test)
table(pred2$class, default)

model
par(mfrow = c(1,1))
plot(model$x[,1], model$class, col=test$default)
