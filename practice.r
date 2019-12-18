library(MASS)
library(ggplot2)

data = read.csv("7.csv")
data$default = gsub("No",0,data$default)
data$default = gsub("Yes",1,data$default)
data$student = gsub("No",0,data$student)
data$student = gsub("Yes",1,data$student)

data$default = as.integer(data$default)
data$student = as.integer(data$student)

head(data)
dim(data)
summary(data)

set.seed(1)
row.number = sample(1:nrow(data), 0.6 * nrow(data))

train = data[row.number,]
test = data[-row.number,]
dim(train)
dim(test)

model = lda(default ~ student + balance + income, data = train)
model
summary(model)

pred1 = predict(model, data = train)
table(pred1$class, data[row.number,]$default)

pred2 = predict(model, newdata = test)
table(pred2$class, data[-row.number,]$default)

ldahist(pred1$x[,1], g= pred1$class)

par(mfrow=c(1,1))
plot(pred2$x[,1], pred2$class, col=test$default)