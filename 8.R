library(MASS)
library(ggplot2)

data = read.csv("8.csv")

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

model = qda(default ~ student +balance + income, data = train)
model
summary(model)

pred.prob_train = predict(model, data = train)$class
table(pred.prob_train, data[row.number,]$default)

pred.prob_test = predict(model, newdata = test)$class
table(pred.prob_test, data[-row.number,]$default)