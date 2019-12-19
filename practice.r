library(MASS)
library(ggplot2)

data = read.csv('7.csv')

data$default = gsub("No", 0, data$default)
data$default = gsub("Yes", 1, data$default)
data$student = gsub("No", 0, data$student)
data$student = gsub("Yes", 1, data$student)

data$default = as.integer(data$default)
data$student = as.integer(data$student)

set.seed(1)
row.number = sample(1:nrow(data), 0.6 * nrow(data))
train = data[row.number,]
test = data[-row.number,]
dim(train)
dim(test)

model = lda(default ~ student + balance + income, data = train)
summary(model)

prob_train = predict(model, train)
