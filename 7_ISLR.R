data = ISLR::Credit[c('Income', 'Balance', 'Student', 'Married')]
names(data)[names(data) == 'Married'] = 'default'

data$default = gsub("No",0,data$default)
data$default = gsub("Yes",1,data$default)
data$Student = gsub("No",0,data$Student)
data$Student = gsub("Yes",1,data$Student)

data$default = as.integer(data$default)
data$Student = as.integer(data$Student)

head(data)
dim(data)
summary(data)

set.seed(1)
row.number = sample(1:nrow(data), 0.6 * nrow(data))

train = data[row.number,]
test = data[-row.number,]
dim(train)
dim(test)

model = lda(default ~ Student + Balance + Income, data = train)
model
summary(model)

pred.prob_train = predict(model, data = train)$class
table(pred.prob_train, data[row.number,]$default)

pred.prob_test = predict(model, newdata = test)$class
table(pred.prob_test, data[-row.number,]$default)