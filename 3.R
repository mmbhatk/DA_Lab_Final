data <- read.csv('3.csv')
train = data[1:nrow(data)-1,]
test = data[nrow(data),]

distances = c()
for(i in 1:nrow(train))
  distances = append(distances, sqrt((train[i,1]-test[1, 1])**2+(train[i,2]-test[1,2])**2))
train$dist = distances
train <- train[order(train$dist),]

print(train)

count = 0
for(i in 1:3) if(train[i, 3]=='Y') count = count + 1 else count = count - 1

if(count>=0) print("Yes") else print("No")