# Logistic Regression
data = read.csv("hw1.csv")
w = c(0,0,0)
weight = matrix(w)

training =data[-10,]
testing = data[10,]

train_set = training[,-4]
train_label = training['y']
test_set = testing[,-4]
test_label = testing['y']

train_set = data.matrix(train_set)
test_set = data.matrix(test_set)
train_label = data.matrix(train_label)
test_label =data.matrix(test_label)

y_out = c()
y_out1 = c()
Y = 0
v = -1
y_in = c()
count_1 = 0
misclassification = 1

while( misclassification > 0 )
{
  count_1 = count_1 + 1
  misclassification = 0
  for( i in 1:dim(train_set)[1] ) {
    while(1)
    {
      y_in[i] = train_set[i,] %*% weight
      Y = y_in[i] * v
      y_out1[i] = 1/(1 + exp(Y))
      
      if( y_out1[i] >= 0.5 )
        y_out[i] = 1
      if( y_out1[i] < 0.5)
        y_out[i] = 0
      
      if( y_out[i] != train_label[i,1] )
        weight = weight + ((0.3 * train_set[i,]) * (train_label[i,1] -(y_out1[i])))
      if( y_out[i] == train_label[i,1] )
      {
        break
      }
    }
  }
  check = train_set%*% weight
  
  for( i in 1:dim(train_set)[1] )
  {
    
    if( check[i] >= 0 )
      check[i] = 1
    
    if(check[i] < 0 )
      check[i] = 0;
    
    if( check[i] != train_label[i,1])
      misclassification  =misclassification  +1
  }
  
}

print(weight)
train_label
test_set %*% weight
print("number of epochs")
print(count_1)
