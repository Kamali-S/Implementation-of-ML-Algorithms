# and gate
and = read.csv("and_gate.csv")

and_w = c(round(runif(2,0,1),2))

out = list()
and_y_in = list()
and_t_in = list()

#Splitting the dataset
Splitting = sample.split(and$y, SplitRatio = 0.66)
train_set = subset(and, Splitting == TRUE)
test_set = subset(and, Splitting == FALSE)
and_weight = matrix(and_w)

# Seperating features and label
train_label = train_set['y']
train_set = train_set[,-3]
test_label = test_set['y']
test_set = test_set[,-3]

#Converting data frame into matrix
train_set = data.matrix(train_set)
test_set = data.matrix(test_set)


# Algorithm
for( i in 1:dim(train_set)[1] )
{
  while(1)
  {
    and_y_in[i] = train_set[i,] %*% and_weight
    
    if( and_y_in[i] >= 0 )
      and_y_in[i] = 1
    
    if( and_y_in[i] < 0 )
      and_y_in[i] = 0
    
    if( and_y_in[i]==1 && train_label[i,1]==0 )
      and_weight = and_weight - (0.5*train_set[i,])
    
    if( and_y_in[i]==0 && train_label[i,1]==1 )
      and_weight = and_weight + (0.5*train_set[i,])
    
    if( and_y_in[i] == train_label[i,1])
      break
  }
}
print(and_weight)

#Testing
for( i in 1:dim(test_label)[1])
{
  and_t_in[i] = test_set[i,] %*% and_weight
  
  if( and_t_in[i] >= 0 )
    and_t_in[i] = 1
  
  if( and_t_in[i] < 0 )
    and_t_in[i] = 0
  
  if( and_t_in[i] != test_label[i,1]){
    print('all not correct')
    break
  }
}