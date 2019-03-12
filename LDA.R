LDA = read.csv("Iris.csv")
LDA = LDA[,-c(1)]

# Converting the labels into values
LDA['Species'] = data.matrix(LDA['Species'])

u = unique(LDA['Species'])
#Classes = data.frame()
Mean = c()
SM = matrix(c(0), nrow = 4, ncol = 4)

# Seperating the classes 
for( l in 1:length(u[,1]) )
{
  l = 1
  Class = data.frame()
  Class = LDA[LDA[,'Species'] == u[,1][l],]
  #Classes = rbind(Classes, Class)
  
  # Finding mean for each features for each class
  for( i in 1:(dim(Class)[2]-1) )
  {
    val = sum(Class[i]) / nrow(Class)
    Mean = append(Mean, val, length(Mean))
  }
  
  # Finding within-class matrix
    
  Class_features = Class[,-c(5)]
  class_sc = matrix(c(0), nrow = 4, ncol = 4)
  for( i in 1:dim(Class_features)[1] )
  {
    i = 1
    val = Class_features[i,]
    value = matrix(val-Mean)
    class_sc = class_sc + (value %*% t(value))
  }
  
  SM = SM + class_sc

}