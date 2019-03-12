# Naive Bayes
NB = read.csv("Naive_Bayes.csv")

# Finding P[yes] and P[no]
t = c()
U = unique.data.frame(NB['y'])

for (i in 1:length(levels(U[,1]))) 
{
  value = length(which(NB['y']==levels(U[,1])[i]))
  t = append(t, value, length(t))
}

# Finding probablities for each class
test_set = read.csv("NB_test_set.csv")

Probability = function(c,d,U,u,D)
{
  P = matrix(c(0), nrow = length(u), ncol = 3 )
  yes_no = c() 
  
  for( l in 1:length(u) )
  {
    for( i in 1:length(D) )
    {
      value = length(which(c==u[l] & d==D[i]))
      yes_no = append(yes_no, value, length(yes_no))
    }
    P[l,1] = (levels(U[,1])[l])
    P[l,2] = round( (yes_no[2] / t[2]), 2 )
    P[l,3] = round( (yes_no[1] / t[1]), 2 )
    yes_no = c()
  }
  
  # Predicting the value for testset
  yes_value = 1
  no_value = 1
  for( j in 1:dim(test_set)[2] )
  {
    for( l in 1:length(P[,1]) )
    {
      if( is.na(which(test_set[j]==P[,1][l], arr.ind = TRUE)[1]) == FALSE )
      {
        yes_value = yes_value * as.numeric(P[l][2])
        no_value = no_value * as.numeric(P[l][3])
      }
    }
  Prob_value = (t[2]*yes_value) / ((t[2]*yes_value) + (t[1]*no_value))
  }

  return(Prob_value)
}

# Finding the CPT 

for( i in 1:(dim(NB)[2]-1) )
{
  i = 1
  c = data.matrix(NB[i])
  d = data.matrix(NB['y'])
  U = unique.data.frame(NB[i])
  u = unique(c)
  D = unique(d)
  Prob = Probability(c,d,U,u,D)
}

print(Prob)
