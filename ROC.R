# ROC
ROC = read.csv("ROC_curve.csv")

theta = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)
TPR = c()
FPR = c()

for( i in 1:length(theta) )
{
  count = 0
  P_count = 0
  N_count = 0
  for( j in 1:dim(ROC)[1] )
  {
    if( ROC['x'][j,1] >= theta[i] )
      count = count + 1
    else
    {
      if( ROC['y'][j,1] == 'P')
        P_count = P_count + 1
      else
        N_count = N_count + 1
    }
  }
  p_value = (10 - P_count) / 10
  n_value = (10 - N_count) / 10
  TPR = append(TPR, p_value, length(TPR))
  FPR = append(FPR, n_value, length(FPR))
}

min = FPR[1]  

for( i in 1:length(FPR) )
{
  if( min < FPR[i] )
  {
    min = FPR[i]
    FPR[i] = min
  }
}
