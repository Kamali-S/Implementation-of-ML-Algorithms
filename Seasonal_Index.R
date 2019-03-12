# seasonal index
SI = read.csv("mm_time_series.csv")

n = dim(SI)[1] - 3

# Finding Moving Average
SI['Moving_Average'] = NA
j = 3

for( i in 1:n )
{
  SI['Moving_Average'][j,1] = ( SI['y'][i,1] + SI['y'][i+1,1] + SI['y'][i+2,1] + SI['y'][i+3,1] ) / 4
  j = j + 1
}

# Finding Central Moving Average
SI['CMA'] = NA
m = n + 2
k = 4

for( i in 3:m )
{
  SI['CMA'][i,1] = (SI['Moving_Average'][i,1] + SI['Moving_Average'][i+1,1]) /  2
}

# Finding y/CMA
SI['y/CMA'] = NA

for( i in 3:m )
{
  SI['y/CMA'][i,1] = SI['y'][i,1] / SI['CMA'][i,1]
}

# Converting SI_matrix into a matrix
SI_matrix = matrix(SI$`y/CMA`, nrow = 12, ncol = 12, byrow = TRUE)
SI_data = data.frame(SI_matrix)

# Finding colsum and final calculation
total = 0
l = c()
k = 0

# Finding no of non-zero values 
for( i in 1:dim(SI_matrix)[1] )
{
  for( j in 1:dim(SI_matrix)[2] )
  {
    if(is.na(SI_dataframe[j,i]) == TRUE)
      k = k + 1
  }
  l = append(l, k, length(l))
  k = 0
}

# Making the NA values into zero
for( i in 1:dim(SI_data)[1] )
{
  for( j in 1:dim(SI_data)[2] )
  {
    if( is.na(SI_data[i,j]) == TRUE )
      SI_data[i,j] = 0
  }
}

# Finding seasonal index for each month
temp = c()
seasonal_index = c()
ColSum = colSums(SI_data) 
for( i in 1:length(ColSum) )
{
  value = ColSum[i] / (length(ColSum) - l[i])
  temp = append(temp, value, length(temp))
}

for( i in 1:length(temp) )
{
  value = (temp[i] / sum(temp)) * length(temp)
  seasonal_index = append(seasonal_index, value, length(seasonal_index))
}

print("Seasonal index :")
print(seasonal_index)

# Prediction
x_sum = colSums(SI['x'])
y_sum = colSums(SI['y'])
n = dim(SI)[1]
x2 = colSums(SI['x']^2)
xy = colSums(SI['x'] * SI['y'])

A = matrix( data= c(x_sum,n,x2,x_sum), nrow = 2, ncol = 2, byrow = TRUE)
b = matrix( data= c(y_sum,xy), nrow = 2, ncol = 1, byrow = FALSE)
a_b = round(solve(A,b),2)

t = readline(prompt="Enter the value of t :")
trend = a_b[1]*t + a_b [2]



# Finding deseasonalised indices
deseasonalised_index = c()
k=1
for(i in 1:dim(SI)[1])
{
  if(k<=12)
  {
    value = SI['y'][i,1] / seasonal_index[k]
    deseasonalised_index = append(deseasonalised_index, value, length(deseasonalised_index))
    k = k + 1
  }
  else
    k = 1
}

print("Deseonalised index:")
print(deseasonalised_index)
