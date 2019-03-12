#Splione interpolartion
path = read.csv("spline_interpolation.csv")
plot(path, type = "l")

n = dim(path)[1] - 1
values = c()

for( i in 1:n )
{
  A = matrix(data = c(path['x'][i,1],1,path['x'][i+1,1],1), nrow = 2, ncol = 2, byrow = TRUE)
  B = matrix(data = c(path['y'][i,1],path['y'][i+1,1]), nrow = 2, ncol = 1, byrow = FALSE)
  sol = solve(A,B)
  values = append(values, sol[1,1], length(values))
  values = append(values, sol[2,1], length(values))
}

print(values)

# Scatter plotpath <- "scatter_plot1.csv"
df <- read.csv(path, header = TRUE, sep=',')

#Findig column sum
CS = colSums(df['x'])
RS = colSums(df['y'])
n = nrow(df)
x_square = colSums(df['x'] * df['x'])
xy_sum = colSums(df['x'] * df['y'])

#Solving equations and finding a and b
A = matrix(data = c(CS,n,x_square,CS), nrow = 2, ncol = 2, byrow = TRUE)
b = matrix(data = c(RS,xy_sum), nrow = 2, ncol = 1, byrow = FALSE)
print(round(solve(A,b),2))
values = round(solve(A,b),2)

#Finding Y^
df["Y^"] = NA
df$`Y^` = values[1]*df$x + values[2]

#Finding y-y^
print(df['y'] - df['Y^'])

#Plotting 
plot(df$x,df$y)
lines(df$x,df$`Y^`, col = "blue")

#Finding x-square
# for( i in df['x'])
#   x_square = i*i
# not working check out
# x_square_sum = colSums(x_square)
# print(x_square_sum)

#Finding xy
# for (i in df)
#   xy = df['x'] * df['y']
# xy_sum = colSums(xy)
# print(xy_sum)
