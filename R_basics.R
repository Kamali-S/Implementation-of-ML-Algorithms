# Worksheet 
#QUESTION 1

 df <- read.csv(file = "Iris.csv", header = TRUE)
 #print(df)
 
 #Structure 1a
 print(str(df))
 
 #Summary 1b
 print(summary(df))
 
 #Dimensions 1c
 print(dim(df))
 
 #Variables 1d
 print(nrow(df))
 print(ncol(df))
 print(colnames(df))
 
 #Finding NA 1e
 is.na(df['SepalLengthCm'])
 print(NA %in% df)
 
 #Finding count(missing values) 1f
 sum(is.na(df))
 
 #1g
 colSums(is.na(df))

 #1h
 names(which(sapply(df,anyNA)))

 
 # QUESTION 2
 
 temp = flights
# 1a
 first_five = head(temp, n=5)
 print(first_five)

# 1b
 for(i in temp['dep_time'])
   ceiling(log10(012400))

# 1c
 temp["hour"] = NA
 for(i in 1:dim(temp)[2])
 {
   if(i <= 2)
     temp['hour'][i,1] = temp['dep_time'][i,1]
   else
     temp['hour'][i,1] = NA
 }


# 1d
  temp['Minutes'] = NA
  for(i in 1:336776)
  {
    if(i >= 336774)
      temp['Minutes'][i,1] = temp['dep_time'][i,1]
    else
      temp['Minutes'][i,1] = NA
  }
  
#1e
#as.POSIXct("080406 10:11", format = "%y%m%d %H:%M")

# QUESTION 3
#3a
  summary(temp)

#3b
  #check out
  print(temp['arr_time'][which(is.na(temp['arr_time'])==TRUE,arr.ind = TRUE)])
  print(which(is.na(temp$arr_time) == TRUE))
  #answer
  a = temp$arr_time
  Mean = mean(which(is.na(a) == FALSE, arr.ind = TRUE))
  print(Mean)

#3c
  a = temp$arr_time
  M = median(which(is.na(a) == FALSE, arr.ind = TRUE))
  print(M)

#3d 
  a = temp$air_time
  M = median(which(is.na(a) == FALSE, arr.ind = TRUE))
  for( i in 1:dim(temp['air_time'])[1] )
  {
    if( is.na(temp['air_time'][i,1]) == TRUE )
      temp['air_time'][i,1] = M
  }

#3e
  a = temp$dep_time
  Median = median(which(is.na(a) == FALSE, arr.ind = TRUE))
  for( i in 1:dim(temp['air_time'])[1] )
  {
    if( is.na(temp['air_time'][i,1]) == TRUE )
      transform(temp['air_time'][i,1], Median)
  }
  
#3h
  w = c(round(runif(4,0,1),2))
  h = matrix(c(0), nrow = 336776, ncol = 4 )
  h = cbind(temp$air_time)
  h = cbind(h, temp$arr_time) 
  h = cbind(h, temp$dep_time)
  h = cbind(h, temp$arr_delay)
  label = matrix(temp$dep_delay, nrow = 1, ncol = 336776)
  
  for( i in 1:dim(h)[1] )
  {
    ycap = h[i,] %*% w
    
    if( ycap != label[i,1])
    {
      w = w - 0.5*(ycap-label[i,1])
    }
    
  }