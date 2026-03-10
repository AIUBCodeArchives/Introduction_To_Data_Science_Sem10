#8 feb


a<-10

b<-14

d<-c(12,13,14,15,16)

e<-c(1,2,3,4,5)

s<-d+e

d[3]

plot(e,d)


#22 feb

mat <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)

print (mat)

mat1 <- mat[2:3, 2:3]

print(mat1)

mat2 <-mat[c(1,3), c(1,3)]

print(mat2)

mat[,3]

mat[1,]


mat5 <- matrix(1:4, nrow=2)

print(mat5)

mat6 <- matrix(5:8, nrow=2)

print(mat6)

sum_mat<- mat5+mat6

print(sum_mat)

sum_mat<- mat5*mat6

print(sum_mat)

mat5%*%mat6

solve(mat5+mat6)



#1 march

arr <- array(1:18, dim = c(3,3,2))

print(arr)

arr[2,,2]

apply(arr, MARGIN = 1, FUN= sum)

apply(arr, MARGIN = 2, FUN= sum)

apply(arr, MARGIN = 1, FUN= mean)

apply(arr, MARGIN = 2, FUN= mean)


df<- data.frame(
  
  ID = c(101, 102, 103, 104),
  
  Name= c("Alice","Bob", "Charlie","David"),
  
  Age = c(22,23,24,25),
  
  Score = c(80,89,87,88),
  
  Passed = c(TRUE, FALSE, TRUE, TRUE)
  
)

df

df$Age

mean(df$Age)

var(df$Score)

df[3,]

df[3,4]

df[3,"Score"]

df[df$Age >mean(df$Age),]



ages<- c(11,45,23,69,80)

ages[ages > 30] <- "30+"

ages

mean(ages)

var(ages)

ages[ages < 25]

ages[ages < 15] <- 36

ages[2] <- 39

num_vec <- c(1,2,3,4,5)

num_vec[3] <- 30

num_vec[c(2,4)] <- c(60,32)

seq_vec <- c(1, seq(5,100 , by=5))

rep_vec <- c(1, rep(2, times=2))


df
df[df$Score+5]

df$Score <- df$Score+5
df
 

df$Grade <- c("A", "B", "C", "D")
df

df_sorted <- df[order(df$Age, decreasing = TRUE), ]
df_sorted


x <- 10
if (x>7){
  print("ieabokr")
}

for (i in 100:120){
  if(i%%5==0){
    print(i) 
  }
}
  

for (i in 100:120){
  print(i)
}

for (i in 1:5){
  if (i == 3) break
  print(i)
}




