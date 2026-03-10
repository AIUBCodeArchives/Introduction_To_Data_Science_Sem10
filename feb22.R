mat <- matrix(1:9, nrow = 3, ncol = 3)
mat

mat1 <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
mat1

mat2<- t(mat)
mat2

mat3 <- matrix(round(abs(rnorm(9)*100)), nrow = 3, ncol = 3, byrow = TRUE)
mat3


mat4 <- matrix(c(seq(6, 30, by=3)), nrow = 3, ncol = 3)
mat4

mat5 <- matrix(mat4[c(5,6,8,9)], nrow=2, ncol=2)
mat5

mat4[2:3, 2:3]

mat4[c(1,3), c(1, 3)]
mat4[,3]


mat7 <- matrix(1:4, nrow=2)
mat8 <- matrix(5:8, nrow=2)
mat7
mat8

sum_mat <- mat7 + mat8
sum_mat

mul_mat <- mat7 * mat8
mul_mat

muldot_mat <- mat7 %*% mat8
muldot_mat

inverse <- solve(mat7)
inverse
