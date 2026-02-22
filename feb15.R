


#15 feb

num_vec <- c(1, 2, 3, 4, 5)
num_vec

num_vec[3] <- 30

num_vec[3] <- 35

num_vec[c(2, 4)] <- c(25, 45)


num_vec[num_vec > 20]

num_vec[c(6, 7)] <- c(55, 65)

num_vec <- c(num_vec, 85, 95)

len <- length(num_vec)
summ <- sum(num_vec)
meann <- mean(num_vec)
var(num_vec)
sd(num_vec)
median(num_vec)

sort(num_vec)

shorted_vec <- sort(num_vec, decreasing = TRUE)

seq_vec <- seq(0,100, by=4)

seq_vec1 <- c(1, seq(5,100, by=5))

num_vec1 <- c("1", 2, 3, 4, 5)
num_vec1


rep_vec <- c(1, rep(2, times=2),rep(3, times=3),rep(4, times=4),rep(5, times=5),rep(6, times=6),rep(7, times=7),rep(8, times=8),rep(9, times=9),rep(10, times=10))
rep_vec

num_vec2 <- c(1, 2, 3, 4, 5, "Apple")
num_vec2


char_vec <- c("Apple", "Banana", "Cherry")
char_vec


char_vec1 <- c(Apple, "Banana", "Cherry")
char_vec1


log_vec <- c(TRUE, FALSE, TRUE, FALSE)
log_vec



#arithmatic

vec1 <- c(2, 4, 6)
vec2 <- c(1, 3, 5)

sum_vec <- vec1+vec2
min_vec <- vec1-vec2

vec3 <- c(4, 6, 8)
vec4 <- c(2, 3, 4)

div_vec <- vec3/vec4
mul_vec <- vec3*vec4


mod_vec <- vec1%%vec2

power_vec <- vec1^2


