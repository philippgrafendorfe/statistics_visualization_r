n <- 100
p <- 1/2

X <- sample(c(0,1), n, c(p, 1-p), replace = T)

lower <- (qbinom(0.025, n, 0.5)-1)
upper <- qbinom(0.975, n, 0.5)

sum(X) >= lower && sum(X) <= upper
R <- 10000
test <- rep(1, R)

for(i in 1:R){
  
  x <- sample(c(0,1), n, c(p, 1-p), replace = T)
  if(sum(X) >= lower && sum(X) <= upper){test[i]=0}
}
mean(test)

# =====================================================
# erxercise 1

n <- 1e3
p <- 3/4
X <- rbinom(n, 1, p)
R <- 1e4

mu <- rep(0, R)

for(i in 1:R){
  
  xi <- sample(X, R, replace = T)
  mu[i] <- mean(xi)
  
}

boxplot(mu)
abline(h = mean(X), col='red')

mean(mu)
mean(X)

mu.sorted <- sort(mu)
abline(h = mu.sorted[250], col='blue')
abline(h = mu.sorted[9750], col='blue')

# =====================================================
# exercise 2

A <- read.table("http://www.trutschnig.net/Datensatz.txt", header = T)
library("dplyr")


B <- A %>% 
  select(weekday, sum_out) %>% 
  na.omit() %>% 
  filter(weekday == 'Mon')

R <- 1e4

mu <- rep(0, R)

tic()
for(i in 1:R){
  
  xi <- sample(B$sum_out, nrow(B), replace = T)
  mu[i] <- mean(xi)
  
}
toc()

boxplot(mu)
abline(h = mean(B$sum_out), col='red')

mean(mu)
mean(B$sum_out)

mu.sorted <- sort(mu)

mu.sorted <- sort(mu)
abline(h = quantile(mu, 0.025), col='blue')
abline(h = quantile(mu, 0.975), col='blue')
  
hist(mu)
abline(v = quantile(mu, 0.025), col='blue')
abline(v = quantile(mu, 0.975), col='blue')

# ===============================================================
library(boot)

samplemean <- function(dat, idx) {
  
  return(mean(dat[idx]))
  
}

b <- boot(X, statistic = samplemean, R=1e4)

print(b)
plot(b)
hist(b$t)

zoom <- boot.array(b, indices = T)
hist(zoom[1,], breaks = 100)

hist(b$t)

# =============================================================
# permutation

n <- 1e4
p <- 1/2

C1 <- rbinom(n, 1, p)
C2 <- rbinom(n, 1, p)

C <- c(C1, C2)

avg1 <- rep(0, n)
avg2 <- rep(0, n)

for(i in 1:n){
  
  idx1 <- sample(1:2e4, size=round(n/2))
  idx2 <- setdiff(1:2e4, idx1)
  
  avg1[i] <- mean(C[idx1])
  avg2[i] <- mean(C[idx2])
  
}

avg_diff <- avg1 - avg2

hist(avg_diff, xlim=range(-0.25, 0.25))

avg_diff_sorted <- sort(avg_diff)

original_diff <- mean(C1) - mean(C2)

avg_diff_sorted[250] < original_diff && original_diff < avg_diff_sorted


# ==================================================================
# ex from board

A.mon <- A %>% 
  select(weekday, sum_out) %>% 
  na.omit() %>% 
  filter(weekday == 'Mon')

A.thu <- A %>% 
  select(weekday, sum_out) %>% 
  na.omit() %>% 
  filter(weekday == 'Thu')

mu_mon <- mean(A.mon$sum_out)
mu_thu <- mean(A.thu$sum_out)

A.comb <- A %>% 
  select(weekday, sum_out) %>% 
  na.omit() %>% 
  filter(weekday %in% c('Thu', 'Mon'))

avg1 <- rep(0, n)
avg2 <- rep(0, n)

for(i in 1:n){
  
  idx1 <- sample(1:2*nrow(A.comb), size=round(n/2))
  idx2 <- setdiff(1:2*nrow(A.comb), idx1)
  
  avg1[i] <- mean(A.comb$sum_out[idx1])
  avg2[i] <- mean(A.comb$sum_out[idx2])
  
}

avg_diff <- avg1 - avg2

hist(avg_diff)

original_diff <- mean(mu_mon) - mean(mu_thu)

quantile(avg_diff_sorted, 0.025) < original_diff && original_diff < quantile(avg_diff_sorted, 0.975)
























