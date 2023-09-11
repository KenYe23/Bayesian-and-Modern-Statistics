# estimate pi
x <- seq(from = 0, to = 1, by = 0.01)
y <- sqrt(1-x^2)
plot(x, y, type = 'l')

# simulate points in square
nsim <- 1e3

set.seed(42)
x_cord <- runif(n = nsim)
y_cord <- runif(n = nsim)

plot(x_cord, y_cord)
lines(x,y,col = 'red',lwd = 3)

# calculate # points in side
inside <- c()
for (i in 1:length(x_cord)){
  if (x_cord[i]^2 + y_cord[i]^2 <= 1){
    inside <- c(inside,1)
  } else {
    inside <- c(inside,0)
  }
}

## c = concatenate

4*sum(inside)/length(inside)

# fast way
inside_fast <- (x_cord^2 + y_cord^2 <=1)
4*sum(inside_fast)/length(inside_fast)

estimate_pi <- function (nsim, seed = 42){
  set.seed(seed)
  x <- runif(n = nsim)
  y <- runif(n = nsim)
  
  inside <- (x^2 + y^2 <= 1)
  pi_approx <- 4*sum(inside) / length(inside)
}

n<- 10^(1:5)
pi_approx <- sapply(n, estimate_pi)
abs_error <- abs(pi_approx - pi)

# library(latex2exp)
plot(
  log(n,base=10),
  abs_error,
  col = 'steelblue4',
  pch = 16,
  main = 'Approximation Error',
  xlab = Tex('$\\log_{10}(n)$')
  ylab = Tex('$|\\pi_*-\\pi|')
)
lines(log(n,base = 10), abs_error, col = 'steelblue1')
    

# Probability review
# Pr(theta = 0.5 | Y=2)
# Pr(Y = 2 | theta = 0.5)
p1 <- dbinom(2,size = 4, prob = 0.5)
# Pr(theta = 0.5)
p2 <- 0.5
# Pr(Y = 2)
# Pr(Y = 2 | theta = 0.5) Pr(theta=0.5)+Pr(Y=2|theta=0.25)*0.5
p3 <- dbinom(2, size = 4, prob = 0.5) *0.5 +
  dbinom(2, size = 4, prob = 0.25) *0.5

p1*p2/p3

# graph Pr(theta = 0.5) in [0,1]
calculate_posterior <- function(prior) {
  # prior:Pr(theta = 0.5)
  # Pr(theta = 0.25) = 1-prior
  
  # Pr(Y =2 | theta = 0.5)
  p1 <- dbinom(2,size = 4, prob = 0.5)
  # Pr(theta = 0.5)
  p2 <- prior
  # Pr(Y=2)
  p3 <- dbinom(2, size = 4, prob = 0.5) *prior +
    dbinom(2, size = 4, prob = 0.25) *(1-prior)
  
  return (p1*p2/p3)
}

prior_prob <-seq(from = 0, to = 1, length.out = 1001)
posterior <- calculate_posterior(prior_prob)
plot(prior_prob, posterior)

# median
# Y~Binomial(20,0.5)
qbinom(0.5, size = 20, prob=0.5)

y <-0:20
density_vec <- dbinom(y, size = 20, prob = 0.5)
plot(y,density_vec)

plot(y, cumsum(density_vec))

# find first which reach a value
median_index <- which(cumsum(density_vec)>= 0.5)[1]
median <- y[index]

# mode
which(density_vec == max(density_vec))
which.max(density_vec)
