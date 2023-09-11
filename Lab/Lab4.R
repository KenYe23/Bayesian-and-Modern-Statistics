a1 <- 20
b1 <- 10
a2 <- 70
b2 <- 10
w <- 0.2
y <- 3

prior <- function (lambda){
  w*dgamma(lambda, a1, b1) + (1-w)*dgamma(lambda,a2,b2)
}

integrate (prior, 0, Inf)

lambda <- seq(from = 0, to = 15, by = 0.01)
prior_dens <- prior(lambda)

plot(lambda, prior(lambda), type = 'l')

# sample from prior
coin <- rbinom(n = 1, size = 1, prob = w)
if (coin == 1){
  lam <- rgamma(n = 1, shape = a1, rate = b1)
} else {
  lam <- rgamma(n = 1, shape = a2, rate = b2)
}

sample_prior <- function(nsim = 1e4, seed = 42){
  set.seed(42)
  lam <- c()
  
  for (i in 1:nsim){
    coin <- rbinom(n = 1, size = 1, prob = w)
    if (coin == 1){
      lam <- rgamma(n = 1, shape = a1, rate = b1)
    } else {
      lam <- rgamma(n = 1, shape = a2, rate = b2)
    }
  }
  
  return (lam)
}

sim_lam <- sample_prior()
hist(sim_lam)

wpost <- function(y, w, a1, b1, a2, b2){
  num <- w*dbinom(y, mu = a1/b1, size = b1)
  den <- num + (1-w)*dnbinom(y, mu = a2/b2, size = b2)
  return (num/den)
}
wstar <- wpost(y, w, a1, b1, a2, b2)

posterior <- function(lam){
  wstar* dgamma(lam, shape = a1+y, rate = b1+1)+
    (1-wstar)*dgamma(lam, shape =a2+y, size = b2 +1)
}

integrate(posterior, 0, Inf)
