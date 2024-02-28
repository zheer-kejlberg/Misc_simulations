library(dplyr)

n_min <- 30
n_mean <- 120
n_sd <- 13
N <- 220

sex_min <- 0.3
sex_max <- 0.7

create_dataset <- function() {
  n <- max(n_min,rnorm(1,n_mean,n_sd))
  sex_dist <- runif(1, sex_min, sex_max)
  alpha <- 1
  beta1 <- 1.15
  beta2 <- 1.06
  beta3 <- 1.3
  epsilon <- exp(rnorm(n))
  epsilon <- epsilon - mean(epsilon)
  
  df <- tibble(
    x = rnorm(n),
    sex = rbinom(n,1,sex_dist),
    y = exp(alpha) * exp(x * beta1) * exp(sex * beta2) * exp(x * sex * beta3) * exp(epsilon),
    logy = log(y)
  )
  mod <- lm(logy~x*sex,data=df)
  #res <- mod[["coefficients"]][2,1]
  reg <- summary(mod)
  if (reg[["coefficients"]][2,1] <= 0) {
    
    print(reg)
  }
  return(reg[["coefficients"]][2,1])
}

create_dataset()
t <- replicate(N,create_dataset())

hist(exp(t))
hist(t)
hist(log(t))


plot(density(exp(t)))
plot(density(t))
plot(density(log(t)))


plot(ecdf(exp(t)))
plot(ecdf(t))
plot(ecdf(log(t)))

