# Chapter 3 ---------------------------------------------------------------

# Owl workflow
# 1) State a clear question
# 2) sketch your causal assumptions
# 3) Use the sketch to define a generative model
# 4) Use generative model to build estimator
# 5) Profit

# Linear Regression
# 1) Questions/goal/estimand
# 2) Scientific model
# 3) Statistical model
# 4) Validate model
# 5) Analyze data

# function to simulate weights of individuals from height
sim_weight <- function(H, b, sd){
  U <- rnorm(length(H), 0, sd)
  W <- b*H + U
  return(W)
}

H <- runif(200, min = 130, max = 170)

W <- sim_weight(H, b=0.5, sd=5)

plot(W ~ H, col = 2, lwd = 3)

m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma)
    , mu <- a + b*H
    , a ~ dnorm(0, 10)
    , b ~ dunif(0, 1)
    , sigma ~ dunif(0, 10)
  )
  , data = list(W=W, H=H)
)

n <- 1e3
a <- rnorm(n, 0, 10)
b <- runif(n, 0, 1)
plot(NULL
     , xlim=c(0, 170)
     , ylim=c(0,90)
     , xlab= "height (cm)"
     , ylab= "weight (kg)"
     )

for(j in 1:50) abline(a=a[j], b=b[j], lwd=2, col=2)


#Step 4 - Validate Data

#simulate a sample of 10 people
set.seed(93)
H <- runif(10, 130, 170)
W <- sim_weight(H, b=0.5, sd=5)

#run the model
library(rethinking)
m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma)
    , mu <- a + b*H
    , a ~ dnorm(0, 10)
    , b ~ dunif(0, 1)
    , sigma ~ dunif(0, 10)
  )
  , data = list(W=W, H=H)
)

#summary
precis(m3.1)


#simulate a sample of 10 people
set.seed(93)
H <- runif(10, 130, 170)
W <- sim_weight(H, b=0.9, sd=5)

#run the model
m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma)
    , mu <- a + b*H
    , a ~ dnorm(0, 10)
    , b ~ dunif(0, 1)
    , sigma ~ dunif(0, 10)
  )
  , data = list(W=W, H=H)
)

#summary
precis(m3.1)

#simulate a sample of 1e4 people
set.seed(93)
H <- runif(1e4, 130, 170)
W <- sim_weight(H, b=0.5, sd=5)

#run the model
m3.1 <- quap(
  alist(
    W ~ dnorm(mu, sigma)
    , mu <- a + b*H
    , a ~ dnorm(0, 10)
    , b ~ dunif(0, 1)
    , sigma ~ dunif(0, 10)
  )
  , data = list(W=W, H=H)
)

#summary
precis(m3.1)

# 5 Analyze data
data("Howell1")
d <- Howell1[Howell1$age>=18,]

m3.2 <- quap(
  alist(
    W ~ dnorm(mu, sigma)
    , mu <- a + b*H
    , a ~ dnorm(0, 10)
    , b ~ dunif(0, 1)
    , sigma ~ dunif(0, 10)
  )
  , data = list(W=d$weight, H=d$height)
)

precis(m3.2)

post <- extract.samples(m3.2)
head(post)

plot(d$height
     , d$weight
     , col = 2
     , lwd = 3
     , xlab= "height (cm)"
     , ylab= "weight (kg)"
)

for (j in 1:20) abline(a=post$a[j], b=post$b[j], lwd=1)
