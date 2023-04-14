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


# Book Homework ----------------------------------------------------------------

library(rethinking)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot( posterior ~ p_grid , type="l" )

set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )


## 3E1 -------------------------------------------------------------------

a <- sum( samples < 0.2 )
an3E1 <- a/1e4


## 3E2 -------------------------------------------------------------------

an3E2 <- sum( samples > 0.8 )/1e4


## 3E3 -------------------------------------------------------------------

an3E3a <- 1-(an3E1+an3E2)
an3E3b <- sum(samples > 0.2 & samples < 0.8)/1e4


## 3E4 -------------------------------------------------------------------

an3E4 <- quantile(samples, 0.2)


## 3E5 -------------------------------------------------------------------

an3E5 <- quantile(samples, 0.8)


## 3E6 -------------------------------------------------------------------

an3E6 <- HPDI( samples , prob=0.66 )


## 3E7 -------------------------------------------------------------------

an3E7 <- PI( samples , prob=0.66 )


## 3M1 -------------------------------------------------------------------

likelihood2 <- dbinom( 8 , size=15 , prob=p_grid )
posterior2 <- likelihood2 * prior
posterior2 <- posterior2 / sum(posterior2)

plot( posterior2 ~ p_grid , type="l" )

set.seed(100)
samples2 <- sample( p_grid , prob=posterior2 , size=1e4 , replace=TRUE )


## 3M2 -------------------------------------------------------------------

an3M2 <- HPDI(samples2, prob=0.9)


## 3M3 -------------------------------------------------------------------

b <- rbinom(1e4, size=15, prob=samples2)
simplehist(b, xlab="dummy water count")

an3M3 <- sum( b==8 ) / 1e4


## 3M4 -------------------------------------------------------------------

c <- rbinom(1e4, size=9, prob=samples2)
simplehist(c, xlab="dummy water count")

an3M4 <- sum( c==6 ) / 1e4


## 3M5 -------------------------------------------------------------------

prior3 <- ifelse(p_grid<0.5, 0, 1)
likelihood3 <- dbinom( 8 , size=15 , prob=p_grid )
posterior3 <- likelihood3 * prior3
posterior3 <- posterior3 / sum(posterior3)

plot( posterior3 ~ p_grid , type="l" )

set.seed(100)
samples3 <- sample( p_grid , prob=posterior3 , size=1e4 , replace=TRUE )

### 3M2 -------------------------------------------------------------------

an3M2a <- HPDI(samples3, prob=0.9)


### 3M3 -------------------------------------------------------------------

ba <- rbinom(1e4, size=15, prob=samples3)
simplehist(ba, xlab="dummy water count")

an3M3a <- sum( ba==8 ) / 1e4


### 3M4 -------------------------------------------------------------------

ca <- rbinom(1e4, size=9, prob=samples3)
simplehist(ca, xlab="dummy water count")

an3M4a <- sum( ca==6 ) / 1e4


## 3H1 ---------------------------------------------------------------------

## R code 3.28
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0,
            0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,
            1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,
            1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,
            1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,
            0,0,0,1,1,1,0,0,0,0)


p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,length(p_grid))

boys <- sum(birth1) + sum(birth2)
children <- length(birth1)+length(birth2
                                  )

likelihood4 <- dbinom(boys , size=children , prob=p_grid )
posterior4 <- likelihood4 * prior
posterior4 <- posterior4 / sum(posterior4)

plot( posterior4 ~ p_grid , type="l" )

an3H1 <- p_grid[which.max(posterior4)]


## 3H2 ---------------------------------------------------------------------

set.seed(100)
samples4 <- sample( p_grid , prob=posterior4 , size=1e4 , replace=TRUE )

an3H2a <- HPDI(samples4, prob=0.5)
an3H2b <- HPDI(samples4, prob=0.89)
an3H2c <- HPDI(samples4, prob=0.97)


## 3H3 ---------------------------------------------------------------------

d <- rbinom(1e4, size=200, prob=samples4)
simplehist(d, xlab="dummy boys count")

dens(d, adj=0.1)
abline( v=sum(birth1)+sum(birth2) , col="red",  lwd=3)


## 3H4 ---------------------------------------------------------------------

e <- rbinom(1e4, size=100, prob=samples4)
dens(e, adj=0.1)
abline( v=sum(birth1) , col="red",  lwd=3)


## 3H5 ---------------------------------------------------------------------

b01 <- birth2[birth1==0]

f <- rbinom(1e4, size=length(b01), prob=samples4)
dens(f, adj=0.1)


abline( v=sum(b01) , col="red",  lwd=3)


