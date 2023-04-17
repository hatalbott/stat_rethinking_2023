library(rethinking)


# Book Chapter ------------------------------------------------------------

small <- replicate(1e4, prod(1+runif(12, 0, 0.01)))
dens(small)


big <- replicate(1e4, prod(1+runif(12, 0, 0.5)))
dens(big)

log.big <- replicate(1e4, log(prod(1+runif(12, 0, 0.5))))
dens(log.big)


## 4.3.1 Data -------------------------------------------------------------------

data("Howell1")

d <- Howell1

str(d)

precis(d)

d2 <- d[d$age >= 18, ]


## 4.3.2 Model -------------------------------------------------------------------

dens(d2$height)

curve(dnorm(x, 178, 20), from=100, to=250)

curve(dunif(x, 0, 50), from=-10, to=60)

sample_mu <- rnorm(1e4, 178, 20)

sample_sigma <- runif(1e4, 0, 50)

prior_h <- rnorm(1e4, sample_mu, sample_sigma)

dens(prior_h)

sample_mu2 <- rnorm(1e4, 178, 100)

prior_h2 <- rnorm(1e4, sample_mu2, sample_sigma)

dens(prior_h2)


## 4.3.3 Grid approx -------------------------------------------------------------------

## R code 4.16
mu.list <- seq( from=150, to=160 , length.out=100 )

sigma.list <- seq( from=7 , to=9 , length.out=100 )

post <- expand.grid( mu=mu.list , sigma=sigma.list )

post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )

post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )

post$prob <- exp( post$prod - max(post$prod) )

contour_xyz(post$mu, post$sigma, post$prob)

image_xyz(post$mu, post$sigma, post$prob)


## 4.3.4 Sampling from posterior -------------------------------------------------------------------

## R code 4.19
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )

sample.mu <- post$mu[ sample.rows ]

sample.sigma <- post$sigma[ sample.rows ]

## R code 4.20
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2,0.1) )

dens(sample.mu)

dens(sample.sigma)

PI(sample.mu)

PI(sample.sigma)

d3 <- sample(d2$height, size = 20)

## R code 4.24
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
  sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
              log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
  dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
                        prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
      col=col.alpha(rangi2,0.1) ,
      xlab="mu" , ylab="sigma" , pch=16 )

dens(sample2.sigma, norm.comp = TRUE)


## 4.3.5 Find posterior distribution -------------------------------------------------------------------


# Video -------------------------------------------------------------------




# S=1 female; S=2 male
sim_HW <- function(S, b, a) {
  N <- length(S)
  H <- ifelse(S==1, 150, 160) + rnorm(N, 0, 5)
  W <- a[S] + b[S]*H + rnorm(N, 0, 5)
  data.frame(S, H, W)
}

x <- sim_HW(S=rbern(100)+1, b=c(0.5, 0.6), a=c(0, 0))

#Female sample
simF <- sim_HW(S=rep(1,100), b=c(0.5, 0.6), a=c(0, 0))

#<ale sample
simM <- sim_HW(S=rep(2,100), b=c(0.5, 0.6), a=c(0, 0))

#Effect of sex (male-female)
diff <- mean(simM$W - simF$W)

#Observe sample
dat <- sim_HW(S=rbern(100)+1, b=c(0.5,0.6), a=c(0,0))

#Estimate Posterior
m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma)
    , mu <- a[S]
    , a[S] ~ dnorm(60, 10)
    , sigma ~ dunif(0,10)
  )
  , data=dat
)

precis(m_SW, depth=2)

#Analyze the real sample
data("Howell1")

d <- Howell1[Howell1$age>=18,]

dat = list(W=d$weight
            , S=d$male + 1 #S=1 female, S=2 male
            )

m_SW <- quap(
  alist(
    W ~ dnorm(mu, sigma)
    , mu <- a[S]
    , a[S] ~ dnorm(60, 10)
    , sigma ~ dunif(0,10)
  )
  , data=dat
)

precis(m_SW, depth=2)

# Posterior mean W
post <- extract.samples(m_SW)


dens(post$a[,1]
     , xlim=c(39,50)
     , lwd=3
     , col = 2
     , xlab="posterior mean weight (kg)"
     )

dens(post$a[,2]
     , lwd = 3
     , col = 4
     , add = TRUE
)

#Posterior W distributions
W1 <- rnorm(1000, post$a[,1], post$sigma)

W2 <- rnorm(1000, post$a[,2], post$sigma)

dens(W1
     , xlim=c(20,70)
     , ylim=c(0,0.085)
     , lwd=3
     , col = 2
     , xlab="posterior mean weight (kg)"
)

dens(W2
     , lwd = 3
     , col = 4
     , add = TRUE
)

#Causal contrast
str(post)

#causal contrast (in means)
mu_contrast <- post$a[,2] - post$a[,1]

dens(mu_contrast
     ,xlim=c(3,10)
     ,lwd=3
     ,col=1
     ,xlab="posterior mean weight contrast (kg)"
     )

#posterior W distributions
W1 <- rnorm(1e3, post$a[,1], post$sigma)
W2 <- rnorm(1e3, post$a[,2], post$sigma)

#contrast
W_contrast <- W2-W1

dens(W_contrast
     , xlim=c(-25,35)
     ,lwd=3
     ,col=1
     ,xlab="posterior weight contrast (kg)"
     )
abline( v=sum(W_contrast>0)/1e3 , col="red",  lwd=3)


#proportion above zero
sum(W_contrast>0)/1e3
#proportion below zero
sum(W_contrast<0)/1e3

#Analyze the sample
d <- Howell1[Howell1$age>=18,]

dat <- list(
  W = d$weight
  , H = d$height
  , Hbar = mean(d$height)
  , S = d$male +1
)

m_SHW <- quap(
  alist(
    W ~ dnorm(mu,sigma)
    , mu <- a[S] + b[S]*(H-Hbar)
    , a[S] ~ dnorm(60, 10)
    , b[S] ~ dunif(0, 1)
    , sigma ~ dunif(0, 10)
  )
  ,data=dat
)



