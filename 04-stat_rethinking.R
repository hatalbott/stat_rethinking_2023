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

flist <- alist(
  height ~ dnorm(mu, sigma)
  , mu ~ dnorm(178, 20)
  , sigma ~ dunif(0, 50)
)

m4.1 <- quap(flist, data = d2)

precis(m4.1)

flist <- 

m4.2 <- quap(alist(
  height ~ dnorm(mu, sigma)
  , mu ~ dnorm(178, 0.1)
  , sigma ~ dunif(0, 50)
)
, data = d2
)

precis(m4.2)

vcov(m4.1)

diag(vcov(m4.1))

cov2cor(vcov(m4.1))

post <- extract.samples(m4.1, n = 1e4)

head(post)

precis(post)

plot(post)


## 4.4 Linear prediction -------------------------------------------------------

plot(d2$height ~ d2$weight)


## 4.4.1 The linear model strategy -----------------------------------------

set.seed(2971)

N <- 100

a <- rnorm(N, 178, 20)

b <- rnorm(N, 0, 10)

plot(NULL
     , xlim = range(d2$weight)
     , ylim=c(-100, 400)
     , xlab="weight"
     , ylab="height"
     )

abline(h=0, lty=2)

abline(h=272, lty=1, lwd=0.5)

mtext("b ~ dnorm(0, 10)")

xbar <- 

for (i in 1:N) {
  curve(a[i] + b[i]*(x-mean(d2$weight))
        , from=min(d2$weight)
        , to=max(d2$weight)
        , add=TRUE
        , col=col.alpha("black", 0.2)
        )
}

c <- rlnorm(1e4, 0, 1)

dens(c)

set.seed(2971)

b2 <- rlnorm(N, 0, 1)

plot(NULL
     , xlim = range(d2$weight)
     , ylim=c(-100, 400)
     , xlab="weight"
     , ylab="height"
)

abline(h=0, lty=2)

abline(h=272, lty=1, lwd=0.5)

mtext("log(b2) ~ dnorm(0, 1)")

  for (i in 1:N) {
    curve(a[i] + b2[i]*(x-mean(d2$weight))
          , from=min(d2$weight)
          , to=max(d2$weight)
          , add=TRUE
          , col=col.alpha("black", 0.2)
    )
  }


## 4.4.2 Finding posterior distribution ------------------------------------

m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b * (weight - mean(d2$weight))
    , a ~ dnorm(178, 20)
    , b ~ dlnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = d2
)

precis(m4.3)

round(vcov(m4.3), 3)

plot(height ~ weight, data=d2, col=rangi2)

post <- extract.samples(m4.3)

a_map <- mean(post$a)

b_map <- mean(post$b)

curve(a_map+b_map*(x-mean(d2$weight)), add=TRUE)

head(post)

dN <- d2[1:10,]

mN <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b * (weight - mean(weight))
    , a ~ dnorm(178, 20)
    , b ~ dlnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = dN
)

#extract 20 samples from posterior
post <- extract.samples(mN, n=20)

#display raw data and sample size
plot(dN$weight
    , dN$height
    , xlim=range(d2$weight)
    , ylim=range(d2$height)
    , col=rangi2
    , xlab="weight"
    , ylab="height"
    )

mtext(concat("N = ", nrow(dN)))

#plot the lines, with transparency
for(i in 1:nrow(post)){
curve(post$a[i] + post$b[i]*(x-mean(dN$weight))
      , col=col.alpha("black", 0.3)
      , add = TRUE
      )
}

dN <- d2[1:300,]

mN <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b * (weight - mean(weight))
    , a ~ dnorm(178, 20)
    , b ~ dlnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = dN
)

#extract 20 samples from posterior
post <- extract.samples(mN, n=20)

#display raw data and sample size
plot(dN$weight
     , dN$height
     , xlim=range(d2$weight)
     , ylim=range(d2$height)
     , col=rangi2
     , xlab="weight"
     , ylab="height"
)

mtext(concat("N = ", nrow(dN)))

#plot the lines, with transparency
for(i in 1:nrow(post)){
  curve(post$a[i] + post$b[i]*(x-mean(dN$weight))
        , col=col.alpha("black", 0.3)
        , add = TRUE
  )
}

post <- extract.samples(m4.3)

mu_at_50 <- post$a + post$b * (50-mean(d2$weight))

dens(mu_at_50)

PI(mu_at_50, prob=0.89)

mu <- link(m4.3)

str(mu)

weight.seq <- seq(from=25, to=70, by=1)

mu_sp <- link(m4.3, data=data.frame(weight=weight.seq))

str(mu_sp)

plot(height ~ weight, d2)

for (i in 1:1000) (
  points(weight.seq
         , mu_sp[i,]
         , pch=16
         , col=col.alpha(rangi2, 0.1)
         )
)

mu_sp.mean <- apply(mu_sp, 2, mean)
mu_sp.PI <- apply(mu_sp, 2, PI, prob=0.89)

#plot raw data
plot(height ~ weight
     , data = d2
     , col=col.alpha(rangi2, 0.5)
     )

#plot the MAP line aka mean mu
lines(weight.seq, mu_sp.mean)

#plot a shaded region for 89% PI
shade(mu_sp.PI, weight.seq)


mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI, prob=0.89)

#plot raw data
plot(height ~ weight
     , data = d2
     , col=col.alpha(rangi2, 0.5)
)

#plot the MAP line aka mean mu
lines(d2$weight, mu.mean)

#plot a shaded region for 89% PI
lines(d2$weight, mu.PI["5%",], col="gray")
lines(d2$weight, mu.PI["94%",], col="gray")

sim.height <- sim(m4.3, data=list(weight=weight.seq))
str(sim.height)

height.PI <- apply(sim.height, 2, PI, prob=0.89)
height.PI67 <- apply(sim.height, 2, PI, prob=0.67)
height.PI89 <- apply(sim.height, 2, PI, prob=0.89)
height.PI97 <- apply(sim.height, 2, PI, prob=0.97)

#plot raw data
plot(height ~ weight
     , data = d2
     , col=col.alpha(rangi2, 0.5)
)

#plot the MAP line aka mean mu
lines(weight.seq, mu_sp.mean)

#plot a shaded region for 89% PI
shade(mu_sp.PI, weight.seq)

#draw PI region for simulated heights
shade(height.PI67, weight.seq)
shade(height.PI89, weight.seq)
shade(height.PI97, weight.seq)


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



