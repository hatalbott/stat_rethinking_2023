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


## 4.5 Curves from lines ---------------------------------------------------

library(rethinking)

data("Howell1")

d <- Howell1

plot(height ~ weight, d)

d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2

### Linear ---------------------------------------------------
#Linear approximation
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b*weight_s
    , a ~ dnorm(178, 20)
    , b ~ dlnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = d
)

precis(m4.3)

#Fit linear
weight.seq <- seq(from=-2.2, to=2, length.out=30)

pred_dat_lin <- list(weight_s=weight.seq)

mu_lin <- link(m4.3, data = pred_dat_lin)

mu.mean_lin <- apply(mu_lin, 2, mean)

mu.PI_lin <- apply(mu_lin, 2, PI, prob=0.89)

sim.height_lin <- sim(m4.3, data=pred_dat_lin)

height.PI_lin <- apply(sim.height_lin, 2, PI, prob=0.89)

#Plot linear
plot(height ~ weight_s, d, col=col.alpha(rangi2, 0.5))

lines(weight.seq, mu.mean_lin)

shade(mu.PI_lin, weight.seq)

shade(height.PI_lin, weight.seq)

### Quadratic ---------------------------------------------------
#Quadratic approximation
m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b1*weight_s + b2*weight_s2
    , a ~ dnorm(178, 20)
    , b1 ~ dlnorm(0, 1)
    , b2 ~ dnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = d
)

precis(m4.5)

weight.seq <- seq(from=-2.2, to=2, length.out=30)

pred_dat <- list(weight_s=weight.seq, weight_s2=weight.seq^2)

mu <- link(m4.5, data = pred_dat)

mu.mean <- apply(mu, 2, mean)

mu.PI <- apply(mu, 2, PI, prob=0.89)

sim.height <- sim(m4.5, data=pred_dat)

height.PI <- apply(sim.height, 2, PI, prob=0.89)

#Plot quadratic
plot(height ~ weight_s, d, col=col.alpha(rangi2, 0.5))

lines(weight.seq, mu.mean)

shade(mu.PI, weight.seq)

shade(height.PI, weight.seq)


### Parabolic ---------------------------------------------------
#Parabolic approximation
d$weight_s3 <- d$weight_s^3

m4.6 <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b1*weight_s + b2*weight_s2 + b3*weight_s3
    , a ~ dnorm(178, 20)
    , b1 ~ dlnorm(0, 1)
    , b2 ~ dnorm(0, 10)
    , b3 ~ dnorm(0, 10)
    , sigma ~ dunif(0, 50)
  ), data = d
)

precis(m4.6)


#Parabolic fitting

pred_dat_par <- list(weight_s=weight.seq, weight_s2=weight.seq^2, weight_s3=weight.seq^3)

mu_par <- link(m4.6, data = pred_dat_par)

mu.mean_par <- apply(mu_par, 2, mean)

mu.PI_par <- apply(mu_par, 2, PI, prob=0.89)

sim.height_par <- sim(m4.6, data=pred_dat_par)

height.PI_par <- apply(sim.height_par, 2, PI, prob=0.89)


#Plot parabolic
plot(height ~ weight_s, d, col=col.alpha(rangi2, 0.5), xaxt="n")

lines(weight.seq, mu.mean_par)

shade(mu.PI_par, weight.seq)

shade(height.PI_par, weight.seq)

at <- c(-2,-1,0,1,2)
labels <- at*sd(d$weight)+mean(d$weight)
axis(side = 1, at=at, labels = round(labels, 1))

rm(list = ls())

## 4.5.2 Splines -----------------------------------------------------------

library(rethinking)

data("cherry_blossoms")

d <- cherry_blossoms

precis(d)

plot(doy ~ year, d)

d2 <- d[complete.cases(d$doy),]

num_knots <- 15

knot_list <- quantile(d2$year, probs = seq(0, 1, length.out=num_knots))

library(splines)

#create splines
B <- bs(d2$year
        , knots = knot_list[-c(1,num_knots)]
        , degree = 3
        , intercept = TRUE
        )

#plot splines
plot(NULL
     , xlim = range(d2$year)
     , ylim = c(0,1)
     , xlab = "year"
     , ylab = "basis"
     )

for (i in 1:ncol(B)) {
  lines(d2$year, B[,i])
}

m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + B %*% w
    , a ~ dnorm(100, 10)
    , w ~ dnorm(0, 10)
    , sigma ~ dexp(1)
  ), data = list(D=d2$doy, B=B)
  , start = list(w=rep(0, ncol(B)))
)

precis(m4.7, depth = 2)

post <- extract.samples(m4.7)

w <- apply(post$w, 2, mean)

plot(NULL
     , xlim = range(d2$year)
     , ylim = c(-6,6)
     , xlab = "year"
     , ylab = "basis * weight"
     )

for (i in 1:ncol(B)) {
  lines(d2$year, w[i]*B[,i])
}

mu <- link(m4.7)

mu_PI <- apply(mu, 2, PI, prob=0.97)

plot(d2$year
     , d2$doy
     , col=col.alpha(rangi2, 0.3)
     , pch=16
     )

shade(mu_PI
      , d2$year
      , col=col.alpha("black", 0.3)
      )

rm(list = ls())

# Book Homework -----------------------------------------------------------


## 4M1 ---------------------------------------------------------------------

sample_mu <- rnorm(1e4, 0, 10)

sample_sigma <- rexp(1e4, 1)

dens(sample_mu)

dens(sample_sigma)

prior_y <- rnorm(1e4, sample_mu, sample_sigma)

dens(prior_y)


## 4M2 ---------------------------------------------------------------------

m4m2 <- quap(
  alist(
    y ~ dnorm(mu, sigma)
    mu ~ dnorm(0, 10)
    sigma ~ dexp(1)
  )
)

## 4M7 ---------------------------------------------------------------------

data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18, ]

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
diag(vcov(m4.3))
cov2cor(vcov(m4.3))

plot(height ~ weight, data=d2, col=rangi2)

post <- extract.samples(m4.3)

precis(post)

plot(post)

HPDI(post, prob=0.89)

a_map <- mean(post$a)

b_map <- mean(post$b)

curve(a_map+b_map*(x-mean(d2$weight)), add=TRUE)

head(post)

m4.3b <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b * (weight)
    , a ~ dnorm(178, 20)
    , b ~ dlnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = d2
)

precis(m4.3b)

round(vcov(m4.3b), 3)
diag(vcov(m4.3b))
cov2cor(vcov(m4.3b))

plot(height ~ weight, data=d2, col=rangi2)

postb <- extract.samples(m4.3b)

precis(postb)

plot(postb)

HPDI(postb, prob=0.89)

a_mapb <- mean(postb$a)

b_mapb <- mean(postb$b)

curve(a_mapb+b_mapb*(x), add=TRUE)

head(post)

# Each entry shows the correlation for each pair of parameters. 
# The 1's indicate a paramenter's correlation with itself. 
# BAD if anything except 1. 
# The other entries are typically closer to zero. 
# This indicates that learning a tells us nothing about b. 
# But because we didn't correct for mean weight (xbar) in m4.3b learning about a "the intercept/expected height at xbar" 
# does give us information about b "the slope/rate of change in expectation" best seen with plot(post) and plot (postb)

rm(list = ls())

## 4M8 ---------------------------------------------------------------------

library(rethinking)
library(splines)

data("cherry_blossoms")

d <- cherry_blossoms

precis(d)

plot(doy ~ year, d)

d2 <- d[complete.cases(d$doy),]


### Original ----------------------------------------------------------------

num_knots <- 15

knot_list <- quantile(d2$year, probs = seq(0, 1, length.out=num_knots))

#create splines
B <- bs(d2$year
        , knots = knot_list[-c(1,num_knots)]
        , degree = 3
        , intercept = TRUE
)

#plot splines
plot(NULL
     , xlim = range(d2$year)
     , ylim = c(0,1)
     , xlab = "year"
     , ylab = "basis"
)

for (i in 1:ncol(B)) {
  lines(d2$year, B[,i])
}

m4.7 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + B %*% w
    , a ~ dnorm(100, 10)
    , w ~ dnorm(0, 10)
    , sigma ~ dexp(1)
  ), data = list(D=d2$doy, B=B)
  , start = list(w=rep(0, ncol(B)))
)

precis(m4.7, depth = 2)

post <- extract.samples(m4.7)

w <- apply(post$w, 2, mean)

plot(NULL
     , xlim = range(d2$year)
     , ylim = c(-6,6)
     , xlab = "year"
     , ylab = "basis * weight"
)

for (i in 1:ncol(B)) {
  lines(d2$year, w[i]*B[,i])
}

mu <- link(m4.7)

mu_PI <- apply(mu, 2, PI, prob=0.97)

plot(d2$year
     , d2$doy
     , col=col.alpha(rangi2, 0.3)
     , pch=16
     , main="15 knots, sigma ~ dexp(1)"
)

shade(mu_PI
      , d2$year
      , col=col.alpha("black", 0.3)
)


### Double knots ------------------------------------------------------------

num_knots2 <- 30

knot_list2 <- quantile(d2$year, probs = seq(0, 1, length.out=num_knots2))

#create splines
B2 <- bs(d2$year
        , knots = knot_list2[-c(1,num_knots)]
        , degree = 3
        , intercept = TRUE
)

#plot splines
plot(NULL
     , xlim = range(d2$year)
     , ylim = c(0,1)
     , xlab = "year"
     , ylab = "basis"
)

for (i in 1:ncol(B2)) {
  lines(d2$year, B2[,i])
}

m4.72 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + B %*% w
    , a ~ dnorm(100, 10)
    , w ~ dnorm(0, 10)
    , sigma ~ dexp(1)
  ), data = list(D=d2$doy, B=B2)
  , start = list(w=rep(0, ncol(B2)))
)

precis(m4.72, depth = 2)

post2 <- extract.samples(m4.72)

w2 <- apply(post2$w, 2, mean)

plot(NULL
     , xlim = range(d2$year)
     , ylim = c(-6,6)
     , xlab = "year"
     , ylab = "basis * weight"
)

for (i in 1:ncol(B2)) {
  lines(d2$year, w2[i]*B2[,i])
}

mu2 <- link(m4.72)

mu_PI2 <- apply(mu2, 2, PI, prob=0.97)

plot(d2$year
     , d2$doy
     , col=col.alpha(rangi2, 0.3)
     , pch=16
     , main="30 knots, sigma ~ dexp(1)"
)

shade(mu_PI2
      , d2$year
      , col=col.alpha("black", 0.3)
)

### Change Weight we assign to each segment of spline ----------------------------------------------------------------

m4.73 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + B %*% w
    , a ~ dnorm(100, 10)
    , w ~ dnorm(0, 2) #Change is here
    , sigma ~ dexp(1)
  ), data = list(D=d2$doy, B=B)
  , start = list(w=rep(0, ncol(B)))
)

precis(m4.73, depth = 2)

post3 <- extract.samples(m4.73)

w3 <- apply(post3$w, 2, mean)

plot(NULL
     , xlim = range(d2$year)
     , ylim = c(-6,6)
     , xlab = "year"
     , ylab = "basis * weight"
)

for (i in 1:ncol(B)) {
  lines(d2$year, w3[i]*B[,i])
}

mu3 <- link(m4.73)

mu_PI3 <- apply(mu3, 2, PI, prob=0.97)

plot(d2$year
     , d2$doy
     , col=col.alpha(rangi2, 0.3)
     , pch=16
     , main="15 knots, sigma ~ dexp(1)"
)

shade(mu_PI3
      , d2$year
      , col=col.alpha("black", 0.3)
)

### Width of prior dunif(0, 50)------------------------------------------------------------
##This doesn't make much difference
m4.73 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + B %*% w
    , a ~ dnorm(100, 10)
    , w ~ dnorm(0, 10)
    , sigma ~ dunif(0, 50)
  ), data = list(D=d2$doy, B=B)
  , start = list(w=rep(0, ncol(B)))
)

precis(m4.73, depth = 2)

post3 <- extract.samples(m4.73)

w3 <- apply(post3$w, 2, mean)

plot(NULL
     , xlim = range(d2$year)
     , ylim = c(-6,6)
     , xlab = "year"
     , ylab = "basis * weight"
)

for (i in 1:ncol(B)) {
  lines(d2$year, w3[i]*B[,i])
}

mu3 <- link(m4.73)

mu_PI3 <- apply(mu3, 2, PI, prob=0.97)

plot(d2$year
     , d2$doy
     , col=col.alpha(rangi2, 0.3)
     , pch=16
     , main="15 knots, sigma ~ dunif(0, 50)"
)

shade(mu_PI3
      , d2$year
      , col=col.alpha("black", 0.3)
)

### Width of prior dexp(10)------------------------------------------------------------
##This doesn't make much difference

m4.74 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + B %*% w
    , a ~ dnorm(100, 10)
    , w ~ dnorm(0, 10)
    , sigma ~ dexp(100)
  ), data = list(D=d2$doy, B=B)
  , start = list(w=rep(0, ncol(B)))
)

precis(m4.74, depth = 2)

post4 <- extract.samples(m4.74)

w4 <- apply(post4$w, 2, mean)

plot(NULL
     , xlim = range(d2$year)
     , ylim = c(-6,6)
     , xlab = "year"
     , ylab = "basis * weight"
)

for (i in 1:ncol(B)) {
  lines(d2$year, w4[i]*B[,i])
}

mu4 <- link(m4.74)

mu_PI4 <- apply(mu4, 2, PI, prob=0.97)

plot(d2$year
     , d2$doy
     , col=col.alpha(rangi2, 0.3)
     , pch=16
     , main="15 knots, sigma ~ dexp(100)"
)

shade(mu_PI4
      , d2$year
      , col=col.alpha("black", 0.3)
)

rm(list = ls())


## 4H1 ---------------------------------------------------------------------

#1 Question: find expected height based on weight

data("Howell1")

d <- Howell1

plot(d$height ~ d$weight)

d2 <- d[d$age>=18,]

plot(d2$height ~ d2$weight_s)

#2 Assumptions: 

set.seed(2971)

N <- 100

a <- rnorm(N, 178, 20)

b <- rlnorm(N, 0, 1)

plot(NULL
     , xlim = range(d2$weight)
     , ylim=c(-100, 400)
     , xlab="weight"
     , ylab="height"
)

abline(h=0, lty=2)

abline(h=272, lty=1, lwd=0.5)

mtext("b ~ dlnorm(0, 10)")

xbar <- mean(d2$weight)
  
  for (i in 1:N) {
    curve(a[i] + b[i]*(x-xbar)
          , from=min(d2$weight)
          , to=max(d2$weight)
          , add=TRUE
          , col=col.alpha("black", 0.2)
    )
  }

#3 Generative model

#Linear approximation
m4.3 <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b*weight
    , a ~ dnorm(178, 20)
    , b ~ dlnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = d2
)

precis(m4.3)

post <- extract.samples(m4.3)

str(post)

y1 <- rnorm(1e4, post$a + post$b*46.95, post$sigma)
y2 <- rnorm(1e4, post$a + post$b*43.72, post$sigma)
y3 <- rnorm(1e4, post$a + post$b*64.78, post$sigma)
y4 <- rnorm(1e4, post$a + post$b*32.59, post$sigma)
y5 <- rnorm(1e4, post$a + post$b*54.63, post$sigma)

#5 Profit

p1 <- mean(y1)
p1_PI <- PI(y1, prob = 0.89)
  
p2 <- mean(y2)
p2_PI <- PI(y2, prob = 0.89)

p3 <- mean(y3)
p3_PI <- PI(y3, prob = 0.89)

p4 <- mean(y4)
p4_PI <- PI(y4, prob = 0.89)

p5 <- mean(y5)
p5_PI <- PI(y5, prob = 0.89)

rm(list = ls())


## 4H2 ---------------------------------------------------------------------

data("Howell1")

d <- Howell1[Howell1$age<18,]

plot(d$height ~ d$weight)

### 4H2a ---------------------------------------------------------------------

#2 Assumptions: 

set.seed(2971)

N <- 100

a <- rnorm(N, 100, 50)

b <- rlnorm(N, 0, 1)

plot(NULL
     , xlim = range(d$weight)
     , ylim = c(-10, 180)
     , xlab="weight"
     , ylab="height"
)

abline(h=0, lty=2)

mtext("Priors graph")

xbar <- mean(d$weight)

for (i in 1:N) {
  curve(a[i] + b[i]*(x-xbar)
        , from=min(d$weight)
        , to=max(d$weight)
        , add=TRUE
        , col=col.alpha("black", 0.2)
  )
}

#3 Generative model

#Linear approximation
m2H2a <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b*weight
    , a ~ dnorm(100, 50)
    , b ~ dlnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = d
)

precis(m2H2a)

an2H2a <- 10*2.72


### 4H2b ---------------------------------------------------------------------

mu_lin <- link(m2H2a)

mu.mean_lin <- apply(mu_lin, 2, mean)

mu.PI_lin <- apply(mu_lin, 2, PI, prob=0.89)

#Plot linear
plot(height ~ weight, d, col=col.alpha(rangi2, 0.5))

lines(d$weight, mu.mean_lin)

shade(mu.PI_lin, d$weight)


### 4H2b ---------------------------------------------------------------------

#Tails clearly do not fit linear line, change to quadratic or above model

rm(list=ls())

## 4H3 ---------------------------------------------------------------------
### 4H3a ---------------------------------------------------------------------

library(rethinking)

data("Howell1")

d4H3 <- Howell1

d4H3$weight_log <- log(d4H3$weight)

plot(height ~ weight, data=d4H3)

plot(height ~ weight_log, data=d4H3)

m4H3a <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b*weight_log
    , a ~ dnorm(138, 100)
    , b ~ dnorm(0, 100)
    , sigma ~ dunif(0, 50)
  ), data = d4H3
)

precis(m4H3a)


### 4H3b ---------------------------------------------------------------------

post <- extract.samples(m4H3a)

range(d4H3$weight_log)

lw.seq <- seq(from = floor(min(d4H3$weight_log)*10)/10, to = ceiling(max(d4H3$weight_log)*10)/10, length.out = 50)

pred_dat_lin <- list(weight_log=lw.seq)

mu_lin <- link(m4H3a, data = pred_dat_lin)

mu.mean_lin <- apply(mu_lin, 2, mean)

mu.PI_lin <- apply(mu_lin, 2, PI, prob = 0.97)

sim.height_lin <- sim(m4H3a, data=pred_dat_lin)

height.PI_lin <- apply(sim.height_lin, 2, PI, prob=0.97)

#Plot linear
plot(height ~ weight, d4H3, col=col.alpha(rangi2, 0.5))

lines(exp(lw.seq), mu.mean_lin)

shade(mu.PI_lin, exp(lw.seq))

shade(height.PI_lin, exp(lw.seq))

lines(exp(lw.seq), mu.PI_lin[1,], lty=2)
lines(exp(lw.seq), mu.PI_lin[2,], lty=2)

lines(exp(lw.seq), height.PI_lin[1,], lty=3)
lines(exp(lw.seq), height.PI_lin[2,], lty=3)

##My CI lines are much narrower than in practice problems bc I was looking at CI of mu rather than CI of height predication

rm(list=ls())

## 4H4 ---------------------------------------------------------------------

data("Howell1")

d <- Howell1

d$weight_s <- (d$weight - mean(d$weight))/sd(d$weight)
d$weight_s2 <- d$weight_s^2
d$weight_s3 <- d$weight_s^3

plot(d$height ~ d$weight_s)

set.seed(2971)

N <- 100

a <- rnorm(N, 180, 20)

b1 <- rnorm(N, 50, 20)

b2 <- runif(N, -100, 1)

plot(NULL
     , xlim = range(d$weight_s)
     , ylim=c(-100, 400)
     , xlab="weight"
     , ylab="height"
)

abline(h=0, lty=2)

abline(h=272, lty=1, lwd=0.5)

mtext("Quadratic Priors")

  for (i in 1:N) {
    curve(a[i] + b1[i]*x + b2[i]*x^2
          , from=min(d$weight_s)
          , to=max(d$weight_s)
          , add=TRUE
          , col=col.alpha("black", 0.2)
    )
  }

library(tidyverse)

n <- 1000
tibble(group = seq_len(n),
       alpha = rnorm(n, 178, 20),
       beta1 = rlnorm(n, 0, 1),
       beta2 = rnorm(n, 0, 1)) %>%
  expand(nesting(group, alpha, beta1, beta2),
         weight = seq(25, 70, length.out = 100)) %>%
  mutate(height = alpha + (beta1 * weight) + (beta2 * (weight ^ 2))) %>%
  ggplot(aes(x = weight, y = height, group = group)) +
  geom_line(alpha = 1 / 10) +
  geom_hline(yintercept = c(0, 272), linetype = 2:1, color = "red") +
  annotate(geom = "text", x = 25, y = 0, hjust = 0, vjust = 1,
           label = "Embryo") +
  annotate(geom = "text", x = 25, y = 272, hjust = 0, vjust = 0,
           label = "World's tallest person (272cm)") +
  coord_cartesian(ylim = c(-25, 300)) +
  labs(x = "Weight", y = "Height")

n <- 1000
tibble(group = seq_len(n),
       alpha = rnorm(n, -190, 5),
       beta1 = rnorm(n, 13, 0.2),
       beta2 = runif(n, -0.13, -0.1)) %>%
  expand(nesting(group, alpha, beta1, beta2),
         weight = seq(25, 70, length.out = 100)) %>%
  mutate(height = alpha + (beta1 * weight) + (beta2 * (weight ^ 2))) %>%
  ggplot(aes(x = weight, y = height, group = group)) +
  geom_line(alpha = 1 / 10) +
  geom_hline(yintercept = c(0, 272), linetype = 2:1, color = "red") +
  annotate(geom = "text", x = 25, y = -3, hjust = 0, vjust = 1,
           label = "Embryo") +
  annotate(geom = "text", x = 25, y = 275, hjust = 0, vjust = 0,
           label = "World's tallest person (272cm)") +
  coord_cartesian(ylim = c(-25, 300)) +
  labs(x = "Weight", y = "Height")

rm(list=ls())

## 4H5 ---------------------------------------------------------------------

data("cherry_blossoms")

d <- cherry_blossoms
d2 <- d[complete.cases(d$doy),]
d3 <- d2[complete.cases(d2$temp),]

plot(d3$doy ~ d3$temp)

d3$temp_s <- (d3$temp - mean(d3$temp))/sd(d3$temp)
d3$temp_s2 <- d3$temp_s^2
d3$temp_s3 <- d3$temp_s^3

plot(d3$doy ~ d3$temp_s)


### Linear ---------------------------------------------------
#Priors

N <- 100

a <- rnorm(N, 100, 100)

b1 <- rnorm(N, 0, 100)

plot(NULL
     , xlim = range(d3$temp_s)
     , ylim=c(0, 400)
     , xlab="temp"
     , ylab="doy"
)

mtext("Linear Priors")

for (i in 1:N) {
  curve(a[i] + b1[i]*x
        , from=min(d3$temp_s)
        , to=max(d3$temp_s)
        , add=TRUE
        , col=col.alpha("black", 0.2)
  )
}

#Linear approximation
m4H5 <- quap(
  alist(
    doy ~ dnorm(mu, sigma)
    , mu <- a + b1*temp_s
    , a ~ dnorm(100, 100)
    , b1 ~ dnorm(0, 100)
    , sigma ~ dunif(0, 50)
  ), data = d3
)

precis(m4H5)

#Fit linear
range(d3$temp_s)

temp.seq <- seq(from = floor(min(d3$temp_s)*10)/10, to = ceiling(max(d3$temp_s)*10)/10, length.out = 50)

pred_dat_lin <- list(temp_s=temp.seq)

mu_lin <- link(m4H5, data = pred_dat_lin)

mu.mean_lin <- apply(mu_lin, 2, mean)

mu.PI_lin <- apply(mu_lin, 2, PI, prob=0.89)

sim.doy_lin <- sim(m4H5, data=pred_dat_lin)

doy.PI_lin <- apply(sim.doy_lin, 2, PI, prob=0.89)

#Plot linear
plot(doy ~ temp_s, d3, col=col.alpha(rangi2, 0.5))

lines(temp.seq, mu.mean_lin)

shade(mu.PI_lin, temp.seq)

shade(doy.PI_lin, temp.seq)

### Quadratic ---------------------------------------------------
#Quadratic approximation
m4.5 <- quap(
  alist(
    height ~ dnorm(mu, sigma)
    , mu <- a + b1*weight_s + b2*weight_s2
    , a ~ dnorm(178, 20)
    , b1 ~ dlnorm(0, 1)
    , b2 ~ dnorm(0, 1)
    , sigma ~ dunif(0, 50)
  ), data = d
)

precis(m4.5)

weight.seq <- seq(from=-2.2, to=2, length.out=30)

pred_dat <- list(weight_s=weight.seq, weight_s2=weight.seq^2)

mu <- link(m4.5, data = pred_dat)

mu.mean <- apply(mu, 2, mean)

mu.PI <- apply(mu, 2, PI, prob=0.89)

sim.height <- sim(m4.5, data=pred_dat)

height.PI <- apply(sim.height, 2, PI, prob=0.89)

#Plot quadratic
plot(height ~ weight_s, d, col=col.alpha(rangi2, 0.5))

lines(weight.seq, mu.mean)

shade(mu.PI, weight.seq)

shade(height.PI, weight.seq)




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



