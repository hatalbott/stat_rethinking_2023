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



