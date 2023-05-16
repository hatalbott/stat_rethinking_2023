library(rethinking)


# 5.1 Spurious association ------------------------------------------------

data("WaffleDivorce")

# standardize variables
WaffleDivorce$D <- standardize(WaffleDivorce$Divorce)
WaffleDivorce$M <- standardize(WaffleDivorce$Marriage)
WaffleDivorce$A <- standardize(WaffleDivorce$MedianAgeMarriage)

plot(WaffleDivorce$MedianAgeMarriage ~ WaffleDivorce$Divorce)

sd(WaffleDivorce$MedianAgeMarriage)

m5.1 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <- a + bA * A
    , a ~ dnorm(0, 0.2)
    , bA ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
  ), data = WaffleDivorce
)

precis(m5.1)

#Simulate priors
set.seed(10)

prior <- extract.prior(m5.1)

mu <-  link(m5.1, post = prior, data = list(A = c(-2, 2)))

plot(NULL
     , xlim = c(-2, 2)
     , ylim = c(-2, 2)
     )

for (i in 1:50) {
  lines(c(-2, 2), mu[i,], col = col.alpha("black", 0.4))
  
}

#Posterior predication
# compute percentile interval of mean

A_seq <- seq(from = floor(min(WaffleDivorce$A)*10)/10, to = ceiling(max(WaffleDivorce$A)*10)/10, length.out = 30) #Not exactly what is used in the book but more generalizible

mu_post <- link(m5.1, data = list(A = A_seq))

mu_post.mean <- apply(mu_post, 2, mean)

mu_post.PI <- apply(mu_post, 2, PI)

# plot
plot(D ~ A, data = WaffleDivorce, col = rangi2)

lines(A_seq, mu_post.mean, lwd = 2)

shade(mu_post.PI, A_seq)

m5.2 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + bM * M
    , a ~ dnorm(0, 0.2)
    , bM ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
  ), data = WaffleDivorce
)

precis(m5.2)

# Drawing DAGs
library(dagitty)

dag5.1 <- dagitty("dag{A -> D; A -> M; M -> D}")

coordinates(dag5.1) <- list(x = c(A=0, D=1, M=2), y = c(A=0, D=1, M=0))

plot(graphLayout(dag5.1))

drawdag(dag5.1)

cor(WaffleDivorce$A, WaffleDivorce$M)

#DAG 1
DMA_dag1 <- dagitty('dag{D <- A -> M -> D}')

impliedConditionalIndependencies(DMA_dag1)

#DAG 2
DMA_dag2 <- dagitty('dag{D <- A -> M}')

impliedConditionalIndependencies(DMA_dag2) #D is independent of M, conditional on A


## 5.1.4 Approximating the posterior ----------------------------------------

m5.3 <- quap(
  alist(
    D ~ dnorm(mu, sigma)
    , mu <-  a + bM*M + bA*A
    , a ~ dnorm(0, 0.2)
    , bM ~ dnorm(0, 0.5)
    , bA ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
  ), data = WaffleDivorce
)

precis(m5.3)

### reset mar
par(mar = c(5,4,4,2))

plot(coeftab(m5.1, m5.2, m5.3)
     , par = c("bA", "bM")
     , xlab = "Estimate"
     )

# Relationship between age at marriage and marriage rate
m5.am <- quap(
  alist(
    M ~ dnorm(mu, sigma)
    , mu <- a + bA * A
    , a ~ dnorm(0, 0.2)
    , bA ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
  ), data = WaffleDivorce
)

precis(m5.am)

#Posterior predication
# compute percentile interval of mean

mu_am_post <- link(m5.am, data = list(A = A_seq))

mu_am_post.mean <- apply(mu_am_post, 2, mean)

mu_am_post.PI <- apply(mu_am_post, 2, PI)

# plot
plot(M ~ A, data = WaffleDivorce, col = rangi2)

lines(A_seq, mu_am_post.mean, lwd = 2)

shade(mu_am_post.PI, A_seq)


##5.1.5 Plotting multivariate posteriors ------------------------------------------------

m5.4 <- quap(
  alist(
    M ~ dnorm(mu, sigma)
    , mu <- a + bAM * A
    , a ~ dnorm(0, 0.2)
    , bAM ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
  ), data = WaffleDivorce
)

mu <-  link(m5.4)

mu_am_mean <- apply(mu, 2, mean)

mu_am_resid <- WaffleDivorce$M - mu_am_mean


###5.1.5.1 Predictor residual plots ------------------------------------------------
#These plots show the outcome against residual predictor values. 
#They are useful for understanding the statical model, but not much else.

#Marriage rate
plot(WaffleDivorce$M ~ WaffleDivorce$A, col = rangi2)

lines(A_seq, mu_am_post.mean, lwd = 2)

segments(WaffleDivorce$A, WaffleDivorce$M
         , WaffleDivorce$A,  mu_am_mean
)


plot(WaffleDivorce$D ~ mu_am_resid)

abline(lm(WaffleDivorce$D ~ mu_am_resid))

abline(v = 0, lty = 2)

#Age at marriage
m5.4a <- quap(
  alist(
    A ~ dnorm(mu, sigma)
    , mu <- a + bAM * M
    , a ~ dnorm(0, 0.2)
    , bAM ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
  ), data = WaffleDivorce
)

M_seq <- seq(from = floor(min(WaffleDivorce$M)*10)/10, to = ceiling(max(WaffleDivorce$M)*10)/10, length.out = 30) #Not exactly what is used in the book but more generalizible

mu_a <-  link(m5.4a)
mu_a_mean <- apply(mu_a, 2, mean)
mu_a_resid <- WaffleDivorce$A - mu_a_mean

mu_a_post <- link(m5.4a, data = list(M = M_seq))
mu_a_post.mean <- apply(mu_a_post, 2, mean)

plot(WaffleDivorce$A ~ WaffleDivorce$M, col = rangi2)

lines(M_seq, mu_a_post.mean, lwd = 2)

segments(WaffleDivorce$M, WaffleDivorce$A
         , WaffleDivorce$M,  mu_a_mean
)

plot(WaffleDivorce$D ~ mu_a_resid, col = rangi2)

abline(lm(WaffleDivorce$D ~ mu_a_resid))

abline(v = 0, lty = 2)

###5.1.5.2 Posterior prediction plots ------------------------------------------------
#These show model-based predictions against raw data, 
#or otherwise display the error in prediction. 
#They are tools for checking fit and assessing predictions. \
#They are not causal tools.

#call link without specifying new data
#so it uses original data
mu <- link(m5.3)

# summarize samples across cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

# simulate observations
# again no new data, so uses original data
D_sim <- sim(m5.3, n = 1e4)
D_PI <- apply(D_sim, 2, PI)

plot(mu_mean ~ WaffleDivorce$D
     , col = rangi2
     , ylim = range(mu_PI)
     , xlab = "Observed divorce"
     , ylab = "Predicted divorce"
     )

abline(a=0, b=1, lty=2)

for (i in 1:nrow(WaffleDivorce)) {
  lines(rep(WaffleDivorce$D[i], 2)
        , mu_PI[,i]
        , col = rangi2
        )
}

identify(x=WaffleDivorce$D
         , y = mu_mean
         , labels = WaffleDivorce$Loc
         )


###5.1.5.3 Counterfactual plots ------------------------------------------------
#These show the implied predictions for imaginary experiments.
#These plots allow you to explore the causal implications of 
#manipulating one or more variables.

m5.3_A <- A <- quap(
  alist(
    ## A -> D <- M
    D ~ dnorm(mu, sigma)
    , mu <-  a + bM*M + bA*A
    , a ~ dnorm(0, 0.2)
    , bM ~ dnorm(0, 0.5)
    , bA ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
    
    ## A -> M
    , M ~ dnorm(mu_M, sigma_M)
    , mu_M <-  aM + bAM*A
    , aM ~ dnorm(0, 0.2)
    , bAM ~ dnorm(0, 0.5)
    , sigma_M ~ dexp(1)
  ), data = WaffleDivorce
)

precis(m5.3_A)

A_seq <- seq(from = -2, to = 2, length.out = 30)

#prep data
sim_dat <- data.frame(A = A_seq)

#simulate M and then D, using A_seq
s <- sim(m5.3_A, data = sim_dat, vars = c("M", "D"))

plot(sim_dat$A, colMeans(s$D)
     , ylim = c(-2, 2)
     , type = "l"
     , xlab = "manipulated A"
     , ylab = "counterfactual D"
     , main = "Total counterfactual effect of A on D"
     )

shade(apply(s$D, 2, PI), sim_dat$A)

plot(sim_dat$A, colMeans(s$M)
     , ylim = c(-2, 2)
     , type = "l"
     , xlab = "manipulated A"
     , ylab = "counterfactual M"
     , main = "Total counterfactual effect of A -> M"
)

shade(apply(s$M, 2, PI), sim_dat$A)

#new data frame, standardized to mean 26.1 and std dev 1.24

sim2_dat <- data.frame(A=(c(20,30)-26.1)/1.24) 

s2 <- sim(m5.3_A, data = sim2_dat, vars = c("M", "D"))

mean(s2$D[,2] - s2$D[,1])

sim_dat <- data.frame(M = seq(from = -2, to = 2, length.out = 30), A = 0)

s <- sim(m5.3_A, data = sim_dat, vars = "D")

plot(sim_dat$M, colMeans(s)
     , ylim = c(-2, 2)
     , type = "l"
     , xlab = "manipulated M"
     , ylab = "counterfactual D"
     , main = "Total counterfactual effect of M on D"
     )

shade(apply(s, 2, PI), sim_dat$M)

###5.1.5.2 Overthinking ------------------------------------------------
N <- 100
x_real <- rnorm(N)
x_spur <- rnorm(N, x_real)
y <- rnorm(N, x_real)
d <- data.frame(y, x_real, x_spur)
pairs(d)

m5.over <- quap(
  alist(
    y ~ dnorm(mu, sigma)
    , mu <-  a + bR*x_real + bS*x_spur
    , a ~ dnorm(0, 0.2)
    , bR ~ dnorm(0, 0.5)
    , bS ~ dnorm(0, 0.5)
    , sigma ~ dexp(1)
  ), data = d
)

precis(m5.over)


###5.1.5.3 Overthinking ------------------------------------------------

A_seq <- seq(from = -2, to = 2, length.out = 30)

post <- extract.samples(m5.3_A)

M_sim <- with(post
              , sapply(1:30
                       , function(i){
                         rnorm(1e3, aM + bAM*A_seq[i], sigma_M)
                       }
                )
              )

D_sim <- with(post
              , sapply(1:30
                       , function(i){
                         rnorm(1e3, a+bA*A_seq[i] + bM*M_sim[,i], sigma)
                       }
                       )
              )

plot(colMeans(D_sim) ~ A_seq)


# 5.2 Masked relationship -------------------------------------------------


