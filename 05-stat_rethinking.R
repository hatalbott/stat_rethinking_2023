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


## 5.14 Approximating the posterior ----------------------------------------

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

### THIS PLOT IS NOT WORKING ###
x <- coeftab(m5.1, m5.2, m5.3)
x2 <- data.frame(x@coefs)

plot(x@coefs
     # , par = c("bA", "bM")
     )

plot(x2, par = c("bA", "bM"))
