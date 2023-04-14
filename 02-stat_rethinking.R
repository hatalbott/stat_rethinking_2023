library(rethinking)

sample <- c("W", "L", "W", "W", "W", "L","W", "L", "W")

# number of W observed
W <- sum(sample == "W")

# number of L observed
L <- sum(sample == "L")

# proportions W
poss <-  c(0, .25, .5, .75, 1)

ways <- sapply(poss, function(q) (q*4)^W * ((1-q)*4)^L)

post <- ways/sum(ways)

cbind(p, ways, post)


#function to toss a globe cover p by water N times
sim_globe <- function(p = 0.7, N = 9) {
                      sample(c("W", "L")
                             , size = N
                             , prob = c(p, 1-p)
                             , replace = TRUE
                             )
}
sim_globe()
sim_globe(p=1, N=11)
sim_globe(p=0, N=11)
sum(sim_globe(p=.5, N=1e4)=="W")/1e4


#function to compute posterior distribution
compute_posterior <- function(the_sample, poss=c(0, .25, .5, .75, 1)){
  # number of W observed
  W <- sum(the_sample == "W")
  
  # number of L observed
  L <- sum(the_sample == "L")
  ways <- sapply(poss, function(q) (q*4)^W * ((1-q)*4)^L)
  post <- ways/sum(ways)
  data.frame(poss, ways, post=round(post, 3))
}

compute_posterior(sim_globe())
compute_posterior(sim_globe(p=0.7, N=1))

compute_posterior(sim_globe(p=0.5, N=1e2))



# Part 2 ------------------------------------------------------------------

post_samples <- rbeta(1e3, 6+1, 3+1)

dens(post_samples, lwd=4, col=2, xlab="proportion water", adj=0.1)
graphics::hist(post_samples) #not quite right
curve(dbeta(x, 6+1, 3+1), add=TRUE, lty=2, lwd=3)

#simulate posterior predictive distribution
post_samples <- rbeta(1e3, 6+1, 3+1)
pred_post <- sapply(post_samples
                    , function(p) sum(sim_globe(p,10)=="W")
)

tab_post <- table(pred_post) 
for (i in 0:10) lines(c(i,i), c(0, tab_post[i+1]), lwd=4, col=4)


# Book Homework ----------------------------------------------------------------

## 2M1 ---------------------------------------------------------------------

#define grid
p_grid <- seq(from=0, to=1, length.out=20)

#define prior
prior <- rep(1, 20)

#compute likelihood at each value in grid
likelihood <- dbinom(6, size=9, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

ex <- plot(
  p_grid
  , posterior
  , type="b"
  , xlab= "probability of water"
  , ylab = "posterior probability"
)

mtext("20 points")

## 1
#compute likelihood at each value in grid
likelihood <- dbinom(3, size=3, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

x1 <- plot(
  p_grid
  , posterior
  , type="b"
  , xlab= "probability of water"
  , ylab = "posterior probability"
)

mtext("20 points")

## 2
#compute likelihood at each value in grid
likelihood <- dbinom(3, size=4, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

x2 <- plot(
  p_grid
  , posterior
  , type="b"
  , xlab= "probability of water"
  , ylab = "posterior probability"
)

mtext("20 points")

## 3
#compute likelihood at each value in grid
likelihood <- dbinom(5, size=7, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

x2 <- plot(
  p_grid
  , posterior
  , type="b"
  , xlab= "probability of water"
  , ylab = "posterior probability"
)

mtext("20 points")


## 2M2 ---------------------------------------------------------------------

#define prior
prior2 <- ifelse(p_grid < 0.5, 0, 1)


## 1
#compute likelihood at each value in grid
likelihood <- dbinom(3, size=3, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior2

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

x2M2.1 <- plot(
  p_grid
  , posterior
  , type="b"
  , xlab= "probability of water"
  , ylab = "posterior probability"
)

mtext("20 points")

## 2
#compute likelihood at each value in grid
likelihood <- dbinom(3, size=4, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior2

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

x2M2.2 <- plot(
  p_grid
  , posterior
  , type="b"
  , xlab= "probability of water"
  , ylab = "posterior probability"
)

mtext("20 points")

## 3
#compute likelihood at each value in grid
likelihood <- dbinom(5, size=7, prob=p_grid)

#compute product of likelihood and prior
unstd.posterior <- likelihood * prior2

#standardize the posterior, so it sums to 1
posterior <- unstd.posterior/sum(unstd.posterior)

x2M2.2 <- plot(
  p_grid
  , posterior
  , type="b"
  , xlab= "probability of water"
  , ylab = "posterior probability"
)

mtext("20 points")


# # 2M3 -------------------------------------------------------------------

# Pr(land|Earth) = 1 − 0.7 = 0.3
# Pr(land|Mars) = 1
# 
# Pr(Earth) = 0.5
# Pr(Mars) = 0.5
# 
# Pr(Earth|land) = (Pr(land|Earth) Pr(Earth)) / Pr(land)

an2M3 <- (0.3*0.5) / (0.3*0.5 + 1*0.5)


# # 2H1 -------------------------------------------------------------------

# Pr(twin|pandaA) = 0.1
# Pr(twin|pandaB) = 0.2
# 
# Pr(pandaA) = 0.5
# Pr(panda2) = 0.5
# 
# Pr(twin) "unconditioned" probability
tw <- (0.1*0.5 + 0.2*0.5)

# Pr(twins1,  twins2) the joint probability that both births are twins
# Pr(twins1,  twins2|pandaA)
dbtw_pdA <- 0.1*0.1

# Pr(twins1,  twins2|panda2)
dbtw_pdB <- 0.2*0.2

#Average over species
dbtw <- 0.5*(dbtw_pdA) + 0.5*(dbtw_pdB)

# Pr(twins2|twins1)
an2H1 <- dbtw/tw


# # 2H2 -------------------------------------------------------------------

# Pr(pandaA|twin) = (Pr(twin|pandaA) Pr(pandaA)) / Pr(twin)

pdA_tw <- (0.1*0.5) / (0.1*0.5 + 0.2*0.5)

# Pr(panda2|twin) = (Pr(twin|panda2) Pr(panda2)) / Pr(twin)
pdB_tw <- (0.2*0.5) / (0.1*0.5 + 0.2*0.5)


# # 2H3 -------------------------------------------------------------------

# Pr(pandaA|twin1, single2) = (Pr(twin1, single2|pandaA) * Pr(pandaA)) / Pr(twin1, single2)

# Pr(pandaA)
pdA <- 0.5

# Pr(twin1, single2|pandaA)
TS_pdA <- 0.1*0.9

# Pr(twin1, single2|panda)
TS_pdB <- 0.2*0.8

# Pr(twin1, single2)
#Average over species
TS <- 0.5*(TS_pdA) + 0.5*(TS_pdB)

#Combine it all
an2H3 <- (pdA * TS_pdA)/TS


#Easier you can use posterior probability from previous problem and use them as prior probabilites in this problem

# Pr(pandaA|single) = Pr(singleton|A) * Pr(A) / Pr(singleton)

# Pr(pandaA|twin) = (Pr(twin|pandaA) Pr(pandaA)) / Pr(twin) NEW PRIOR

pdA_tw <- (0.1*0.5) / (0.1*0.5 + 0.2*0.5)

# Pr(singleton|A)
S_pdA <- 1-0.1 #(Pr|twin)

# Pr(singleton|B)
S_pdB <- 1-0.2 #(Pr|twin)

# Pr(singleton) = Pr(singleton|A) Pr(A) + Pr(singleton|B) Pr(B) TAKE INTO ACCOUNT PRIOR
S <- S_pdA*pdA_tw + S_pdB*(1-pdA_tw)

# Pr(pandaA|single) = Pr(singleton|A) * Pr(A) / Pr(singleton)
an2H3 <- (S_pdA * pdA_tw) / S


# # 2H4 -------------------------------------------------------------------

#With no additional information
# Pr(pandaA|testA) = Pr(testA|A)*Pr(A) / Pr(testA)

# Pr(testA) = Pr(testA|pandaA + testA|pandaB)
testA <- 0.8*.5 + (1-0.65)*.5

an2H4a <- (0.8*0.5)/testA

#With birth information, easy way, just use previously calculated probability as the prior
# Pr(testA) = Pr(testA|pandaA + testA|pandaB)
testA_birth <- 0.8*an2H3 + (1-0.65)*(1-an2H3)

an2H4b <- (0.8*an2H3)/testA_birth

