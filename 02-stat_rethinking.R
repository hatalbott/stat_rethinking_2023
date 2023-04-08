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




