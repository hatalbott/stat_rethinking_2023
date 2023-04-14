# GitHub Homework ---------------------------------------------------------

# # 1 -----------------------------------------------------------------------

#function to compute posterior distribution
compute_posterior <- function(W, L, poss){
  ways <- sapply(poss, function(q) (q)^W * (1-q)^L)
  post <- ways/sum(ways)
  data.frame(poss, ways, post=round(post, 3))
}

compute_posterior(W=4, L=11, poss=seq(from=0, to=1, len=11))

curve( dbeta(x,4+1,11+1) , from=0 , to=1 , xlab="p" )


# # 2 ---------------------------------------------------------------------

#simulate posterior predictive distribution

p_samples <- rbeta(1e4,4+1,11+1)
W_sim <- rbinom(1e4
                ,size=5
                ,p=p_samples
)

plot(table(W_sim))

# # 3 ---------------------------------------------------------------------

ans3 <- sum(W_sim >= 3)/1e4


# # 4 ---------------------------------------------------------------------

compute_posterior_N <- function( W , p , N_max ) {
  ways <- sapply( W:N_max ,
                  function(n) choose(n,W) * p^W * (1-p)^(n-W) )
  post <- ways/sum(ways)
  data.frame( N=W:N_max , ways , post=round(post,3) )
}

compute_posterior_N( W=5 , p=0.7 , N_max=20 )
