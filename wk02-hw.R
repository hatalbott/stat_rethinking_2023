
# Online Homework ---------------------------------------------------------

library(rethinking)

## 1 -----------------------------------------------------------------------

data("Howell1")

g <- Howell1[Howell1$age<13,]

sim_WA <- function(A, b, a) {
  N <- length(A)
  H <- A
  W <- a[A] + b[A]*H + rnorm(N, 0, 5)
  data.frame(A, H, W)
}