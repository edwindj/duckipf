library(duckdb)
library(duckplyr)

d <- expand.grid(A = c("a1", "a2"), B = c("b1", "b2", "b3"))

a_s <- data.frame(
  A = c("a1", "a2"),
  value = c(.25, .75)
)

b_s <- data.frame(
  B = c("b1", "b2", "b3"),
  value = c(.5, .3, .2)
)


d$value <- runif(nrow(d))
d$value <- 100 * d$value / sum(d$value)

setDT(d)
setDT(a_s)
setDT(b_s)

# add to the data
d$A_t <- a_s[d$A,value, on="A"]
d$B_t <- b_s[d$B,value, on="B"]

d1 <- d
max_iter <- 10
eps <- 1e-5

for (i in seq_len(max_iter)){
  d1[, value := value/sum(value) * A_t, by = .(A)]
  d1[, value := value/sum(value) * B_t, by = .(B)]
  diff <- d1[, .(diff = sum(value) - A_t[1]), by = .(A)]
  max_diff <- max(abs(diff$diff))
  message("max_diff: ", max_diff)
  if (max_diff < eps){
    message("Converged")
    break
  }
}
