library(duckdb)
library(duckplyr)

d <- expand.grid(
  A = c("a1", "a2"),
  B = c("b1", "b2", "b3"),
  C = c("c1", "c2"),
  stringsAsFactors = FALSE
)

a_s <- data.frame(
  A = c("a1", "a2", "a1", "a2"),
  C = c("c1", "c1", "c2", "c2"),
  value = c(.20, .80, .10, .90)
)

b_s <- data.frame(
  B = c("b1", "b2", "b3", "b1", "b2", "b3"),
  C = c("c1", "c1", "c1", "c2", "c2", "c2"),
  value = c(.5, .3, .2, .05, .05, .9)
)


d$value <- runif(nrow(d))
d$value <- 200 * d$value / sum(d$value)

d_org <- d
s <- 100

a_s <- a_s |> mutate(value = value/sum(value) * s, .by = C)
b_s <- b_s |> mutate(value = value/sum(value) * s, .by = C)

d <- d |>
  left_join(a_s |> select(A, C, A_t = value), by = c("C", "A")) |>
  left_join(b_s |> select(B, C, B_t = value), by = c("C", "B"))

library(dplyr)
iter <- 10
eps <- 1e-8

for (i in seq_len(iter)){
  d <- d |>
    mutate(value = value/sum(value, na.rm = TRUE) * A_t, .by = c(C,A)) |>
    mutate(value = value/sum(value, na.rm = TRUE) * B_t, .by = c(C,B))

  diff <- d |> summarise(diff = sum(value, na.rm = TRUE) - first(A_t), .by = c(C,A))
  max_diff <- max(abs(diff$diff))
  message("max_diff: ", max_diff)
  if (max(abs(diff$diff)) < s*eps){
    message("Converged")
    break
  }
}

d |> summarise(value = sum(value), .by = c(C,A))
d |> summarise(value = sum(value), .by = c(C,B))

