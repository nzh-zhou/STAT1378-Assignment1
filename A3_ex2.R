## Exercise 2
library(tidyverse)
# Part 0
set.seed(2048)

# Part 1
X <- map(1:100, ~ list(name = glue::glue("n{.x}"),
                       vect = rnorm(5, mean = 23, sd = 5)))

# Part 2
str(X)

# Part 3
map_dbl(1:100, ~ sum(X[[.x]]$vect))

# Part 4
Y <- vector("list", 100) #create list with 100 empty elements
for (i in 1:100) {
  Y[[i]] <- X[[i]]$vect #extract each `vect` in `X` as elements in `Y` for a 2D list
}
M <- matrix(unlist(Y), byrow = TRUE, ncol = 5) #create matrix `M` using `Y`

# Part 5
sums_M <- double(5) #create a numeric vector with 5 zero elements
for (i in 1:5) {
  sums_M[i] <- M[, i] %>% sum() #sum each column and assign it to an element in sums_M
}
sums_M
