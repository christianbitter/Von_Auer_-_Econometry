# https://www.youtube.com/watch?v=f9XFM8YLccg&t=3009s
rm(list = ls());

library(dplyr);
library(ggplot2);

source("./common.R");

set.seed(1234);

# the weak law of large numbers states that the some of i.i.d. random variables
# follows a normal distribution. So let us see this result for two different
# type of r.v. and then even their sum.

# so let us define two random variables U and V that follow a uniform and
# poisson distribution respectively.
U <- function(n) runif(n = n);
V <- function(n) rpois(n = n, lambda = .5);

# let us generate N samples from each
N <- 1e3;

uv <- data.frame(x = 1:N, u = U(N), v = V(N));

p <- uv %>% ggplot() + labs(caption = "(c) 2019 Christian Bitter");
p + geom_point(aes(x = x, y = u));
p + geom_histogram(aes(x = u));

p + geom_point(aes(x = x, y = v)) + labs(caption = "(c) 2019 Christian Bitter");
p + geom_histogram(aes(x = v));

# now let's - replicate the following for K times, we generate N r.v. and sum them
K <- 1e4;
u_x <- replicate(n = K,
               expr = (function(){
                 u_i <- sum(U(N));
               })(),
               simplify = T);
v_x <- replicate(n = K,
                 expr = (function(){
                   v_i <- sum(V(N));
                 })(),
                 simplify = T);

# let's look at the result
wl_uv_x <- data.frame(x = 1:K, y = u_x, z = v_x);
p <- wl_uv_x %>% ggplot() + labs(caption = "(c) 2019 Christian Bitter");
p + geom_histogram(aes(x = y));
p + geom_histogram(aes(x = z));
