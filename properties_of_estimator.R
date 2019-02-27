#
#In this example, we are going to explore, the properties of our estimators
#
rm(list = ls());

library(ggplot2);
library(ggthemes);
library(dplyr);
library(lmtest);

set.seed(1234);

df <- read.csv("restaurant_tipping.csv", header = T, sep = ";", dec = ",");

cat("In the first example, we are going to look at how our model estimates vary with different samples that we take\r\n");
N <- 1e4;
cat("Running ", N, "simulations\r\n")
vbeta <- replicate(n = N, expr = (function(i) {
  s_i <- sample(1:nrow(df), size = nrow(df), replace = T);
  df <- df[s_i, ];
  lm_i <- lm(Yt ~ Xt - 1, df);
  xt_i <- coef(lm_i);
  return(xt_i);
  })(), simplify = T);

hist(vbeta, main = "Histogram of model beta")
abline(v = mean(vbeta), col = "red");
