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

cat("The histogram shows that we have different realizations of our estimated parameter.\r\n")
cat("These realizations depend on the sample of observations we have used to estimate our parameter.\r\n")
cat("As we can see the different estimates are quite similar and very close to about 0.139.\r\n")
hist(vbeta, main = "Histogram of model beta")
abline(v = mean(vbeta), col = "red");


# in order to understand better that indeed our tips are a random variable two,
# we are going to simulate independent samples/ realizations
# however we are not going to rely on the given data but generate it ourself
n   <- 50;
# let's pick 50 guests spending between 5 and 20 euro for a meal
X_t <- c(11,8,14,9,8,19,9,13,7,14,15,20,12,13,11,18,7,18,18,11,8,9,12,17,15,18,11,11,17,17,19,5,8,6,8,18,5,12,20,10,11,19,7,7,19,7,7,11,5,19);
# let's assume each guest tips according to the following model
# Y_t = alpha + beta * X + noise
alpha <- 0;
beta <- .1;
Y_t <- alpha + beta * X_t + rnorm(n = n);
df <- data.frame(x = X_t, y = Y_t);
df %>% 
  ggplot(aes(x = x, y = y)) + geom_point() + 
  stat_smooth(method="lm") + 
  theme_light()
# what we see is, if we run this procedure again, we will get a slightly different
# plot, because our tips depend on the random variable u
# so let's put this into a function
get_tip <- function(X_t = c(11,8,14,9,8,19,9,13,7,14,15,20,12,13,11,18,7,18,18,11,8,9,12,17,15,18,11,11,17,17,19,5,8,6,8,18,5,12,20,10,11,19,7,7,19,7,7,11,5,19)) {
  n   <- length(X_t);
  if (n < 1) stop("get_tip - number of meal spent cannot be less than 1")
  # let's pick n guests spending between 5 and 20 euro for a meal
  # let's assume each guest tips according to the following model
  # Y_t = alpha + beta * X + noise
  alpha <- 0;
  beta <- .1;
  u <- rnorm(n = n);
  Y_t <- alpha + beta * X_t + u;
  df <- data.frame(x = X_t, y = Y_t);
  # let's build a model, with explicit intercept 
  m <- lm(Y_t ~ 1 + X_t, df);
  return(coef(m));
}

# now replicate the study 1000 times
M <- 1e2;
replicated <- replicate(n = M, expr = get_tip(), simplify = T); 
v_coefs <- matrix(data = replicated, byrow = T, nrow = M);
df <- data.frame(v_coefs);
names(df) <- c("alpha", "beta");

plot(0,0);
for (j in 1:nrow(df)) {
  abline(a = df$alpha[j], b=df$beta[j], col = "lightblue");
}
# add the true model
abline(a = 0, b = .1, col="red");
title(main="Simulating Noise in the tipping process",
      sub ="Different models due to random U (red is true model)");
