#
# In this notebook we are going to explore a bunch of different distributions that
# naturally arise when doing estimations
#
rm(list = ls())

library(dplyr);
library(ggplot2);

source("common.R");

M <- 1e4;
# Normal distribution
# let's generate random numbers according to a normal distribution
mu <- 5;
s  <- 1;
x <- rnorm(n = M, mean = mu, sd = s);

df <- data.frame(x = x);

df %>% 
  ggplot(aes(x = x)) + geom_histogram(fill = "lightgray") + 
  geom_vline(aes(xintercept = mean(x)), col = "blue") +
  theme_light() + 
  labs(title = "Histogram of 10000 i.i.d. x ~ N(5, 1)", x = "x");

# Z
# the standard normal arises when we standardize a normal distribution
# through removing the mean and dividing by s
z <- (x - mu) / s;
df <- cbind(df, z = z);
df %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(fill = "gray", alpha = .5) + 
  geom_vline(aes(xintercept = mean(x)), col = "blue") +
  geom_histogram(fill = "blue", alpha = .5, aes(x = z)) + 
  geom_vline(aes(xintercept = mean(z)), col = "blue") +
  theme_light() + 
  labs(title = "Histogram of 10000 i.i.d. x ~ N(5, 1), z ~ N(0, 1)", x = "x");

# we can always recover the normal from the standardized r.v. by  multiplying with s
# and adding the mean component
zx <- z * s + mu;
df <- cbind(df, zx = zx);
df %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(fill = "gray", alpha = .8) + 
  geom_vline(aes(xintercept = mean(x)), col = "blue") +
  geom_histogram(fill = "blue", alpha = .5, aes(x = z)) + 
  geom_vline(aes(xintercept = mean(z)), col = "blue") +
  geom_histogram(col = "green", fill = "green", alpha = .2, aes(x = zx)) + 
  geom_vline(aes(xintercept = mean(z)), col = "blue") +
  theme_light() + 
  labs(title = "Histogram of 10000 i.i.d. x ~ N(5, 1)", x = "x");

# X2
# the x2 distribution with v degrees of freedom arises when we add v squared standard normal r.v
degree_of_freedom <- 10;
chi_2 <- rep(0, M);
for (i in 1:degree_of_freedom) {
  z_i <- rnorm(n = M, mean = 0, sd = 1);
  z_i <- z_i^2;
  chi_2 <- chi_2 + z_i;
}
df <- cbind(df, chi2 = chi_2);
# let's compare against R's builtin 
df <- cbind(df, chi2r = rchisq(n = M, df = degree_of_freedom));
# we see that the means almost perfectly overlap, but that there is some some small
# difference likely due to randomness and maybe accuracy
# we could make a replication of this and take the average behaviour and compare
# but for now we are good
df %>% 
  ggplot(aes(x = chi2)) + 
  geom_histogram(fill = "gray", alpha = .8) + 
  geom_vline(aes(xintercept = mean(chi2)), col = "blue") + 
  geom_histogram(aes(x = chi2r), fill = "blue", alpha = .3) + 
  geom_vline(aes(xintercept = mean(chi2r)), col = "red") + 
  theme_light() + 
  labs(title = "Chi2(10)", 
       subtitle = "Comparing 1000 generated Chi2(10) against R's built-in function", x = "x");

# T
# given a Chi2 distribution with k degree of freedom c and an independent standard normal z
# the t distributed random variable l arises as z / sqrt(c / v)
c <- chi_2;
v <- degree_of_freedom;
z <- rnorm(n = M, mean = 0, sd = 1);
l <- z / sqrt(c / v);

df <- cbind(df, t = l);
# let's compare against R's builtin 
df <- cbind(df, tr = rt(n = M, df = v));
# as above in the chi2 case, both distributions behave almost indicentically
# also, we we see the t distribution as a standard normal only with wider/ fatter tails
df %>% 
  ggplot(aes(x = t)) + 
  geom_histogram(fill = "gray", alpha = .8) + 
  geom_vline(aes(xintercept = mean(t)), col = "blue") + 
  geom_histogram(aes(x = tr), fill = "green", alpha = .3) + 
  geom_vline(aes(xintercept = mean(tr)), col = "red") + 
  theme_light() + 
  labs(title = "t(10)", 
       subtitle = "Comparing 1000 generated t(10) against R's built-in function", x = "x");

# F
# the F distribution arises as the quotient of two independently distributed chi2 
# distributions and their respective degrees of freedom
dof_1 <- 10;
dof_2 <- 15;
c1    <- sim_chi2(degree_of_freedom = dof_1, M = M);
c2    <- sim_chi2(degree_of_freedom = dof_2, M = M);
f <- (c1/dof_1) / (c2/dof_2);


df <- cbind(df, f = f);
# let's compare against R's builtin 
df <- cbind(df, fr = rf(n = M, df1 = dof_1, df2 = dof_2));
# as above in the chi2 case, both distributions behave almost indicentically
df %>% 
  ggplot(aes(x = f)) + 
  geom_histogram(fill = "gray", alpha = .8) + 
  geom_vline(aes(xintercept = mean(f)), col = "blue") + 
  geom_histogram(aes(x = fr), fill = "green", alpha = .3) + 
  geom_vline(aes(xintercept = mean(fr)), col = "red") + 
  theme_light() + 
  labs(title = "t(10)", 
       subtitle = "Comparing 1000 generated f(10, 15) against R's built-in function", x = "x");
