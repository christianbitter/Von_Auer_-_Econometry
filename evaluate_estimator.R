# this topic is concerned with taking a random simulation
# and demonstrating bias and efficiency in an estimator
# we generate a random sample of some normals
# we create the following types of estimators

rm(list = ls());
library(ggplot2);
library(dplyr);
library(gridExtra)

estimator_mean <- function(x){
  return(mean(x));
}

estimator_constant <- function(x){
  return(x[1]);
}

# so let's get a bunch of random normals
mu <- 10;
sd <- 1;
N  <- 1e4;

x <- rnorm(n = N, mean = mu, sd = sd);

df <- data.frame(x = x);

df %>%
  ggplot(aes(x = x)) + 
  geom_histogram(aes(alpha = .1)) + 
  geom_vline(aes(xintercept = mu), colour = "red") + 
  labs(title = "Evaluating Estimators", 
       subtitle = sprintf("%s random normals with mean %s and sd %s", N, mu, sd),
       caption = "Christian Bitter") + 
  theme_light()


# we know the true mean for comparison, 
# but we also know that the emperical mean is the best estimator of the true mean mu
# so let's create M samples, estimate and see how our estimator stacks against the
# true mean
M <- 1e4;
L <- N;
estimates <- replicate(n = M,
                       expr = (function() {
                         s <- sample(x = x,
                                     size = L,
                                     replace = T);
                         mu_hat <- estimator_mean(s);
                         e_hat  <- estimator_constant(s);
                         return(c(mu_hat, e_hat));
                       })(),
                       simplify = T);

m <- matrix(estimates, byrow = T, ncol = 2);
df_e <- data.frame(x = m[, 1], type = rep("Mean Estimator", M));
df_e <- rbind(df_e, 
              data.frame(x = m[, 2], type = rep("Fixed Estimator", M)));
df_e$type <- as.factor(df_e$type);

df_e_mu <- apply(m, mean, MARGIN = 2);

# instead of faceting, we plot the two estimators separately, so that
# the shape of their distribution becomes apparent.
p1 <-
  df_e[df_e$type == "Mean Estimator", ] %>%
    ggplot(aes(x = x)) + 
    geom_histogram(aes(x = x), alpha = 0.3) + 
    geom_vline(aes(xintercept = mu, colour = "true mean")) + 
    geom_vline(aes(xintercept = df_e_mu[1], colour = "empirical mean estimate")) + 
    geom_vline(aes(xintercept = df_e_mu[2], colour = "fixed estimate")) + 
    labs(title = "Evaluating Estimators", 
         subtitle = sprintf("Distribution of empirical mean"),
         caption = "Christian Bitter") + 
    theme_light();
# notice how concentrated the mean estimator is (see the range of the histogram)
# whereas the fixed estimators has huge spread
p2 <- 
  df_e[df_e$type == "Fixed Estimator", ] %>%
    ggplot(aes(x = x)) + 
    geom_histogram(aes(x = x), alpha = 0.3) + 
    geom_vline(aes(xintercept = mu, colour = "true mean")) + 
    geom_vline(aes(xintercept = df_e_mu[1], colour = "empirical mean estimate")) + 
    geom_vline(aes(xintercept = df_e_mu[2], colour = "fixed estimate")) + 
    xlim(7, 13) + 
    labs(title = "Evaluating Estimators", 
         subtitle = sprintf("Distribution of fixed estimate"),
         caption = "Christian Bitter") + 
    theme_light();

plot(gridExtra::arrangeGrob(p1, p2));
