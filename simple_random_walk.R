# https://www.youtube.com/watch?v=TuTmC8aOQJE&list=PLUl4u3cNGP63ctJIEC1UnZ0btsphnnoHR&index=5&t=0s
rm(list = ls())

library(ggplot2);
library(dplyr);

source("common.R");

# TODO: we need to correct ... so that 100 steps are from 1 to 100
# this involves correcting the cumulative sum as all other variables depend on it

set.seed(1234);

# a simple random walk is a sequence of N random variables
# each r.v. may take on a value of -1 or 1 with equal probability
r_v <- function()
  sample(x = c(-1, 1), size = 1)


random_walk <- function(N) {
  r_walk <- sapply(
    X = 1:N,
    FUN = function(i)
      r_v(),
    simplify = T
  )
  rc_walk <- c(0, cumsum(r_walk))
  return(rc_walk);
  
}


N <- 100;
rc_walk <- random_walk(N = N);
df_walk <- data.frame(x = 1:length(rc_walk), y = rc_walk);


# this visualization makes it clearer, in which direction the path goes
df_walk %>%
  ggplot(aes(x = y, y = x)) +
  geom_path(linetype = 4) + geom_point() +
  geom_vline(xintercept = range(df_walk$y), colour = "blue") +
  theme_light() +
  labs(
    title = sprintf("Random Walk of %s steps", N),
    subtitle = "Extreme values shown in blue",
    caption = "Christian Bitter"
  )


# this visualization makes it clear, which the final value is
df_walk %>%
  ggplot(aes(x, y)) +
  geom_point() +
  geom_line(size = 1,
            colour = "gray",
            alpha = .4) +
  geom_hline(aes(yintercept = mean(y)),
             colour = "blue",
             linetype = 2) +
  geom_hline(aes(yintercept = last(y)),
             colour = "brown",
             linetype = 2) +
  theme_light() +
  labs(
    title = sprintf("Random Walk of %s steps", N),
    subtitle = "Sample Average shown in blue and final value shown in brown.",
    caption = "Christian Bitter"
  )


# Now, run K random walks of N steps
# and see where we end up ...
walkouts <- function(k, n = N) {
  if (k <= 0) stop("k cannot be < 1");
  if (n <= 0) stop("n cannot be < 1");
  
  i <- 1;
  path <- replicate(n = k,
                    expr = (function() {
                      w <- random_walk(N = n);
                      df_w <- data.frame(x = 0:n, y = w);
                      df_w$run <- i;
                      
                      i <<- i + 1;
                      return(df_w);
                    })(),
                    simplify = F)
  df <- path[[1]];
  l <- length(path);
  if (l > 1) {
    for (j in 2:length(path)) {
      df <- rbind(df, path[[j]])
    }
  }
  return(df);
}

K <- 1e3;
df <- walkouts(k = K);

df_p <-
  df %>% dplyr::group_by(x) %>% summarize(y = mean(y),
                                          run = round(median(run)))

# if we vary the number of random walks, our estimates get more or less noise
# i.e. a 10 random walk simulation will show that the mean per step varies a lot more
# (when compared to the overall range of the data) than a 100 or 1000 random walk simulation, 
# in that case, we can see that the overall mean is close to zero and that the step-wise mean,
# i.e. the mean per time-point across the different runs is 0 as well
df %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .3,
             size = .5,
             colour = "black") +
  geom_hline(colour = "blue", alpha = .3, aes(yintercept = mean(y))) +
  geom_line(data = df_p, alpha = .7, colour = "red", aes(x = x, y = y)) +
  theme_light() +
  labs(x = "Step", y = "Cumulative sum of y",
    title = sprintf("%s Random Walks of %s steps", K, N),
    subtitle = sprintf("Time-Average shown in red and overall mean (%s) in blue.", round(mean(df$y), 3)),
    caption = "Christian Bitter"
  ); # let's encapsulate this as a function in common for later reuse

# let's look at the histogram at the final state, which is a sum of random variables
# which by the weak law of large numbers, we know to follow a normal distribution
df %>% 
  dplyr::filter(x == N) %>%
  ggplot(aes(x = y)) + 
  geom_histogram(binwidth = 1) + 
  theme_light() +
  labs(x = "Cumulative sum of y",
       title = "Random Walk - Histogram",
       subtitle = sprintf("Histogram of final value of %s random Walks of %s steps", K, N),
       caption = "Christian Bitter"
  );

# now ask the question of what is the probability of reaching a certain value
# let's assume we have a random walk of length 100
df <- walkouts(k = 1, n = 100);
upper_bound <- 10;
max_bound <- 100;

p <- 
  plot_random_walks(df) + 
  geom_hline(aes(yintercept=upper_bound), colour = "yellow") + 
  geom_hline(aes(yintercept=max_bound), colour = "brown");

plot(p);
