# https://www.youtube.com/watch?v=TuTmC8aOQJE&list=PLUl4u3cNGP63ctJIEC1UnZ0btsphnnoHR&index=5&t=0s
rm(list = ls());

library(ggplot2);
library(dplyr);

set.seed(1234);

# a simple random walk is a sequence of N random variables
# each r.v. may take on a value of -1 or 1 with equal probability
r_v <- function()sample(x = c(-1, 1), size = 1);

random_walk <- function(N) {
  r_walk <- sapply(X = 1:N, FUN = function(i)r_v(), simplify = T)
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
  labs(title = sprintf("Random Walk of %s steps", N),
       subtitle = "Extreme values shown in blue",
       caption = "Christian Bitter");

# this visualization makes it clear, which the final value is 
df_walk %>% 
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_line(size = 1, colour = "gray", alpha = .4) +
  geom_hline(aes(yintercept = mean(y)), colour = "blue", linetype = 2) +
  geom_hline(aes(yintercept = last(y)), colour = "brown", linetype = 2) + 
  theme_light() + 
  labs(title = sprintf("Random Walk of %s steps", N),
       subtitle = "Sample Average shown in blue and final value shown in brown.",
       caption = "Christian Bitter");

# Now, run K random walks of N steps
# and see where we end up ...
K <- 1e3;
i <- 1;
path <- replicate(n = K, 
                  expr = (function() {
                    w <- random_walk(N = N);
                    df_w <- data.frame(x = 1:length(w), y = w);
                    df_w$run <- i;
                    i <<- i + 1;
                    return(df_w);
                  })(), 
                  simplify = F);
df <- path[[1]];
for (j in 2:length(path)) {
  df <- rbind(df, path[[j]])
}

df %>%
  ggplot(aes(x, y)) + 
  geom_point(alpha = .3, size = .5, colour = "black") +
  geom_hline(colour = "red", aes(yintercept = mean(y))) + 
  # geom_line(alpha = .3, colour = "black") + 
  theme_light() + 
  labs(title = sprintf("%s Random Walks of %s steps", K, N),
       subtitle = sprintf("Sample Average (%s) shown in red.", mean(df$y)),
       caption = "Christian Bitter");
