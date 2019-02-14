rm(list = ls());

library(ggplot2);
library(ggthemes);
library(dplyr);

df <- read.csv("restaurant_tipping.csv", header = T, sep = ";", dec = ",");

# let's graphically explore the data

df %>% 
  ggplot(aes(x = Xt, y = Yt)) + 
  geom_point() + 
  labs(x = "Pay Amount [€]", y = "Tip Amount [€]",
       title = "Tipping vs. Bill Size",
       subtitle = "Does restaurant tipping depend on the size of the bill?") + 
  stat_smooth(method = "lm") +
  theme_light();

df %>% 
  ggplot(aes(x = t, y = Xt, size = Yt)) + 
  geom_point() + 
  labs(x = "Customer", y = "Pay Amount [€]",
       title = "Customer Tipping",
       subtitle = "Do we see individual tipping strategies") + 
  theme_light();

df$tip_per_unit <- df$Yt / df$Xt;

df %>% ggplot(aes(y = tip_per_unit)) + geom_boxplot() + theme_light();

df %>% 
  ggplot(aes(x = Xt, y = tip_per_unit)) + 
  geom_point() + 
  labs(x = "Pay Amount [€]", y = "Tip Per Unit Bill [€]",
       title = "Tipping vs. Bill Size",
       subtitle = "Is it more lucrative to go for tips with smaller or larger bills?") + 
  stat_smooth(method = "lm") +
  theme_light();

# intercept only model
lm.1 <- lm(Yt ~ 1, df);
summary(lm.1);

# full model
lm.2 <- lm(Yt ~ Xt + 1, df);
summary(lm.2);
# intercept is not significant anymore

# slope only model
lm.21 <- lm(Yt ~ Xt - 1, df);
summary(lm.21);

# test the assumptions of the model - correlation of residuals
# plot the residuals and residuals vs. attributes
# perform a test for autocorrelated residuals
r <- residuals(lm.21);
yhat <- predict(lm.21);

df$residuals <- r;
df %>% ggplot(aes(x = Xt, y = residuals)) + geom_point() + 
  labs(x = "Pay Amount [€]",
       y = "Residuals", title = "Residual vs. Predictor plot");
df %>% ggplot(aes(x = Yt, y = residuals)) + geom_point() + 
  labs(x = "Tip Amount [€]",
       y = "Residuals", title = "Residual vs. Predictor plot");
df %>% ggplot(aes(x = yhat, y = residuals)) + geom_point() + 
  labs(x = "Predicted Tip Amount [€]",
       y = "Residuals", title = "Residual vs. Predictor plot");

# test the assumptions of the model - heteroscedasticity
# plot and perform a test of variance
df %>% ggplot(aes(x = yhat, y = residuals)) + geom_point() + 
  labs(x = "Predicted Tip Amount [€]",
       y = "Pay Amount [€]");


# test the assumptions of the model - normally distributed residuals
# the last important - plot qqnorm and perform test for normality of residuals
df %>% ggplot(aes(sample = residuals)) + stat_qq() + stat_qq_line() + 
  labs(title = "QQPlot of Residuals",
       subtitle = "Ideally all of our residuals lay on the midpoint line.");

# now perform some inference on some artificially created tips

# now perform a predictive modeling, splitting into train/test or some k-fold sample
# based approach.
