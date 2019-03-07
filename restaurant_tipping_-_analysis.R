rm(list = ls());

library(ggplot2);
library(ggthemes);
library(dplyr);
library(lmtest);

source("common.R")

df <- tipping_df();

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

df %>% ggplot(aes(y = Yt, x = as.factor(1))) + geom_boxplot() + theme_light() + 
  labs(y = "Tip Amount [€]", title = "Range/ Spread of Tip Amounts", x = "");
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

cat("test the assumptions of the model - correlation of residuals\r\n");
# plot the residuals and residuals vs. attributes
# perform a test for autocorrelated residuals
r <- residuals(lm.21);
f_i <- predict(lm.21);

# the sum of residuals should approach zero
s_r <- sum(r);
cat("The sum of residuals: ", s_r, "\r\n");

# https://en.wikipedia.org/wiki/Residual_sum_of_squares
# total sum of squares = explained sum of squares + residual sum of squares.
# The better the linear regression (on the right) fits the data in comparison to the simple average (on the left graph), 
# the closer the value of R^2 is to 1. 
# https://en.wikipedia.org/wiki/Coefficient_of_determination
alpha  <- 0;
beta   <- lm.21$coefficients[1];
y_mu   <- mean(df$Yt);
x_mu   <- mean(df$Xt);
r_i    <- df$Yt - f_i;
ss_tot <- sum((df$Yt - y_mu)^2);
ss_reg <- sum((f_i - y_mu)^2);
ss_res <- sum((df$Yt - f_i)^2);
C_o_Det <- ss_reg / ss_tot;
cat("Coefficient of Determination (R^2) = ", C_o_Det, 
    " total sum of squares vs. regression sum of squares: ", ss_tot, "/", ss_reg, "\r\n");

df_r <- data.frame(x = df$Xt, y = df$Yt, 
                   yhat = f_i, r_i = r_i, xri = df$Xt + r_i,
                   SS_tot = (df$Yt - y_mu)^2, SS_res = (df$Yt - f_i)^2);
df_r %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(aes(col = "Data")) +
  geom_point(aes(y = yhat, col = "Yhat")) + 
  geom_hline(aes(yintercept = y_mu), col = "gray", linetype = 2) + 
  geom_vline(aes(xintercept = x_mu), col = "gray", linetype = 2) + 
  geom_abline(slope = beta, intercept = alpha, col = "gray") + 
  # geom_point(aes(y = SS_tot, col = "Total Sum of Squares"), size = 1) +
  # geom_point(aes(y = SS_res, col = "Residual Sum of Squares"), size = 1) + 
  theme_light() + 
  labs(x = "x", y = "y", title = "Residual Analysis",
       subtitle = "Comparison of Residual and Total Sum of Squares");

# TODO:
df_r %>%
  ggplot(aes(x = x, y = y)) + 
  geom_point(aes(col = "Data")) +
  geom_point(aes(y = yhat, col = "Yhat"), size = 1) + 
  geom_rect(aes(xmin = x, ymin = y, xmax = x + (y - yhat)^2, ymax = yhat)) + 
  geom_abline(slope = beta, intercept = alpha, col = "gray") + 
  # geom_point(aes(y = SS_tot, col = "Total Sum of Squares"), size = 1) +
  # geom_point(aes(y = SS_res, col = "Residual Sum of Squares"), size = 1) + 
  theme_light() + 
  labs(x = "x", y = "y", title = "Residual Analysis",
       subtitle = "Comparison of Residual and Total Sum of Squares");


# in order to check for association we regress residuals - the simplest approach
# and now can check for the slope coefficient's signifance
# we could also aim for the breusch godfrey test or the durbin-watson test
# https://en.wikipedia.org/wiki/Breusch%E2%80%93Godfrey_test
n <- length(r) 
lm_r_check <- lm(r[-n] ~ r[-1]) 
summary(lm_r_check);
# we see that the estimate for the slope is not significant, meaning, that 
# the data does not support the assumption of a non-zero slope
# meaning that there is no association between the residuals
# meaning that they are not correlated.
cat("Residuals are correlated (Breusch Godfrey Test) - p = 0.05: ", (lmtest::bgtest(lm.21)$p.value < .05), "\r\n");
cat("Residuals are correlated (Durbin Watson Test) - p = 0.05: ", (lmtest::dwtest(lm.21)$p.value < .05), "\r\n");

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

# test the assumptions of the model - homogenity of variance
# plot and perform a test of variance
df %>% ggplot(aes(x = yhat, y = residuals)) + geom_point() + 
  labs(x = "Predicted Tip Amount [€]",
       y = "Pay Amount [€]");


# test the assumptions of the model - normally distributed residuals
# the last important - plot qqnorm and perform test for normality of residuals
df %>% ggplot(aes(sample = residuals)) + stat_qq() + stat_qq_line() + 
  labs(title = "QQPlot of Residuals",
       subtitle = "Ideally all of our residuals lay on the midpoint line.");

# now perform some inference on some artificially created meal prices
# let's assume there was someone who paid 1, 5 euro and 70, 100 euro for our meal and see what
# what happens
test_df <- data.frame(Xt = c(1, 5, 70, 100));
test_df$Yt <- predict(lm.21, test_df);
df %>%
  ggplot(aes(x = Xt, y = Yt)) + 
  geom_point(colour = "red") + 
  geom_point(data = test_df, aes(x = Xt, y = Yt), col = "blue") + 
  labs(title = "Restaurant Tips",
       subtitle = "Exploring actual spent and tips (red) and simulated (blue)",
       x = "Pay Amout [€]", y = "Tip Amount [€]",
       caption = "Christian Bitter") + 
  theme_light()
