tipping_df <- function(){
  return(
    read.csv("data/restaurant_tipping.csv", header = T, sep = ";", dec = ",")
  );
}

theme_df <- function() {
  return( 
    ggplot2::theme(panel.background = element_rect(fill = NA),
                   panel.grid.major = element_line(colour = "grey50"),
                   panel.ontop = TRUE,
                   plot.caption = element_text(hjust = 0))
  );
}

sim_chi2 <- function(degree_of_freedom = 1, M = 10) {
  if (degree_of_freedom < 1) stop("Degree of Freedom < 1", call. = T);
  
  chi_2 <- rep(0, M);
  for (i in 1:degree_of_freedom) {
    z_i <- rnorm(n = M, mean = 0, sd = 1);
    z_i <- z_i^2;
    chi_2 <- chi_2 + z_i;
  }
  
  return(chi_2);
}

sim_t <- function(degree_of_freedom = 1, M = 10) {
  v <- degree_of_freedom;
  c <- sim_chi2(degree_of_freedom = v, M = M);
  z <- rnorm(n = M, mean = 0, sd = 1);
  l <- z / sqrt(c / v);
  
  return(l);
}

sim_f <- function(degree_of_freedom_1 = 1,
                  degree_of_freedom_2 = 1,
                  M = 10) {
  dof_1 <- degree_of_freedom_1;
  dof_2 <- degree_of_freedom_2;
  c1    <- sim_chi2(degree_of_freedom = dof_1, M = M);
  c2    <- sim_chi2(degree_of_freedom = dof_2, M = M);
  f <- (c1/dof_1) / (c2/dof_2);
}

plot_random_walks <- function(df) {
  K <- max(unique(df$run));
  N <- max(unique(df$x));
  p <- 
    df %>%
      ggplot(aes(x, y)) +
      geom_point(alpha = .3,
                 size = .5,
                 colour = "black") +
      geom_hline(colour = "blue", alpha = .3, aes(yintercept = mean(y))) +
      geom_line(data = df_p, alpha = .7, colour = "red", aes(x = x, y = y)) +
      theme_light() +
      labs(x = "Step", y = "Cumulative sum of y",
           title = sprintf("%s Random Walk%s of %s step%s", K, ifelse(K > 1, "s", ""), 
                           N, ifelse(N > 1, "s", "")),
           subtitle = sprintf("Time-Average shown in red and overall mean (%s) in blue.", round(mean(df$y), 3)),
           caption = "Christian Bitter"
      ); 
  
  return(p);
}
