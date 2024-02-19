#### A monte-carlo simulation of the effects of "peeking" during a study ####
#### In each iteration, a dataset is made with the same distribution of x and y
#### Then, in the "peeking" variation, one observation from the dataset is "recruited" 
#### (added to the analysis set) at a time, until p < 0.05, and the results are taken to be final.
#### In the non-peeking variation, analysis is always performed on the full dataset.

#### --------------------------------------------------- ####
#### Dependencies
library(ggplot2)
library(plyr)
library(ThemePark)
library(gridExtra)
library(cowplot)

#### --------------------------------------------------- ####
#### Set baseline vars
n_of_people <- 1000
n_of_sims <- 1000
method1 <- paste0("n = ",n_of_people)
method2 <- "Stopped at p<.05"

#### --------------------------------------------------- ####
#### Create functions

create_dataset <- function(n = n_of_people) {
  x <- rnorm(n, 0, 2)
  df <- data.frame(
    x = x,
    y = 0.05*x + rnorm(n,0,3)
  )
  return(df)
}
t <- create_dataset()
summary(lm(y~x,t))[["coefficients"]][2,2]
coef(summary(lm(y~x,t)))[2,2]

stopping_rule <- function() {
  df <- create_dataset()
  
  coefs <- coef(summary(lm(y~x, data = df)))
  results <- data.frame(
    method = method1,
    p = coefs[2,4],
    r = as.numeric(cor.test(df$x,df$y,method="pearson")$estimate),
    beta = coefs[2,1],
    n = n_of_people,
    se = coefs[2,2]
  )
  for (row_i in 10:length(df$x)) {
    cat("Observation no.: ",row_i, " \r")
    flush.console()
    df_stopped <- df[1:row_i,]
    coefs <- coef(summary(lm(y~x, data = df_stopped)))
    p = coefs[2,4]
    
    if (p < 0.05 | row_i == nrow(df)) {
      results_stopped <- data.frame(
        method = method2,
        p = coefs[2,4],
        r = as.numeric(cor.test(df_stopped$x,df_stopped$y,method="pearson")$estimate),
        beta = coefs[2,1],
        n = row_i,
        se = coefs[2,2]
      )
      return(rbind(results, results_stopped))
    }
  }
}

run_simulation <- function() {
  for (i in 1:n_of_sims) {
    print(paste0("Iteration no.: ",i,"..."))
    if (i == 1) { pooled_results <- stopping_rule() }
    else { pooled_results <- rbind(pooled_results, stopping_rule()) }
  }
  return(pooled_results)
}

create_plot <- function(df, var, type = "density", ticks = seq(-1,1,0.05)) {
  mu <- data.frame(
    method = c(method1, method2),
    grp.mean = c(mean(df[df$method == method1,][[var]]),mean(df[df$method == method2,][[var]]))
  )
  if (type == "density") {
    plot <- ggplot(df, aes(x=.data[[var]], fill=method)) +
      geom_density(alpha=0.4) +
      geom_vline(data=mu, aes(xintercept=grp.mean, color=method),
                 linetype="dashed") +
      theme_barbie()
  } else {
    plot <- ggplot(df, aes(x=.data[[var]], color=method)) +
      stat_ecdf(alpha=0.4) +
      scale_x_continuous(breaks=ticks) +
      theme_barbie()
  }
  return(plot)
}

#### --------------------------------------------------- ####
#### Run simulations
res <- run_simulation()

#### --------------------------------------------------- ####
#### Plots
plot_p <- create_plot(res, "p")
plot_r <- create_plot(res, "r")
plot_beta <- create_plot(res, "beta")
plot_n <- create_plot(res, "n")
plot_se <- create_plot(res, "se")

plot_p2 <- create_plot(res, "p", type = "ecdf", ticks = seq(0.05,0.95,0.1))
plot_r2 <- create_plot(res, "r", type = "ecdf")
plot_beta2 <- create_plot(res, "beta", type = "ecdf")
plot_n2 <- create_plot(res, "n", type = "ecdf", ticks = seq(0,1000,100))
plot_se2 <- create_plot(res, "se", type = "ecdf", ticks = seq(0,1000,0.1))

plot_grid(plot_p, plot_p2,
          plot_r, plot_r2,
          plot_n, plot_n2,
          plot_beta, plot_beta2,
          plot_se, plot_se2,
          ncol = 2, nrow = 5)

plot_grid(plot_p, plot_p2,
          plot_r, plot_n2,
          plot_beta, plot_se2,
          ncol = 2, nrow = 3)