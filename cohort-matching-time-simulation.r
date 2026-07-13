#### Simulation to evaluate the correct time-zero for matching and follow-up in a matched cohort study ####

# Load libraries
library(tidyverse)
library(furrr)
library(progressr)
library(tictoc)

# Set seed for reproducibility
set.seed(123) 

# Create function to rescale exponential random variables to a specific range
rescale_exp <- function(x, min_val, max_val) {
  rescaled <- (x - min(x)) / (max(x) - min(x))
  rescaled <- rescaled * (max_val - min_val) + min_val
  rescaled
}

# Create function to simulate data
simulate_data <- function(n_t2d = 10000, n_non_t2d = 100000,
                          start_date = as.Date('2012-01-01'), 
                          end_date = as.Date('2024-12-31'),
                          death_rate = 3/1000, aid_rate = 1/100,
                          death_IRR = 1.5, aid_IRR = 1.2,
                          min_age = 18) {
  n_t2d <- (10000/7221.613 * n_t2d) |> round()
  n <- n_t2d + n_non_t2d
  data <- tibble(
    end_of_study = end_date,
    id = 1:n,
    sex = rbinom(n, 1, 0.5), # Random sex
    T2D_diagnosis_date = c(
        sample(seq(start_date, end_date, by="day"), n_t2d, replace = T), 
        rep(NA, n_non_t2d)
    ), # Uniform random T2D diagnosis dates for T2D group, NA for non-T2D group
    high_risk = rbinom(n, 1, 0.3 + 0.15 * !is.na(T2D_diagnosis_date)), # Random high-risk status
    birth_date = c(
        #T2D_diagnosis_date[1:n_t2d] - 44*365.25 - (rexp(n_t2d, rate = 1) |> (\(x) x / max(x) * 26 * 365.25)()), # Random birth dates for T2D group with exponentially distributed age at diagnosis after age 44, scaled to a maximum of 26 years
        T2D_diagnosis_date[1:n_t2d] - rescale_exp(rexp(n_t2d, rate = 1), 44, 70)*365.25, # Random birth dates for T2D group with exponentially distributed age at diagnosis after age 44, scaled to a maximum of 26 years
        sample(seq(as.Date('1940-01-01'), as.Date('1980-12-31'), by="day"), n_non_t2d, replace=T)
    ), # Uniform random birth dates
    
    AID_date = birth_date + 20*365.25 + rexp(n, rate = aid_rate * 2^high_risk) * 365.25, # Time to AID diagnosis follows an exponential distribution
    death_date = birth_date + 50*365.25 + rexp(n, rate = death_rate * 2^high_risk) * 365.25 # Time to death follows an exponential distribution
  ) |>
  mutate(
    T2D_diagnosis_date = case_when(
      !is.na(T2D_diagnosis_date) & T2D_diagnosis_date >= pmin(AID_date, death_date, end_of_study, na.rm =T) ~ NA,
      TRUE ~ T2D_diagnosis_date
    )
  ) |>
  #filter(is.na(T2D_diagnosis_date) | (T2D_diagnosis_date < pmin(AID_date, death_date, end_of_study, na.rm =T))) |>
  mutate(
    death_date = case_when(
            !is.na(T2D_diagnosis_date) ~ T2D_diagnosis_date + rexp(n(), rate = death_IRR * death_rate  * 2^high_risk) * 365.25,
            TRUE ~ death_date
    ),

    AID_date = case_when(
            !is.na(T2D_diagnosis_date) ~ T2D_diagnosis_date + rexp(n(), rate = aid_IRR * aid_rate * 2^high_risk) * 365.25,
            TRUE ~ AID_date
    )
  ) |>
  filter(
    is.na(T2D_diagnosis_date) | T2D_diagnosis_date > birth_date + min_age*365.25
  ) |>
    mutate(
        AID_date = case_when(
            AID_date >= death_date ~ NA,
            AID_date > end_of_study ~ NA,
            TRUE ~ AID_date
        ),
        death_date = case_when( 
            death_date > end_of_study ~ NA,
            TRUE ~ death_date
        ),
        first_event_date = pmin(AID_date, death_date, end_of_study, na.rm = TRUE),
        event = case_when(
            !is.na(AID_date) & AID_date == first_event_date ~ 1,
            !is.na(death_date) & death_date == first_event_date ~ 2,
            TRUE ~ 0
        )
    )

  data
}

z <- \(x) simulate_data(10000,100000) |> filter(!is.na(T2D_diagnosis_date)) |> nrow()
sapply(1:100, z) |> mean()

# Create function to perform matching and calculate follow-up time
perform_matching <- function(df, match_ratio = 1, 
                             post_matching_quarantine = 365.25/2, 
                             match_time_delay = 0,
                             fu_start_delay = 365.25/2) {
  
  df <- df |> mutate(birth_year = as.numeric(format(birth_date, "%Y")))
  
  t2d_group <- df |>
  filter(
    !is.na(T2D_diagnosis_date),
    T2D_diagnosis_date < pmin(AID_date, death_date, end_of_study, na.rm = T) - post_matching_quarantine
  ) |> 
  mutate(match_group = row_number())

  #control_group <- df |> filter(is.na(T2D_diagnosis_date))

  controls <- t2d_group |>
    rowwise() |>
    mutate(
      matched_controls = list({
        t2d_sex         <- .data$sex
        #t2d_birth_year  <- .data$birth_year
        t2d_match_group <- .data$match_group
        t2d_birth_date  <- .data$birth_date
        matching_date  <- .data$T2D_diagnosis_date + match_time_delay


        control_ids <- df |>
          filter(
            sex == t2d_sex,
            birth_date |> between(t2d_birth_date - 365.25, t2d_birth_date + 365.25),
            first_event_date > matching_date + post_matching_quarantine,
            is.na(T2D_diagnosis_date) | T2D_diagnosis_date > matching_date + post_matching_quarantine
          ) |>
          mutate(
            #first_event_date = pmin(first_event_date, T2D_diagnosis_date),
            diff_birth = abs(as.numeric(birth_date - t2d_birth_date))
          ) |>
          slice_min(diff_birth, n = match_ratio) |>
          pull(id)

          cat(sprintf("\rProgress: %d/%d = %.3f%%", t2d_match_group, nrow(t2d_group), 100*t2d_match_group / nrow(t2d_group)))  # Print the number of potential matches for debugging

        control_ids
      })
    ) |>
    ungroup() |> 
    select(match_group, matched_controls) |>
    unnest(matched_controls) |>  
    mutate(matched_controls = unlist(matched_controls)) |>
    left_join(df, by = c("matched_controls" = "id")) |>
    rename(id = matched_controls) |>
    mutate(
        first_event_date = pmin(first_event_date, T2D_diagnosis_date, na.rm=TRUE)
    )

  matched_data <- t2d_group |>
    bind_rows(controls) |>
    #arrange(match_group, id) |>
    group_by(match_group) |>
    mutate(
      matching_date = t2d_group$T2D_diagnosis_date[match_group] + match_time_delay,
      tte = difftime(first_event_date, matching_date + fu_start_delay, units = "days") |> as.numeric()
    ) |> 
    filter(n() > 1) |>
    ungroup()

  matched_data
}


# Create function to fit Poisson regression and extract coefficient
get_coef <- function(df) {
  df <- df |> mutate(event = as.numeric(event == 1))
  poisson_res <- glm(event ~ !is.na(T2D_diagnosis_date), family = poisson, offset = log(tte |> as.numeric()), data = df)
  log_irr <- poisson_res$coefficients[2] |> unname() #|> exp() |> round(3)
  se_log_irr <- summary(poisson_res)$coefficients[2, 2]
  list(log_irr = log_irr, se_log_irr = se_log_irr)
}

# Create function to extract just the IRR coefficient from the coef output
get_irr <- function(coef_output) {
  exp(coef_output$log_irr)
}


# Create function to run the entire simulation process
handlers(handler_progress())

run_all <- function(replication_no = 1, n_t2d = 20000, n_non_t2d = 200000, 
                    match_ratio = 1, post_matching_quarantine = 365.25/2, match_time_delay = 0, fu_start_delay = 365.25/2, prog = NULL) {
  tic()
  #cat("\nReplication no.: ", replication_no, "\n")
  df <- simulate_data(n_t2d, n_non_t2d)
  matched_df <- perform_matching(df, match_ratio, post_matching_quarantine = post_matching_quarantine, match_time_delay = match_time_delay, fu_start_delay = fu_start_delay)
  if (!is.null(prog)) prog(sprintf(": Worker number %d finished (total %d workers). Note: completions are not ordered", replication_no, iterations))
  toc()
  get_coef(matched_df)
}


# Run a single replication
run_all(n_t2d = 1000, n_non_t2d = 10000)
run_all(n_t2d = 10000, n_non_t2d = 100000, match_ratio = 1, post_matching_quarantine = 365.25/2, match_time_delay = 0, fu_start_delay = 0)


# Time complexity test: Run repeated simulations and measure time taken for different numbers of T2D cases
timed <- function(n, n2 = NULL) {
  if (is.null(n2)) n2 <- n * 10
  tic()
  run_all(n_t2d = n, n_non_t2d = n2, match_ratio = 1, post_matching_quarantine = 365.25/2, match_time_delay = 0)
  cat("\n")
  z <- toc()
  time <- list()
  time[as.character(n)] <- as.numeric(z$toc - z$tic)
  time
}

times <- lapply(c(20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000), timed)
time_complexity <- times |> 
  unlist() |>
  enframe(name = "n_t2d", value = "time_seconds") |>
  mutate(n_t2d = as.numeric(n_t2d)) |>
  ggplot(aes(x = n_t2d, y = time_seconds)) +
  geom_line() +
  geom_point() +
  labs(title = "Time taken for simulation by number of T2D cases",
       x = "Number of T2D cases",
       y = "Time taken (seconds") +
  theme_minimal() + 
  geom_smooth(aes(x = n_t2d, y = time_seconds), formula = y ~ x * log(x), method = "lm", se = FALSE) #linear regression line
time_complexity

# Parallelize the simulation using furrr
plan(multisession)
iterations <- 100
n_t2d <- 10000
n_non_t2d <- 20000

with_progress({
  tic()
  p <- progressor(iterations)
  iterate_sim_early_match <- future_map(
    1:iterations, run_all, 
    n_t2d = n_t2d, n_non_t2d = n_non_t2d, match_ratio = 1, post_matching_quarantine = 365.25/2, match_time_delay = 0, prog = p,
    .options = furrr_options(seed = TRUE, chunk_size = 1)
  )
  toc()
})

with_progress({
  tic()
  p <- progressor(iterations)
  iterate_sim_late_match <- future_map(
    1:iterations, run_all,
    n_t2d = n_t2d, n_non_t2d = n_non_t2d, match_ratio = 1, post_matching_quarantine = 0, match_time_delay = 365.25/2, prog = p,
    .options = furrr_options(seed = TRUE, chunk_size = 1)
  )
  toc()
})




# Create function to perform meta-analysis on the coefficients
meta_analyse <- function(list_of_results) {
  log_irr <- sapply(list_of_results, function(x) x$log_irr)
  se_log_irr <- sapply(list_of_results, function(x) x$se_log_irr)
  
  # Calculate weights for each study (inverse of variance)
  weights <- 1 / (se_log_irr^2)
  
  # Calculate the weighted mean of log IRR
  weighted_mean_log_irr <- sum(weights * log_irr) / sum(weights)
  
  # Calculate the standard error of the weighted mean
  se_weighted_mean_log_irr <- sqrt(1 / sum(weights))
  
  # Calculate the 95% confidence interval
  ci_lower <- weighted_mean_log_irr - 1.96 * se_weighted_mean_log_irr
  ci_upper <- weighted_mean_log_irr + 1.96 * se_weighted_mean_log_irr
  
  data.frame(
    mean = exp(weighted_mean_log_irr),
    xmin = exp(ci_lower),
    xmax = exp(ci_upper)
  )
}

library(metafor)
meta_analyse2 <- function(list_of_results) {
  log_irr <- sapply(list_of_results, function(x) x$log_irr)
  se_log_irr <- sapply(list_of_results, function(x) x$se_log_irr)
  
  # Perform random-effects meta-analysis using the 'metafor' package
  res <- rma(yi = log_irr, sei = se_log_irr, method = "REML")
  
  data.frame(
    mean = exp(res$b),
    xmin = exp(res$ci.lb),
    xmax = exp(res$ci.ub)
  )
}

iterate_sim_late_match |> meta_analyse() #|> print()
iterate_sim_late_match |> meta_analyse2() #|> print()

# Create function to plot density of coefficients
plot_density <- function(list, title = "Density of Simulated Coefficients") {
  IRRs <- list |> sapply(function(x) x$log_irr) |> exp()
  df <- tibble(coef = IRRs)
  
  #ci_df <- data.frame(
  #  mean = mean(unlist(list)),
  #  xmin = mean(unlist(list)) - 1.96 * sd(unlist(list)) / sqrt(length(unlist(list))),
  #  xmax = mean(unlist(list)) + 1.96 * sd(unlist(list)) / sqrt(length(unlist(list)))
  #)

  ci_df <- list |> meta_analyse2()

  ggplot(df, aes(x = coef)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    geom_vline(xintercept = mean(list |> unlist()), color = "red", linetype = "dashed") +
    labs(title = title, subtitle = sprintf("1:1 matched, ~ %d with T2D included from population of %dk w T2D and %dk w/o", n_t2d, n_t2d/1000, n_non_t2d/1000),
         x = "Coefficient",
         y = "Density") +
    theme_minimal() + 
    geom_vline(xintercept = 1.2, color = "black", linetype = "dashed") + # ground truth line at 1.2 for AID
    annotate("text", x = 1.2, y = 0, label = "Ground Truth: 1.2", color = "black", vjust = -1, hjust = 0, angle = 90) + # Add text annotation for ground truth
    annotate("text", x = (ci_df$mean + 0.015), y = 0, label = sprintf("Estimate: %.3f (%.3f - %.3f)", ci_df$mean, ci_df$xmin, ci_df$xmax), color = "red", vjust = -1, hjust = 0, angle = 90) + # Add text annotation for mean coefficient
    # add confidence band around the mean coefficient
    geom_rect(data = ci_df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf), fill = "red", alpha = 0.2, inherit.aes = FALSE)
}

# Plot density of coefficients for early and late matching
d1 <- plot_density(iterate_sim_early_match, title = sprintf("Density of Simulated Coefficients for Early Matching (%d iterations)", iterations))
d2 <- plot_density(iterate_sim_late_match, title = sprintf("Density of Simulated Coefficients for Late Matching (%d iterations)", iterations))

d1
d2

# Save plots to files
ggsave("cohort_matching/1. density_early_matching.png", plot = d1, width = 8, height = 6)
ggsave("cohort_matching/2. density_late_matching.png", plot = d2, width = 8, height = 6)

ggsave("cohort_matching/3. time_complexity.png", plot = time_complexity, width = 8, height = 6)
