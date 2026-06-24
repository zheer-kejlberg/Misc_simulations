#### Simulation to evaluate the correct time-zero for matching and follow-up in a matched cohort study ####

# Load libraries
library(tidyverse)
library(furrr)
library(progressr)
library(tictoc)

# Set seed for reproducibility
set.seed(123) 

# Create function to simulate data
simulate_data <- function(n_t2d = 20000, n_non_t2d = 200000,
                          start_date = as.Date('2012-01-01'), 
                          end_date = as.Date('2024-12-31'),
                          death_rate = 3/1000, aid_rate = 1/100,
                          death_IRR = 1.5, aid_IRR = 1.2,
                          min_age = 18,
                          post_T2D_quarantine = 365.25/2) {
  n <- n_t2d + n_non_t2d
  data <- tibble(
    id = 1:n,
    sex = rbinom(n, 1, 0.5), # Random sex
    high_risk = rbinom(n, 1, 0.3), # Random high-risk status
    T2D_diagnosis_date = c(
        sample(seq(start_date, end_date, by="day"), n_t2d, replace=T), 
        rep(NA, n_non_t2d)
    ), # Random T2D diagnosis dates for T2D group, NA for non-T2D group
    birth_date = c(
        T2D_diagnosis_date[1:n_t2d] - 44*365.25 - (rexp(n_t2d, rate = 1) |> (\(x) x / max(x) * 26 * 365.25)()), # Random birth dates for T2D group
        sample(seq(as.Date('1950-01-01'), as.Date('1979-12-31'), by="day"), n_non_t2d, replace=T)
    ), # Random birth dates
    
    AID_date = birth_date + 20*365.25 + rexp(n, rate = aid_rate * 1.5^high_risk) * 365.25, # Time to AID diagnosis follows an exponential distribution
    death_date = birth_date + 50*365.25 + rexp(n, rate = death_rate * 1.5^high_risk) * 365.25 # Time to death follows an exponential distribution
  ) |>
  filter(is.na(T2D_diagnosis_date) | (T2D_diagnosis_date < pmin(AID_date, death_date, end_date, na.rm =T))) |>
  mutate(
    death_date = case_when(
            !is.na(T2D_diagnosis_date) ~ T2D_diagnosis_date + rexp(n(), rate = death_IRR * death_rate  * 1.5^high_risk) * 365.25,
            TRUE ~ death_date
    ),

    AID_date = case_when(
            !is.na(T2D_diagnosis_date) ~ T2D_diagnosis_date + rexp(n(), rate = aid_IRR * aid_rate * 1.5^high_risk) * 365.25,
            TRUE ~ AID_date
    )
  ) |>
  filter(
    is.na(T2D_diagnosis_date) | 
    (T2D_diagnosis_date < pmin(AID_date, death_date, end_date, na.rm =T) - post_T2D_quarantine & 
     T2D_diagnosis_date > birth_date + min_age*365.25)
  ) |>
    mutate(
        AID_date = case_when(
            AID_date >= death_date ~ NA,
            AID_date > end_date ~ NA,
            TRUE ~ AID_date
        ),
        death_date = case_when( 
            death_date > end_date ~ NA,
            TRUE ~ death_date
        ),
        first_event_date = pmin(AID_date, death_date, end_date, na.rm = TRUE),
        event = case_when(
            !is.na(AID_date) & AID_date == first_event_date ~ 1,
            !is.na(death_date) & death_date == first_event_date ~ 2,
            TRUE ~ 0
        )
    )

  data
}

# Create function to perform matching and calculate follow-up time
perform_matching <- function(df, match_ratio = 1, post_index_quarantine = 365.25/2, index_delay = 0) {
  
  df <- df |> mutate(birth_year = as.numeric(format(birth_date, "%Y")))
  
  t2d_group <- df |>
  filter(!is.na(T2D_diagnosis_date)) |> 
  mutate(match_group = row_number())

  non_t2d_group <- df |> filter(is.na(T2D_diagnosis_date))

  controls <- t2d_group |>
    rowwise() |>
    mutate(
      matched_controls = list({
        t2d_sex         <- .data$sex
        #t2d_birth_year  <- .data$birth_year
        t2d_match_group <- .data$match_group
        t2d_index_date  <- .data$T2D_diagnosis_date + index_delay
        t2d_birth_date  <- .data$birth_date


        output <- non_t2d_group |>
          filter(
            sex == t2d_sex,
            birth_date |> between(t2d_birth_date - 365.25, t2d_birth_date + 365.25),
            first_event_date > t2d_index_date + post_index_quarantine
          ) |>
          mutate(diff_birth = {
            abs(as.numeric(birth_date - t2d_birth_date))
          }) |>
          arrange(diff_birth) |>          
          slice_sample(n = match_ratio) |>
          pull(id)

          cat(sprintf("\rProgress: %d/%d = %.3f%%", t2d_match_group, nrow(t2d_group), 100*t2d_match_group / nrow(t2d_group)))  # Print the number of potential matches for debugging

        output
      })
    ) |>
    ungroup() |> 
    select(match_group, matched_controls) |>
    unnest(matched_controls) |>  
    mutate(matched_controls = unlist(matched_controls)) |>
    left_join(df, by = c("matched_controls" = "id")) |>
    rename(id = matched_controls)

  matched_data <- t2d_group |>
    bind_rows(controls) |>
    arrange(match_group, id) |>
    group_by(match_group) |>
    mutate(
      index_date = df$T2D_diagnosis_date[match_group] + index_delay,
      tte = difftime(first_event_date, index_date + post_index_quarantine, units = "days") |> as.numeric()
    ) |> 
    filter(n() > 1) |>
    ungroup()

  return(matched_data)
}


# Create function to fit Poisson regression and extract coefficient
get_coef <- function(df) {
  poisson_res <- glm(event == 1 ~ !is.na(T2D_diagnosis_date), family = poisson, offset = log(tte |> as.numeric()), data = df)
  poisson_res$coefficients[2] |> exp() |> round(3)
}


# Create function to run the entire simulation process
handlers(handler_progress())

run_all <- function(replication_no = 1, n_t2d = 20000, n_non_t2d = 200000, 
                    match_ratio = 1, post_index_quarantine = 365.25/2, index_delay = 0, 
                    prog = NULL) {
  
  #cat("\nReplication no.: ", replication_no, "\n")
  df <- simulate_data(n_t2d, n_non_t2d)
  matched_df <- perform_matching(df, match_ratio, post_index_quarantine = post_index_quarantine, index_delay = index_delay)
  if (!is.null(prog)) prog(sprintf(": Iteration number %d completed from total of %d completions (note: completions are not ordered)", replication_no, iterations))
  get_coef(matched_df)
}

# Run a single replication
run_all(n_t2d = 10000, n_non_t2d = 100000, match_ratio = 1, post_index_quarantine = 365.25/2, index_delay = 0)


#zz <- simulate_data(10000,100000) |> perform_matching(match_ratio = 1, post_index_quarantine = 365.25/2, index_delay = 0)
#glm(event == 1 ~ !is.na(T2D_diagnosis_date), family = poisson, offset = log(tte |> as.numeric()), data = zz) |> summary()

# Time complexity test: Run repeated simulations and measure time taken for different numbers of T2D cases
timed <- function(n, n2 = NULL) {
  if (is.null(n2)) n2 <- n * 10
  tic()
  run_all(n_t2d = n, n_non_t2d = n2, match_ratio = 1, post_index_quarantine = 365.25/2, index_delay = 0)
  cat("\n")
  z <- toc()
  time <- list()
  time[as.character(n)] <- as.numeric(z$toc - z$tic)
  return(time)
}

#b <- lapply(c(20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000), timed)
b |> unlist() |>
  enframe(name = "n_t2d", value = "time_seconds") |>
  mutate(n_t2d = as.numeric(n_t2d)) |>
  ggplot(aes(x = n_t2d, y = time_seconds)) +
  geom_line() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Time taken for simulation by number of T2D cases",
       x = "Number of T2D cases (log scale)",
       y = "Time taken (seconds, log scale)") +
  theme_minimal() + 
  geom_smooth(aes(x = n_t2d, y = time_seconds), formula = y ~ x * log(x), method = "lm", se = FALSE) #linear regression line


# Parallelize the simulation using furrr
plan(multisession)
iterations <- 20

with_progress({
  tic()
  p <- progressor(iterations)
  iterate_sim_early_match <- future_map(
    1:iterations, run_all,
    n_t2d = 10000, n_non_t2d = 100000, match_ratio = 1, post_index_quarantine = 365.25/2, index_delay = 0, prog = p,
    .options = furrr_options(seed = TRUE, chunk_size = 1)
  )
  toc()
})

with_progress({
  tic()
  p <- progressor(iterations)
  iterate_sim_late_match <- future_map(
    1:iterations, run_all,
    n_t2d = 10000, n_non_t2d = 100000, match_ratio = 1, post_index_quarantine = 0, index_delay = 365.25/2, prog = p,
    .options = furrr_options(seed = TRUE, chunk_size = 1)
  )
  toc()
})


# Create function to plot density of coefficients
plot_density <- function(list, title = "Density of Simulated Coefficients") {
  df <- tibble(coef = list |> unlist())
  
  ggplot(df, aes(x = coef)) +
    geom_density(fill = "lightblue", alpha = 0.5) +
    geom_vline(xintercept = mean(list |> unlist()), color = "red", linetype = "dashed") +
    labs(title = title,
         x = "Coefficient",
         y = "Density") +
    theme_minimal()
}

# Plot density of coefficients for early and late matching
d1 <- plot_density(iterate_sim_early_match, title = "Density of Simulated Coefficients for Early Matching")
d2 <- plot_density(iterate_sim_late_match, title = "Density of Simulated Coefficients for Late Matching")

d1
d2

# Save plots to files
ggsave("cohort_matching/density_early_matching2.png", plot = d1, width = 8, height = 6)
ggsave("cohort_matching/density_late_matching2.png", plot = d2, width = 8, height = 6)
