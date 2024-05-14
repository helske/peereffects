
library(dplyr)
library(rstan)

compute_direct_effects <- function(samples, d, condition_on) {
  
  inv_logit <- function(x) {
    x <- exp(x)
    x / (1 + x)
  }
  
  beta <- posterior::draws_of(samples$beta)
  mu <- posterior::draws_of(samples$mu)
  u <- posterior::draws_of(samples$sigma_u * samples$u_std)
  beta_timegap <- posterior::draws_of(samples$beta_timegap)
  beta_past <- posterior::draws_of(samples$beta_past)
  beta_past_peer <- posterior::draws_of(samples$beta_past_peer)
  nsim <- nrow(beta)
  rm(samples);gc()
  X0 <- d %>% mutate(peer_leave = 0) %>% 
    model.matrix(
      ~ month + 
        industry + 
        region + 
        occupation +
        log_roll_emp + 
        roll_sex_ratio +
        education +
        partner_education +
        education_difference_partner +
        education_difference_peer +
        income_decile +
        partner_income_decile +
        partner_higher_income +
        age + 
        partner_age +
        reform2013 * income_decile +
        reform2013 * education +
        reform2013 * industry + 
        before_2010reform_lag +
        before_2013reform_lag +
        # for those who have earlier children
        own_previous_birth_peer_timing * education +
        (reform2013 +
           education +
           industry +
           age + 
           income_decile +
           log_roll_emp +
           before_2010reform_lag +
           before_2013reform_lag +
           # for those who have earlier children
           own_previous_birth_peer_timing
        ) * peer_leave,
      data = .)
  X1 <- d %>% mutate(peer_leave = 1) %>% 
    model.matrix(
      ~ month + 
        industry + 
        region + 
        occupation +
        log_roll_emp + 
        roll_sex_ratio +
        education +
        partner_education +
        education_difference_partner +
        education_difference_peer +
        income_decile +
        partner_income_decile +
        partner_higher_income +
        age + 
        partner_age +
        reform2013 * income_decile +
        reform2013 * education +
        reform2013 * industry + 
        before_2010reform_lag +
        before_2013reform_lag +
        # for those who have earlier children
        own_previous_birth_peer_timing * education +
        (reform2013 +
           education +
           industry +
           age + 
           income_decile +
           log_roll_emp +
           before_2010reform_lag +
           before_2013reform_lag +
           # for those who have earlier children
           own_previous_birth_peer_timing
        ) * peer_leave,
      data = .)
  X0 <- X0[, -1]
  X1 <- X1[, -1]
  peer_idx <- grep("peer_leave", colnames(X0))
  X0 <- cbind(X0[, -peer_idx], X0[, peer_idx])
  X1 <- cbind(X1[, -peer_idx], X1[, peer_idx])
  
  workplace <- as.integer(factor(d %>% pull(workplace_id)))
  own_after_peer <- d %>% pull(own_after_peer)
  time_index <- d %>% pull(time)
  time_index <- time_index - min(time_index) + 1
  timegap <- as.integer(d %>% pull(timegap))
  past_leaves <- as.integer(d %>% pull(past_leaves))
  conds <- lapply(condition_on, function(x) forcats::fct_cross(!!!d[x]))
  cond_levels <- lapply(conds, levels)
  n_levels <- lengths(cond_levels)
  conds <- lapply(conds, as.integer)
  
  idx <- lapply(1:length(conds), function(j) lapply(1:n_levels[j], function(i) which(conds[[j]] == i & own_after_peer == 0)))
  ymeans0 <- ymeans1 <- vector("list", length(conds))
  for(i in 1:length(conds)) ymeans0[[i]] <- ymeans1[[i]] <- matrix(NA, n_levels[i], nsim)
  
  for(j in 1:nsim) {
    y_ <- mu[j, time_index] + u[j, workplace] + beta_past[j, past_leaves]
    y0 <- y_ + X0 %*% beta[j, ]
    y1 <- y_ + X1 %*% beta[j, ] + beta_timegap[j, timegap] + beta_past_peer[j, past_leaves]
    ymean0 <- inv_logit(y0)
    ymean1 <- inv_logit(y1)
    # marginalise over fathers
    for(k in 1:length(n_levels)) {
      for(i in 1:n_levels[k]) {
        ymeans0[[k]][i, j] <- mean(ymean0[idx[[k]][[i]]])
        ymeans1[[k]][i, j] <- mean(ymean1[idx[[k]][[i]]])
      }
    }
  }
  out <- lapply(
    1:length(n_levels), 
    function(i) {
      data.frame(
        y0 = c(ymeans0[[i]]), 
        y1 = c(ymeans1[[i]]), 
        tidyr::separate_wider_delim(
          data.frame(conditions = cond_levels[[i]]),
          delim = ":", names = condition_on[[i]], 
          cols = conditions
        ),
        iter = rep(1:nsim, each = n_levels[i])
      )
    }
  )
  names(out) <- sapply(condition_on, paste0, collapse = "_")
  out
}

d <- readRDS("W:/JouniH/peereffects/data.rds")

d <- d %>% mutate(
  parity = factor(first_birth, levels = 1:0, labels = c("0", "1+")),
  before_2013_reform = factor(reform2013, labels = c("before", "after")),
  past_leaves = n_past_leave_takers,
  timegap = timegap_months
)


conditions <- list(
  "parity",
  c("month", "year", "education", "parity"), 
  c("timegap", "education", "parity"),
  c("education", "parity"),
  c("before_2013_reform", "parity"),
  c("before_2013_reform", "education", "parity"),
  c("past_leaves", "education", "parity"),
  c("year", "past_leaves", "education", "parity"),
  c("education_difference_partner", "education", "parity"),
  c("partner_higher_income", "education", "parity"),
  c("year", "education", "parity")
)

source("W:/JouniH/functions_for_stan/readcsv.R")
draws <- read_stan_csv_custom(
  paste0("W:/JouniH/peereffects/posterior_samples_", 1:4, ".csv")
)

samples <- compute_direct_effects(draws, d, condition_on = conditions)

saveRDS(
  samples, 
  file = "W:/JouniH/peereffects/causal_effect_samples.rds"
)
rm(draws)
qs <- c(0.025, 0.975)

aces <- lapply(samples, function(x) {
  x %>% 
    group_by(across(-c(iter, y0, y1))) %>% 
    summarise(
      mean = mean(y1 - y0), 
      tibble::as_tibble_row(
        quantile(y1 - y0, probs = qs), 
        .name_repair = \(x) paste0("q", readr::parse_number(x)))
    )
})
interventional_means <- samples$month_year_education_parity %>% 
  filter(!(year == "2010" & month == 1)) %>% 
  tidyr::pivot_longer(c(y0, y1), names_to = "peer") %>% 
  mutate(Peer = factor(peer, levels = c("y0", "y1"), labels = c("no quota", "quota"))) %>% 
  group_by(month, year, education, parity, Peer) %>% 
  summarise(
    mean = mean(value), 
    tibble::as_tibble_row(
      quantile(value, probs = qs), 
      .name_repair = \(x) paste0("q", readr::parse_number(x)))
  )
ace_diff_parity_edu <-  samples$education_parity %>% 
  group_by(education) %>% 
  summarise(
    mean = mean(
      (y1[parity == "0"] - y0[parity == "0"]) - (y1[parity == "1+"] - y0[parity == "1+"])
    ),
    q2.5 = quantile(
      (y1[parity == "0"] - y0[parity == "0"]) - (y1[parity == "1+"] - y0[parity == "1+"]),
      probs = 0.025
    ),
    q97.5 = quantile(
      (y1[parity == "0"] - y0[parity == "0"]) - (y1[parity == "1+"] - y0[parity == "1+"]),
      probs = 0.975)
  )

ace_magnitude_prob <- samples$year_education_parity %>%
  group_by(year, education, parity) %>%
  summarise(
    x1 = mean((y1 - y0) <= 0),
    x2 = mean((y1 - y0) > 0 & (y1 - y0) <= 0.02),
    x3 = mean((y1 - y0) > 0.02 & (y1 - y0) <= 0.04),
    x4 = mean((y1 - y0) > 0.04 & (y1 - y0) <= 0.06),
    x5 = mean((y1 - y0) > 0.06 & (y1 - y0) <= 0.08),
    x6 = mean((y1 - y0) > 0.08 & (y1 - y0) <= 0.1),
    x7 = mean((y1 - y0) > 0.1)
  ) %>%
  tidyr::pivot_longer(
    cols = starts_with("x"),
    names_to = "between",
    names_pattern = "x(.*)",
    values_to = "probability"
  ) %>%
  mutate(between = ordered(
    between, levels = 7:1, 
    labels = rev(paste0(c("less than 0", "0-2", "2-4", "4-6", "6-8", "8-10", "more than 10"), "%"))))

save(
  list = c("interventional_means", ls(pattern = "ace")), 
  file = "W:/JouniH/peereffects/causal_effects.rda")

rm(d)
gc()
gc()


