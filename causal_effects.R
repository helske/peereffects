
library(dplyr)
library(rstan)
library(forcats)

compute_direct_effects <- function(samples, d, condition_on, nsim) {
  
  inv_logit <- function(x) {
    x <- exp(x)
    x / (1 + x)
  }
  
  beta <- samples$beta
  mu <- samples$mu
  u <- samples$u
  beta_timegap_f <- samples$beta_timegap_f
  beta_timegap_peer_f <- samples$beta_timegap_peer_f
  beta_past_f <- samples$beta_past_f
  beta_past_peer_f <- samples$beta_past_peer_f
  beta_timegap_e <- samples$beta_timegap_e
  beta_timegap_peer_e <- samples$beta_timegap_peer_e
  beta_past_e <- samples$beta_past_e
  beta_past_peer_e <- samples$beta_past_peer_e
  
  if (missing(nsim)) nsim <- nrow(beta)
  rm(samples);gc()
  
  X0 <- d |> mutate(peer_leave = 0) |> 
    model.matrix(
      ~ month + 
        roll_sex_ratio + 
        region + 
        occupation +
        age * partner_age + 
        # interactions and main effects with the 2013 reform
        reform2013 * (
          education +
            partner_education + 
            income_decile * partner_higher_income +
            partner_income_decile + 
            industry + 
            sector
        ) +
        # interactions and main effects with the peer
        peer_leave * (
          reform2013 + 
            education * partner_education + 
            income_decile * partner_higher_income +
            partner_income_decile + 
            education_peer + 
            sector + 
            log_roll_emp
        ) +
        # extra terms for experienced fathers, experienced_extra = own_leave:reform2013_lag:reform2013
        experienced + 
        experienced:experienced_extra +
        experienced:experienced_extra:education + 
        experienced:income_decile + 
        experienced:peer_leave +
        experienced:reform2013:peer_leave + 
        experienced:education:peer_leave,
      data = _)
  
  X1 <- d |> mutate(peer_leave = 1) |> 
    model.matrix(
      ~ month + 
        roll_sex_ratio + 
        region + 
        occupation +
        age * partner_age + 
        # interactions and main effects with the 2013 reform
        reform2013 * (
          education +
            partner_education + 
            income_decile * partner_higher_income +
            partner_income_decile + 
            industry + 
            sector
        ) +
        # interactions and main effects with the peer
        peer_leave * (
          reform2013 + 
            education * partner_education + 
            income_decile * partner_higher_income +
            partner_income_decile + 
            education_peer + 
            sector + 
            log_roll_emp
        ) +
        # extra terms for experienced fathers, experienced_extra = own_leave:reform2013_lag:reform2013
        experienced + 
        experienced:experienced_extra +
        experienced:experienced_extra:education + 
        experienced:income_decile + 
        experienced:peer_leave +
        experienced:reform2013:peer_leave + 
        experienced:education:peer_leave,
      data = _)
  X0 <- X0[, -1]
  X1 <- X1[, -1]
  idx_f <- which(d$first_birth == 1)
  idx_e <- which(d$first_birth == 0)
  workplace <- d$workplace_id
  time_index <- d$time
  time_index <- time_index - min(time_index) + 1
  time_index_f <- time_index[idx_f]
  time_index_e <- time_index[idx_e]
  timegap <- as.integer(d$timegap_months)
  timegap_f <- timegap[idx_f]
  timegap_e <- timegap[idx_e]
  past_leaves <- as.integer(d$past_leaves)
  past_leaves_f <- past_leaves[idx_f]
  past_leaves_e <- past_leaves[idx_e]
  d <- data.frame(d)
  conds <- lapply(condition_on, function(x) fct_cross(!!!d[x]))
  cond_levels <- lapply(conds, levels)
  n_levels <- lengths(cond_levels)
  conds <- lapply(conds, as.integer)
  idx <- lapply(1:length(conds), function(j) lapply(1:n_levels[j], function(i) which(conds[[j]] == i)))
  ymeans0 <- ymeans1 <- vector("list", length(conds))
  for(i in 1:length(conds)) ymeans0[[i]] <- ymeans1[[i]] <- matrix(NA, n_levels[i], nsim)
  
  for(j in 1:nsim) {
    y <- mu[j, time_index] + u[j, workplace]
    y[idx_f] <- y[idx_f] + beta_past_f[j, past_leaves_f] + beta_timegap_f[j, timegap_f]
    y[idx_e] <- y[idx_e] + beta_past_e[j, past_leaves_e] + beta_timegap_e[j, timegap_e] 
    y0 <- y + X0 %*% beta[j, ]
    y1 <- y + X1 %*% beta[j, ]
    y1[idx_f] <- y1[idx_f] + beta_timegap_peer_f[j, timegap_f] + beta_past_peer_f[j, past_leaves_f]
    y1[idx_e] <- y1[idx_e] + beta_timegap_peer_e[j, timegap_e] + beta_past_peer_e[j, past_leaves_e]
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

d <- readRDS("data.rds")

d <- d |> 
  mutate(
    Father = factor(first_birth, levels = 1:0, labels = c("First-time", "Experienced"))
  )


condition_on <- list(
  "Father",
  "past_leaves",
  "education",
  "reform2013",
  c("education", "Father"),
  c("past_leaves", "Father"),
  c("reform2013", "Father"),
  c("reform2013", "education"),
  c("month", "year", "Father"),
  c("month", "year", "education", "Father"), 
  c("reform2013", "education", "Father"),
  c("past_leaves", "education", "Father"),
  c("partner_education", "education", "Father"),
  c("partner_higher_income", "education", "Father"),
  c("reform2013", "partner_education", "education", "Father"),
  c("reform2013", "education",  "partner_education"), 
  c("reform2013", "education",  "sector"),
  c("reform2013", "education",  "partner_education", "Father"), 
  c("reform2013", "education",  "sector", "Father")
)
fit <- readRDS("fit_model.rds")
#sumr <- posterior::as_draws(fit) |> posterior::summarise_draws()
samples <- extract(fit)
rm(fit);gc()

samples <- compute_direct_effects(samples, d, condition_on)

saveRDS(samples, file = "causal_effect_samples.rds")
qs <- c(0.025, 0.975)

samples$Father |> 
  mutate(peer_effect = y1 - y0) |> 
  group_by(iter) |> 
  summarise(
    difference = 
      peer_effect[Father == "First-time"] - peer_effect[Father == "Experienced"]
  ) |> 
  summarise(
    mean = mean(difference), 
    q2.5 = quantile(difference, 0.025),
    q97.5 = quantile(difference, 0.975),
    first_time_bigger = mean(difference > 0)
  )
edu_levels <- c("Basic", "Upper secondary", "Lower tertiary", "Higher tertiary")
samples$education_Father |> 
  mutate(
    education = ordered(education, levels = edu_levels),
    peer_effect = y1 - y0
  ) |> 
  group_by(iter, education) |> 
  summarise(
    difference = peer_effect[Father == "First-time"] - peer_effect[Father == "Experienced"]
  ) |> 
  group_by(education) |> 
  summarise(
    mean = mean(difference), 
    q2.5 = quantile(difference, 0.025),
    q97.5 = quantile(difference, 0.975),
    first_time_bigger = mean(difference > 0)
  )

samples$past_leaves_Father |> 
  mutate(
    past_leaves = ordered(past_leaves, labels = c("First-time", 1:4, "5+")),
    peer_effect = y1 - y0
  ) |> 
  group_by(iter, past_leaves) |> 
  summarise(
    difference = peer_effect[Father == "First-time"] - peer_effect[Father == "Experienced"]
  ) |> 
  group_by(past_leaves) |> 
  summarise(
    mean = mean(difference), 
    q2.5 = quantile(difference, 0.025),
    q97.5 = quantile(difference, 0.975),
    first_time_bigger = mean(difference > 0)
  )

aces <- lapply(samples, function(x) {
  x |> 
    group_by(across(-c(iter, y0, y1))) |> 
    summarise(
      mean = mean(y1 - y0), 
      q2.5 = quantile(y1 - y0, 0.025),
      q97.5 = quantile(y1 - y0, 0.975),
      .groups = "drop")
})
interventional_means <- samples$month_year_education_Father |> 
  filter(!(year == "2010" & month == 1)) |> 
  tidyr::pivot_longer(c(y0, y1), names_to = "Peer") |> 
  mutate(Peer = factor(Peer, levels = c("y0", "y1"), labels = c("No quota", "Quota"))) |> 
  group_by(month, year, education, Father, Peer) |> 
  summarise(
    mean = mean(value), 
    q2.5 = quantile(value, 0.025),
    q97.5 = quantile(value, 0.975),
    .groups = "drop"
  )

ace_diff_father_edu <-  samples$education_Father |> 
  group_by(education) |> 
  summarise(
    mean = mean(
      (y1[Father == "First-time"] - y0[Father == "First-time"]) - 
        (y1[Father == "Experienced"] - y0[Father == "Experienced"])
    ),
    q2.5 = quantile(
      (y1[Father == "First-time"] - y0[Father == "First-time"]) - 
        (y1[Father == "Experienced"] - y0[Father == "Experienced"]),
      probs = 0.025
    ),
    q97.5 = quantile(
      (y1[Father == "First-time"] - y0[Father == "First-time"]) - 
        (y1[Father == "Experienced"] - y0[Father == "Experienced"]),
      probs = 0.975),
    .groups = "drop"
  )

save(
  list = c("interventional_means", ls(pattern = "yearly_inter"), 
           ls(pattern = "ace")), 
  file = "causal_effects.rda")

rm(d)
gc()