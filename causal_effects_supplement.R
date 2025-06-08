
library(dplyr)
library(rstan)
library(forcats)

compute_direct_effects_timegap <- function(samples, d, condition_on, nsim) {
  
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
compute_direct_effects_reform <- function(samples, d, condition_on, nsim) {
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
        industry + 
        # interactions and main effects with the 2013 reform
        # reform2013 * (
        #   education +
        #     partner_education + 
        #     income_decile * partner_higher_income +
        #     partner_income_decile + 
        #     industry + 
        #     sector
        # ) +
        # interactions and main effects with the peer
        peer_leave * (
          # reform2013 + 
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
        # experienced:reform2013:peer_leave + 
        experienced:education:peer_leave,
      data = _)
  
  X1 <- d |> mutate(peer_leave = 1) |> 
    model.matrix(
      ~ month + 
        roll_sex_ratio + 
        region + 
        occupation +
        age * partner_age + 
        industry + 
        # interactions and main effects with the 2013 reform
        # reform2013 * (
        #   education +
        #     partner_education + 
        #     income_decile * partner_higher_income +
        #     partner_income_decile + 
        #     industry + 
        #     sector
        # ) +
        # interactions and main effects with the peer
        peer_leave * (
          # reform2013 + 
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
        # experienced:reform2013:peer_leave + 
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

condition_on <- list(
  "Father",
  "past_leaves",
  "education",
  "reform2013",
  c("year", "education"),
  c("year", "education", "Father"),
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
  c("reform2013", "partner_education", "education", "Father")
)


d <- readRDS("data.rds") |> 
  dplyr::filter(timegap_months %in% levels(timegap_months)[-(1:4)]) |> 
  droplevels()

d <- d |> 
  mutate(
    Father = factor(first_birth, levels = 1:0, labels = c("First-time", "Experienced"))
  )

fit <- readRDS("fit_model_timegap.rds")
samples <- extract(fit)
rm(fit);gc()

samples <- compute_direct_effects_timegap(samples, d, condition_on)

qs <- c(0.025, 0.975)

aces <- lapply(samples, function(x) {
  x |> 
    group_by(across(-c(iter, y0, y1))) |> 
    summarise(
      mean = mean(y1 - y0), 
      q2.5 = quantile(y1 - y0, 0.025),
      q97.5 = quantile(y1 - y0, 0.975),
      .groups = "drop")
})

saveRDS(aces, file = "causal_effects_aces_timegap.rds")

fit <- readRDS("fit_model_reform.rds")
samples <- extract(fit)
rm(fit);gc()


d <- readRDS("data.rds") |> 
  dplyr::filter(reform2013 == "eligible" & year %in% 2013:2017) |> 
  droplevels()

d <- d |> 
  mutate(
    Father = factor(first_birth, levels = 1:0, labels = c("First-time", "Experienced"))
  )
condition_on <- list(
  "Father",
  "past_leaves",
  "education",
  c("year", "education"),
  c("year", "education", "Father"),
  c("education", "Father"),
  c("past_leaves", "Father"),
  c("month", "year", "Father"),
  c("month", "year", "education", "Father"),
  c("past_leaves", "education", "Father"),
  c("partner_education", "education", "Father"),
  c("partner_higher_income", "education", "Father")
)
samples <- compute_direct_effects_reform(samples, d, condition_on)

qs <- c(0.025, 0.975)

aces <- lapply(samples, function(x) {
  x |> 
    group_by(across(-c(iter, y0, y1))) |> 
    summarise(
      mean = mean(y1 - y0), 
      q2.5 = quantile(y1 - y0, 0.025),
      q97.5 = quantile(y1 - y0, 0.975),
      .groups = "drop")
})

saveRDS(aces, file = "causal_effects_aces_reform.rds")


rm(d)
gc()

compute_direct_effects_large_wps <- function(samples, d, condition_on, nsim) {
  
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
            industry
        ) +
        # interactions and main effects with the peer
        peer_leave * (
          reform2013 + 
            education * partner_education + 
            income_decile * partner_higher_income +
            partner_income_decile + 
            education_peer + 
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
            industry
        ) +
        # interactions and main effects with the peer
        peer_leave * (
          reform2013 + 
            education * partner_education + 
            income_decile * partner_higher_income +
            partner_income_decile + 
            education_peer + 
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

condition_on <- list(
  "Father",
  "past_leaves",
  "education",
  "reform2013",
  c("year", "education"),
  c("year", "education", "Father"),
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
  c("reform2013", "partner_education", "education", "Father")
)


d <- readRDS("data_large_wps.rds")

d <- d |> 
  mutate(
    Father = factor(first_birth, levels = 1:0, labels = c("First-time", "Experienced"))
  )

fit <- readRDS("fit_model_large_wps.rds")
samples <- extract(fit)
rm(fit);gc()

samples <- compute_direct_effects_large_wps(samples, d, condition_on)

qs <- c(0.025, 0.975)

aces <- lapply(samples, function(x) {
  x |> 
    group_by(across(-c(iter, y0, y1))) |> 
    summarise(
      mean = mean(y1 - y0), 
      q2.5 = quantile(y1 - y0, 0.025),
      q97.5 = quantile(y1 - y0, 0.975),
      .groups = "drop")
})

saveRDS(aces, file = "causal_effects_aces_large_wps.rds")

load("causal_effects.rda")
aces$reform2013_education_sector |> 
  mutate(
    mean = round(100*mean, 1),
    q2.5 = round(100*q2.5, 1),
    q97.5 = round(100*q97.5, 1),
    education = factor(education, levels = edu_levels)) |> 
  arrange(desc(reform2013), education, sector)

aces$reform2013_education_partner_education |> 
  mutate(
    mean = round(100*mean, 1),
    q2.5 = round(100*q2.5, 1),
    q97.5 = round(100*q97.5, 1),
    education = factor(education, levels = edu_levels),
    partner_education = factor(partner_education, levels = edu_levels)) |> 
  arrange(desc(reform2013), education, partner_education) |> 
  print(n = 1000)

samples$reform2013_education_partner_education |> 
  group_by(across(-c(iter, y0, y1))) |> 
  summarise(
    mean = mean((y1 - y0) / y0), 
    q2.5 = quantile((y1 - y0) / y0, 0.025),
    q97.5 = quantile((y1 - y0) / y0, 0.975),
    .groups = "drop")