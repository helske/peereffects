library(dplyr)
## Return samples from P(Y|do(Peer))

sample_py <- function(samples, d, nsim) {
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
  ydox0 <- ydox1 <- matrix(NA, nrow(d), nsim)
  for(j in 1:nsim) {
    y <- mu[j, time_index] + u[j, workplace]
    y[idx_f] <- y[idx_f] + beta_past_f[j, past_leaves_f] + beta_timegap_f[j, timegap_f]
    y[idx_e] <- y[idx_e] + beta_past_e[j, past_leaves_e] + beta_timegap_e[j, timegap_e]
    y0 <- y + X0 %*% beta[j, ]
    y1 <- y + X1 %*% beta[j, ]
    y1[idx_f] <- y1[idx_f] + beta_timegap_peer_f[j, timegap_f] + beta_past_peer_f[j, past_leaves_f]
    y1[idx_e] <- y1[idx_e] + beta_timegap_peer_e[j, timegap_e] + beta_past_peer_e[j, past_leaves_e]
    ydox0[, j] <- inv_logit(y0)
    ydox1[, j] <- inv_logit(y1)
  }
  list(y_peer0 = ydox0, y_peer1 = ydox1)
}

d <- readRDS("data.rds")

fit <- readRDS("fit_model.rds")
samples <- rstan::extract(fit, pars = c("y_rep", "log_lik"), include = FALSE)
rm(fit);gc()
py_samples <- sample_py(samples, d, nsim = 500)
rm(samples);gc()
saveRDS(py_samples, file = "py_samples.rds")

res <- data.frame(
  peer_effect = c(py_samples$y_peer1 - py_samples$y_peer0),
  education = d$education,
  first_birth = d$first_birth
)
saveRDS(res, file = "individual_effects.rds")
