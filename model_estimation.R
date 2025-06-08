library(rstan)
library(data.table)
n_samples <- 2000
n_warmup <- 1000
chains <- 2
set.seed(1)
setwd("W:/JouniH/peereffects/May2025")
d <- readRDS("W:/JouniH/peereffects/May2025/data.rds") 

X <- d |>
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
X <- X[, -1]
(K <- ncol(X)) # 278
N <- nrow(X)
leave <- d$leave
idx_f <- which(d$first_birth == 1)
idx_e <- which(d$first_birth == 0)
peer_leave_f <-  d$peer_leave[idx_f]
peer_leave_e <-  d$peer_leave[idx_e]
time_index <- d$time
time_index <- time_index - min(time_index) + 1
T_ <- max(time_index)
time_index_f <- time_index[idx_f]
time_index_e <- time_index[idx_e]
timegap <- d$timegap_months
M <- length(levels(timegap))
timegap <- as.integer(timegap)
timegap_f <- timegap[idx_f]
timegap_e <- timegap[idx_e]
D <- 45
spline_basis_mu <- splines::bs(1:T_, df = D, intercept = TRUE)
past_leaves <- as.integer(d$past_leaves)
K_past <- length(unique(past_leaves))
past_leaves_f <- past_leaves[idx_f]
past_leaves_e <- past_leaves[idx_e]
workplace <- d$workplace_id
N_workplace <- max(workplace)
n_ncp_u <- sum(d[, ncp_u[1], by = workplace_id]$V1)
rm(d);gc()

model <- stan_model(
  "W:/JouniH/peereffects/May2025/model_final.stan",
  allow_optimizations = TRUE
)
fit <- sampling(
  model, 
  init = replicate(
    chains, 
    list(
      sigma_u = runif(1, 0.35, 0.45),
      u_ncp = rnorm(n_ncp_u, 0, 0.2), 
      u_cp = rnorm(N_workplace - n_ncp_u, 0, 0.1), 
      b_std = rnorm(K, 0, 0.1),
      omega_mu = rnorm(D, -0.3, 0.1), 
      tau_mu = runif(1, 0.05, 0.08),
      tau_peer = runif(1, 0.05, 0.1),
      b_timegap_f = runif(1, -0.1, 0.1),
      b_timegap_peer_f = runif(1, -0.5, -0.3),
      b_past_f = runif(1, 0, 0.2),
      b_past_peer_f = runif(1, -0.2, -0.1),
      b_timegap_e = runif(1, -0.1, 0.1),
      b_timegap_peer_f = runif(1, -0.5, -0.3),
      b_past_e = runif(1, 0, 0.2),
      b_past_peer_e = runif(1, -0.2, -0.1),
      simplex_timegap = rep(1/(M - 1), M - 1),
      simplex_timegap_peer = rep(1/(M - 1), M - 1),
      simplex_past = rep(1/(K_past - 1), K_past - 1),
      simplex_past_peer = rep(1/(K_past - 1), K_past - 1),
      tau_peer = runif(1, 0.1, 0.2),
      omega_peer = rnorm(D, 0, 0.1)
    ), 
    simplify = FALSE),
  data = tibble::lst(
    X, leave, N, K, N_workplace, workplace, peer_leave_f, peer_leave_e, time_index,
    T = T_, M, timegap_f, timegap_e, D, spline_basis_mu, 
    K_past, past_leaves_f, 
    past_leaves_e, n_ncp = n_ncp_u, time_index_f, time_index_e,
    idx_f, idx_e, N_f = length(idx_f), N_e = length(idx_e)
  ),
  chains = chains, cores = chains, refresh = 1,
  iter = n_samples + n_warmup, warmup = n_warmup,
  save_warmup = FALSE, include = FALSE,
  pars = c(
    "mu_std","u_ncp", "u_cp", "b_std", "omega_mu", 
    "b_timegap_f","b_timegap_e", "b_timegap_peer_f", 
    "b_timegap_peer_e", "b_past_e", "b_past_f", "b_past_peer_f", 
    "b_past_peer_e", "simplex_timegap", "simplex_timegap_peer", 
    "simplex_past", "simplex_past_peer"
  )
)
saveRDS(fit, file = "W:/JouniH/peereffects/May2025/fit_model_final.rds")
l <- loo::loo(fit)
saveRDS(l, file = "W:/JouniH/peereffects/May2025/loo_final.rds")
rm(fit);gc()

loo_new_D15 <- readRDS("W:/JouniH/peereffects/May2025/loo_new_D15.rds")
loo_new3_D15 <- readRDS("W:/JouniH/peereffects/May2025/loo_new3_D15.rds")
loo_final1 <- readRDS("W:/JouniH/peereffects/May2025/loo_final1.rds")
loo::loo_compare(l, loo_final1, loo_new3_D15, loo_new_D15)
