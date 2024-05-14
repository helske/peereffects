library(dplyr)
library(ggplot2)
library(rstan)
n_samples <- 2500
n_warmup <- 2500
chains <- 1
chain_id <- 1L
set.seed(chain_id)
library(mockthat)
local_mock(`rstan:::is_dir_writable` = function(path) TRUE, mock_env = "rstan")

d <- readRDS("W:/JouniH/peereffects/data.rds") %>% 
  filter(reform2013 == "eligible") %>% droplevels()

X <- d %>%
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
      # reform2013 * income_decile +
      # reform2013 * education +
      # reform2013 * industry + 
      before_2010reform_lag +
      before_2013reform_lag +
      # for those who have earlier children
      own_previous_birth_peer_timing * education +
      (#reform2013 +
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
# reorder columns so that columns related to peer_leave are last
X <- X[, -1]
peer_idx <- grep("peer_leave", colnames(X))
K_peer <- length(peer_idx) 
X <- cbind(X[, -peer_idx], X[, peer_idx])
K <- ncol(X)
N <- nrow(X)
leave <- d %>% pull(leave)
peer_leave <-  d %>% pull(peer_leave)
workplace <- as.integer(factor(d %>% pull(workplace_id)))
N_workplace <- length(unique(workplace))
time_index <- d %>% pull(time)
time_index <- time_index - min(time_index) + 1
T_ <- max(time_index)
timegap <- d %>% pull(timegap_months)
M <- length(levels(timegap))
timegap <- as.integer(timegap)
D_mu <- 45
basis_mu <- splines::bs(1:T_, df = D_mu, intercept = TRUE)
past_leaves <- as.integer(d %>% pull(n_past_leave_takers))
K_past <- length(unique(past_leaves))
rm(d);gc()

model <- stan_model(
  "W:/JouniH/peereffects/model.stan",
  allow_optimizations = TRUE
)
fit <- sampling(
  model, 
  init = replicate(
    chains, 
    list(
      sigma_u = runif(1, 0.45, 0.55),
      u_std = rnorm(N_workplace, 0, 0.1), 
      b_std = rnorm(K, 0, 0.1),
      omega_mu = rnorm(D_mu, 0, 0.5), 
      tau_mu = runif(1, 0.04, 0.5),
      b_timegap = runif(1, -0.5, -0.3),
      b_past = runif(1, 0, 0.2),
      b_past_peer = runif(1, -0.3, -0.1)
    ), 
    simplify = FALSE),
  data = tibble::lst(
    X, leave, N, K, N_workplace, workplace, peer_leave, time_index,
    T = T_, M, timegap, D_mu, basis_mu, K_past, past_leaves
  ),
  chains = chains, cores = chains, refresh = 10,
  iter = n_samples + n_warmup, warmup = n_warmup, 
  control = list(adapt_delta = 0.95),
  save_warmup = FALSE, include = FALSE,
  sample_file = paste0("W:/JouniH/peereffects/posterior_samples_from_2013_", chain_id, ".csv"),
)
saveRDS(fit, file = paste0("W:/JouniH/peereffects/fit_model_from_2013_", chain_id, ".rds")) 
rm(fit);gc()
