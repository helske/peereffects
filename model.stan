data {
  int K;
  int T;
  int M;
  int K_past;
  int<lower=1> N;
  int N_workplace;
  int N_f; // first-time fathers
  int N_e; // experienced fathers
  matrix[N, K] X;
  array[N_f] int idx_f;
  array[N_e] int idx_e;
  array[N] int<lower=0,upper=1> leave;
  array[N] int<lower=0,upper=N_workplace> workplace;
  array[N] int<lower=1,upper=T> time_index;
  array[N_f] int<lower=1,upper=T> time_index_f;
  array[N_e] int<lower=1,upper=T> time_index_e;
  vector[N_f] peer_leave_f;
  array[N_f] int<lower=1,upper=M> timegap_f;
  array[N_f] int<lower=1,upper=K_past> past_leaves_f;
  vector[N_e] peer_leave_e;
  array[N_e] int<lower=1,upper=M> timegap_e;
  array[N_e] int<lower=1,upper=K_past> past_leaves_e;
  int D;
  matrix[T, D] spline_basis_mu; // spline basis for the intercept mu
  int n_ncp; // number of workplaces using non-centered parameterization
  
}

transformed data {
  // Use QR decomposition for speeding up sampling
  vector[K] X_means;
  matrix[N, K] Q;
  matrix[K, K] R_inv;
  {
    matrix[N, K] X_c;
    for(k in 1:K) {
      X_means[k] = mean(X[, k]);
      X_c[, k] = X[, k] - X_means[k];
    }
    Q = qr_thin_Q(X_c) * sqrt(N - 1);
    R_inv = inverse(qr_thin_R(X_c) / sqrt(N - 1)); 
  }
}
parameters {
  // workplace-specific random intercepts
  real<lower=0> sigma_u;
  vector[n_ncp] u_ncp;
  vector[N_workplace - n_ncp] u_cp;
  // standardised regression coefficients
  vector[K] b_std;
  // time-varying common intercept as a spline
  vector[D] omega_mu;
  real<lower=0> tau_mu;
  // monotonic effect of timegap
  real b_timegap_f;
  real b_timegap_e;
  real b_timegap_peer_f;
  real b_timegap_peer_e;
  // monotonic effect of past leaves
  real b_past_e;
  real b_past_f;
  // monotonic effect of peer and past leaves
  real b_past_peer_f;
  real b_past_peer_e;
  
  simplex[M - 1] simplex_timegap;
  simplex[M - 1] simplex_timegap_peer;
  simplex[K_past - 1] simplex_past;
  simplex[K_past - 1] simplex_past_peer;
}
transformed parameters {
  vector[N_workplace] u = append_row(sigma_u * u_ncp, u_cp);
  vector[T] mu_std = spline_basis_mu * omega_mu;
  vector[M] beta_timegap_f;
  vector[M] beta_timegap_e;
  vector[M] beta_timegap_peer_f;
  vector[M] beta_timegap_peer_e;
  vector[K_past] beta_past_f;
  vector[K_past] beta_past_e;
  vector[K_past] beta_past_peer_f;
  vector[K_past] beta_past_peer_e;
  {
    vector[M] tmp = append_row(0, cumulative_sum(simplex_timegap_peer));
    beta_timegap_peer_f = b_timegap_peer_f * tmp;
    beta_timegap_peer_e = b_timegap_peer_e * tmp;
    tmp = append_row(0, cumulative_sum(simplex_timegap));
    beta_timegap_f = b_timegap_f * tmp;
    beta_timegap_e = b_timegap_e * tmp;
  }
  {
    vector[K_past] tmp = append_row(0, cumulative_sum(simplex_past));
    beta_past_f = b_past_f * tmp;
    beta_past_e = b_past_e * tmp;
    tmp = append_row(0, cumulative_sum(simplex_past_peer));
    beta_past_peer_f = b_past_peer_f * tmp;
    beta_past_peer_e = b_past_peer_e * tmp;
  }
}

model {
  b_std ~ std_normal();
  b_timegap_f ~ std_normal();
  b_timegap_e ~ std_normal();
  b_timegap_peer_f ~ std_normal();
  b_timegap_peer_e ~ std_normal();
  b_past_peer_f ~ std_normal();
  b_past_f ~ std_normal();
  b_past_peer_e ~ std_normal();
  b_past_e ~ std_normal();
  tau_mu ~ std_normal();
  omega_mu[1] ~ normal(0, 2);
  omega_mu[2:D] ~ normal(omega_mu[1:(D - 1)], tau_mu);
  sigma_u ~ std_normal();
  u_ncp ~ std_normal();
  u_cp ~ normal(0, sigma_u);
  {
    vector[N] eta = mu_std[time_index] + u[workplace];
    // main effects of past leave takers and timegap
    eta[idx_f] += beta_past_f[past_leaves_f] + beta_timegap_f[timegap_f] +
    // interaction effects of peer and past leaves and peer and timegap, as well as time-varying main effect of peer
    (beta_timegap_peer_f[timegap_f] + beta_past_peer_f[past_leaves_f]) .* peer_leave_f;
    // same for experienced fathers
    eta[idx_e] += beta_past_e[past_leaves_e] + beta_timegap_e[timegap_e] + 
    (beta_timegap_peer_e[timegap_e] + beta_past_peer_e[past_leaves_e]) .* peer_leave_e;
    leave ~ bernoulli_logit_glm(Q, eta, b_std);
  }
}
generated quantities {
  vector[K] beta = R_inv * b_std;
  vector[T] mu = mu_std - dot_product(X_means, beta);
  vector[N] log_lik;
  {
    vector[N] logit_p = Q * b_std + mu_std[time_index] + u[workplace];
    logit_p[idx_f] += beta_past_f[past_leaves_f] + beta_timegap_f[timegap_f] + 
    (beta_timegap_peer_f[timegap_f] + beta_past_peer_f[past_leaves_f]) .* peer_leave_f;
    logit_p[idx_e] += beta_past_e[past_leaves_e] + beta_timegap_e[timegap_e] + 
    (beta_timegap_peer_e[timegap_e] + beta_past_peer_e[past_leaves_e]) .* peer_leave_e;
    for (i in 1:N) {
      log_lik[i] = bernoulli_logit_lpmf(leave[i] | logit_p[i]);
    }
  }
}
