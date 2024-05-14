data {
  int<lower=1> N;
  array[N] int<lower=0,upper=1> leave;
  vector[N] peer_leave;
  int N_workplace;
  array[N] int<lower=0,upper=N_workplace> workplace;
  int K;
  matrix[N, K] X;
  int T;
  array[N] int<lower=1,upper=T> time_index;
  int M;
  array[N] int<lower=1,upper=M> timegap;
  int K_past;
  array[N] int<lower=1,upper=K_past> past_leaves;
  int D_mu;
  matrix[T, D_mu] basis_mu; // spline basis for the intercept mu
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
  vector[N_workplace] u_std;
  // standardised regression coefficients
  vector[K] b_std;
  // time-varying common intercept as a spline
  vector[D_mu] omega_mu;
  real<lower=0> tau_mu;
  // monotonic effect of timegap
  simplex[M - 1] simplex_timegap;
  real b_timegap;
  // conditionally monotonic effect of past leaves
  real b_past;
  real b_past_peer;
  simplex[K_past - 1] simplex_past;
  simplex[K_past - 1] simplex_past_peer;
}
transformed parameters {
  vector[M] beta_timegap = b_timegap * append_row(0, cumulative_sum(simplex_timegap));
  vector[K_past] beta_past = b_past * append_row(0, cumulative_sum(simplex_past));
  vector[K_past] beta_past_peer = b_past_peer * append_row(0, cumulative_sum(simplex_past_peer));
  vector[T] mu_std = 
  basis_mu * cumulative_sum(append_row(2 * omega_mu[1], tau_mu * omega_mu[2:D_mu]));
}

model {
  b_timegap ~ std_normal();
  b_past_peer ~ std_normal();
  b_past ~ std_normal();
  tau_mu ~ std_normal();
  omega_mu ~ std_normal();
  sigma_u ~ std_normal();
  u_std ~ std_normal();
  b_std ~ std_normal();
  leave ~ bernoulli_logit_glm(Q, 
    mu_std[time_index] + sigma_u * u_std[workplace] + beta_past[past_leaves] + 
    (beta_past_peer[past_leaves] + beta_timegap[timegap]) .* peer_leave, 
    b_std);
  
  
}
generated quantities {
  vector[K] beta = R_inv * b_std;
  vector[T] mu = mu_std - dot_product(X_means, beta);
}
