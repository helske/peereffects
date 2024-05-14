source("W:/JouniH/functions_for_stan/readcsv.R")
samples <- read_stan_csv_custom(
  paste0("W:/JouniH/peereffects/posterior_samples_", 1:4, ".csv")
)
library(posterior)
summarise_draws(samples$sigma_u)
summarise_draws(samples$tau_mu)
sumr <- summarise_draws(samples$beta)
range(sumr$ess_bulk)
range(sumr$ess_tail)
range(sumr$rhat)
sumr$variable <- colnames(X)
print(sumr[,c(1:2, 6,7)], n = 1000)
draws <- as_draws_df(samples)
rhats <- rhat(draws)
range(rhats)
bulk_ess <- ess_bulk(draws)
tail_ess <- ess_tail(draws)
rm(draws);gc()


x <- samples$beta_timegap
out <- data.frame(beta_timegap = c(draws_of(x)), iter = 1:ndraws(x), 
                  timegap = rep(ordered(levels(d$timegap_months), 
                                        levels = levels(d$timegap_months)), each = ndraws(x)))

sumr <- out %>% 
  group_by(timegap) %>% 
  summarise(mean = mean(beta_timegap), lwr = quantile(beta_timegap, 0.025), upr = quantile(beta_timegap, 0.975))

ggplot(sumr, aes(timegap, mean)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
   theme_minimal()


x <- samples$beta_past_peer

out <- data.frame(beta = c(draws_of(x)), iter = 1:ndraws(x), 
                  past_leaves = rep(ordered(levels(d$n_past_leave_takers), 
                                            levels = levels(d$n_past_leave_takers),
                                            labels = c("0", "1", "2-4", "5+")), each = ndraws(x)))
sumr <- out %>% 
  group_by(past_leaves) %>% 
  summarise(mean = mean(beta), lwr = quantile(beta, 0.025), upr = quantile(beta, 0.975))

ggplot(sumr, aes(past_leaves, mean)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
   theme_minimal()



x <- samples$beta_past

out <- data.frame(beta = c(draws_of(x)), iter = 1:ndraws(x), 
                  past_leaves = rep(ordered(levels(d$n_past_leave_takers), 
                                            levels = levels(d$n_past_leave_takers),
                                            labels = c("0", "1", "2-4", "5+")), each = ndraws(x)))
sumr <- out %>% 
  group_by(past_leaves) %>% 
  summarise(mean = mean(beta), lwr = quantile(beta, 0.025), upr = quantile(beta, 0.975))

ggplot(sumr, aes(past_leaves, mean)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr)) +
  theme_minimal()


