library(dplyr)
library(ggplot2)
library(paletteer)

edu_levels <- c("Basic", "Upper secondary", "Lower tertiary", "Higher tertiary")
timegap_levels <- c("0-3", "3-6", "6-9", "9-12", "12-15", "15-18", "18-21", "21-24", 
                    "24-27", "27-30", "30-33", "33-36", "36-39", "39-42", "42-45", 
                    "45-48", "48+")
theme_set(theme_minimal(base_size = 12))
theme_update(
  axis.text.y = element_text(size = 12), 
  legend.text = element_text(size = 12),
  strip.text = element_text(size = 12),
  legend.position = "bottom",
  axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
)

samples <- readRDS("causal_effect_samples.rds")
samples <- samples$education_Father |> 
  mutate(
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    peer_effect = y1 - y0,
    education = ordered(education, levels = edu_levels)
  )
colors <- c("#0072B2", "#E69F00")
samples |>  
  ggplot(aes(peer_effect)) +
  geom_density(aes(fill = Father), colour = NA, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
  facet_wrap(~education) +
  xlab("Peer effect") + ylab("Density") +
  scale_colour_manual(values = colors) + 
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = seq(0, 0.12, by = 0.02))
ggsave("figures/expected_value_of_peer_effect_density.png",
       width = 6.5, height = 6)


res <- readRDS("individual_effects.rds")

res |>
  mutate(Father = ordered(first_birth==0, labels = c("First-time", "Experienced"))) |>  
  ggplot(aes(peer_effect)) +
  geom_density(aes(fill = Father), colour = NA, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
  facet_wrap(~education) +
  xlab("Peer effect") + ylab("Density") +
  scale_colour_manual(values = colors) + 
  scale_fill_manual(values = colors) +
  coord_cartesian(xlim = c(-0.4, 0.4))
ggsave("figures/individual_peer_effect_density.png", width = 6.5, height = 6)

# Robustness checks
load("causal_effects.rda")
aces_timegap <- readRDS("causal_effects_aces_timegap.rds")
aces_reform <- readRDS("causal_effects_aces_reform.rds")
aces_large_wps <- readRDS("causal_effects_aces_large_wps.rds")

d <- bind_rows(`After reform` = aces_reform$month_year_education_Father,
               `Main analysis` = aces$month_year_education_Father, 
               .id = "Data") |> 
  mutate(Data = ordered(Data, levels = c("Main analysis", "After reform")))


alpha <- 0.3
d |>  
  mutate(
    education = ordered(education, levels = edu_levels),
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    time = as.Date(paste0(year, "-", month, "-1"))
  )|>
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, fill = Data), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Data), linewidth = 0.8) +
  facet_grid(education ~ Father) +
  xlab("Time of birth") + ylab("Peer effect") +
  paletteer::scale_fill_paletteer_d("suffrager::classic") + 
  paletteer::scale_colour_paletteer_d("suffrager::classic") + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL)

ggsave("figures/from_2013_vs_full_data_peer_effect.png",
       width = 6.5, height = 6)

d <- bind_rows(`Timegap restriction` = aces_timegap$month_year_education_Father,
               `Main analysis` = aces$month_year_education_Father, 
               .id = "Data") |>
  mutate(Data = ordered(Data, levels = c("Main analysis", "Timegap restriction")))
d |>  
  mutate(
    education = ordered(education, levels = edu_levels),
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    time = as.Date(paste0(year, "-", month, "-1"))
  )|>
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, fill = Data), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Data), linewidth = 0.8) +
  facet_grid(education ~ Father) +
  xlab("Time of birth") + ylab("Peer effect") +
  paletteer::scale_fill_paletteer_d("suffrager::classic") + 
  paletteer::scale_colour_paletteer_d("suffrager::classic") + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL)

ggsave("figures/timegap_vs_full_data_peer_effect.png",
       width = 6.5, height = 6)

d <- bind_rows(`Large workplaces` = aces_large_wps$month_year_education_Father,
               `Main analysis` = aces$month_year_education_Father, 
               .id = "Data") |> 
  mutate(Data = ordered(Data, levels = c("Main analysis", "Large workplaces")))
d |>  
  mutate(
    education = ordered(education, levels = edu_levels),
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    time = as.Date(paste0(year, "-", month, "-1"))
  )|>
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, fill = Data), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Data), linewidth = 0.8) +
  facet_grid(education ~ Father) +
  xlab("Time of birth") + ylab("Peer effect") +
  # scale_colour_manual(values = colors) + 
  # scale_fill_manual(values = colors) + 
  paletteer::scale_fill_paletteer_d("suffrager::classic") + 
  paletteer::scale_colour_paletteer_d("suffrager::classic") + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL)

ggsave("figures/large_wps_vs_full_data_peer_effect.png",
       width = 6.5, height = 6)
