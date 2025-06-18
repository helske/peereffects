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


intv_colors <- c("#008080", "#FF6F61")
interventional_means |> 
  mutate(
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    time = as.Date(paste0(year, "-", month, "-1")),
    education = ordered(education, levels = edu_levels)
  ) |> 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, 
                  fill = Peer, alpha = Father)) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Peer, alpha = Father), linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Time") + ylab("E(quota | do(peer))") +
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  scale_colour_manual(values = intv_colors) + 
  scale_fill_manual(values = intv_colors) + 
  scale_alpha_manual(values = c(0.6, 0.3), labels = c("No quota", "Quota")) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12)
  ) +
  guides(
    fill = guide_legend("Experienced", override.aes = list(alpha = 0.3, fill = intv_colors)),
    colour = guide_legend("Experienced"),
    alpha = guide_legend("First-time", override.aes = list(alpha = 0.6, fill = intv_colors))
  )
ggsave("figures/expected_values.png", width = 6.5, height = 6)

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

samples <- readRDS("causal_effect_samples.rds")

edu_levels <- c("Basic", "Upper secondary", "Lower tertiary", "Higher tertiary")

d <- samples$reform2013_education_Father |> 
  mutate(
    peer_effect = y1 - y0,
    education = factor(education, levels = edu_levels),
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    reform = factor(reform2013, levels = c("not eligible", "eligible"), 
                    labels = c("Before 2013 reform", "After 2013 reform"))
  ) |> 
  select(peer_effect, education, Father, reform, iter) |> 
  tidyr::pivot_wider(names_from = education, values_from = peer_effect)

edu_pairs <- combn(edu_levels, 2, simplify = FALSE)
pairwise_diff <- do.call(
  rbind, 
  lapply(
    edu_pairs, \(x) {
      data.frame(
        iter = d$iter,
        reform = d$reform,
        Father = d$Father,
        comparison = paste0(x[1], " vs.\n", x[2]),
        difference = d[[x[1]]] - d[[x[2]]],
        delta_pos = d[[x[1]]] > d[[x[2]]]
      )
    }
  )
) |> 
  group_by(reform, Father, comparison) |> 
  summarise(
    mean = 100 * mean(difference),
    q2.5 = 100 * quantile(difference, 0.025),
    q97.5 = 100 * quantile(difference, 0.975),
    P = mean(delta_pos)
  ) |> 
  mutate(
    comparison = factor(
      comparison, levels = sapply(edu_pairs, 
                                  \(x) paste0(x, collapse = " vs.\n"))
    )
  )

theme_set(theme_minimal(base_size = 12))
theme_update(
  axis.text.y = element_text(size = 12, hjust = 0), 
  legend.text = element_text(size = 12),
  strip.text = element_text(size = 12),
  legend.position = "bottom",
  axis.text.x = element_text(size = 12),
  panel.spacing.x = unit(10, "mm")
)

pairwise_diff |> 
  ungroup() |> 
  mutate(
    Father = factor(Father, 
                    labels = c("First-time fathers", "Experienced fathers")),
    xpos = 0.7 + mean,
    nudge = 0.2 - 0.4 * (reform == "Before 2013 reform"),
    ypos = as.numeric(comparison) + nudge,
    P = paste0("P(\u0394 > 0) = ", format(round(P, 2), nsmall = 2))
  ) |> 
  ggplot(aes(mean, comparison)) + 
  scale_x_continuous(breaks = seq(-4, 10, by = 2)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_pointrange(aes(xmin = q2.5, xmax = q97.5, colour = reform), 
                  position = position_dodge(0.2)) +
  facet_grid( ~ Father) +
  paletteer::scale_colour_paletteer_d("suffrager::classic") + 
  ylab(NULL) + xlab("Percentage point difference \u0394 of the average peer effect") +
  theme(legend.title = element_blank()) +
  geom_text(aes(x = xpos, y = ypos, label = P, hjust = positive), 
            size = 3.5, hjust = 0)

ggsave("figures/peer_effect_difference.png", width = 6.5, height = 6)