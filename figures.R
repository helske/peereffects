library(dplyr)
library(ggplot2)

edu_levels <- c("Basic", "Upper secondary", "Lower tertiary", "Higher tertiary")
timegap_levels <- c("0-3", "3-6", "6-9", "9-12", "12-15", "15-18", "18-21", "21-24", 
                    "24-27", "27-30", "30-33", "33-36", "36-39", "39-42", "42-45", 
                    "45-48", "48+")
#dev.new(width = 180, height = 120, unit = "mm", noRstudioGD = TRUE)
theme_set(theme_minimal(base_size = 12))
theme_update(
  axis.text.y = element_text(size = 12), 
  axis.text.x = element_text(size = 12), 
  legend.text = element_text(size = 12),
  strip.text = element_text(size = 12),
  legend.position = "bottom"
)
load("causal_effects.rda")


alpha <- 0.5
colors <- c("#0072B2", "#E69F00")
# Figure of peer effect  E(leave | do(peer_leave = 1)) - E(leave |do(peer_leave = 0))
aces$month_year_Father |>
  mutate(
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    time = as.Date(paste0(year, "-", month, "-1"))
  )|> 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, fill = Father), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Father), linewidth = 0.8) +
  xlab("Time of birth") + ylab("Peer effect") + 
  scale_colour_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
ggsave("figures/peer_effect_by_father.png", width = 6.5, height = 4)

# Effect of past leaves
aces$past_leaves_Father |> 
  ungroup() |> 
  mutate(
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    past_leaves = ordered(past_leaves, labels = c(0:4, "5+"))
  ) |> 
  ggplot(aes(past_leaves, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_pointrange(
    aes(ymin = q2.5, ymax = q97.5, colour = Father), 
    position = position_dodge(0.3), size = 0.3
  ) +
  xlab("Number of quota users before the peer") + ylab("Peer effect") +
  scale_colour_manual(values = colors)

ggsave("figures/ace_past_leaves.png", width = 6.5, height = 4)

# Figure of peer effect  E(leave | do(peer_leave = 1)) - E(leave |do(peer_leave = 0))
aces$month_year_education_Father |>
  mutate(
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    education = ordered(education, levels = edu_levels),
    time = as.Date(paste0(year, "-", month, "-1"))
  )|> 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, fill = Father), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Father), linewidth = 0.8) +
  facet_wrap(~education) +
  xlab("Time of birth") + ylab("Peer effect") +
  scale_colour_manual(values = colors) + 
  scale_fill_manual(values = colors) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +  
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL)

ggsave("figures/peer_effect_by_father_and_education.png", width = 6.5, height = 6)

# Effect of past leaves
aces$past_leaves_education_Father |> 
  ungroup() |> 
  mutate(
    Father = ordered(Father, levels = c("First-time", "Experienced")),
    education = ordered(education, levels = edu_levels),
    past_leaves = ordered(past_leaves, labels = c(0:4, "5+"))
  ) |> 
  ggplot(aes(past_leaves, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_pointrange(
    aes(ymin = q2.5, ymax = q97.5, colour = Father), 
    position = position_dodge(0.3), size = 0.3
  ) +
  xlab("Number of quota users before the peer") + ylab("Peer effect") +
  scale_colour_manual(values = colors) +
  facet_wrap(~ education)
ggsave("figures/ace_past_leaves_and_education.png", width = 6.5, height = 6)

# Differences in peer effects
theme_update(axis.text.y = element_text(size = 12, hjust = 0))
pairwise_diff <- readRDS("pairwise_diff.rds")
pairwise_diff |> 
  ungroup() |> 
  mutate(
    xpos = 0.2,
    Father = factor(Father, labels = c("First-time fathers", "Experienced fathers")),
    nudge = 0.2 - 0.4 * (reform == "Before 2013 reform"),
    ypos = as.numeric(comparison) + nudge,
    P = paste0("P(\u0394 > 0) = ", format(round(P, 2), nsmall = 2))
  ) |> 
  # group_by(comparison, Father) |> 
  # mutate(xpos = mean(mean)) |> 
  ggplot(aes(mean, comparison)) + 
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_pointrange(aes(xmin = q2.5, xmax = q97.5, colour = reform),
                  position = position_dodge(0.3), size = 0.3) +
  facet_grid(~ Father) +
  paletteer::scale_colour_paletteer_d("suffrager::classic") + 
  ylab(NULL) + xlab("Percentage point difference \u0394 of the average peer effect") +
  theme(legend.title = element_blank()) +
  geom_text(aes(x = xpos, y = ypos, label = P), size = 3.5, hjust = 0)

ggsave("figures/peer_effect_difference.png", width = 10, height = 6)
