library(dplyr)
library(ggplot2)
library(paletteer)
load("causal_effects.rda")

aces$parity
aces$before_2013_reform_parity


aces$year_education_parity %>% 
  ungroup() %>% 
  filter(year == 2010 & parity == "0")

aces$year_education_parity %>% 
  ungroup() %>% 
  filter(year == 2017 & parity == "0")

ace_diff_parity_edu  %>% 
  ungroup() %>% 
  mutate(across(2:4, \(x) round(100*x, 1)))


interventional_means %>% 
  ungroup() %>% 
  filter(year == 2012 & month == 6 & parity == "0") %>% 
  filter(Peer == "quota")

interventional_means %>% 
  ungroup() %>% 
  filter(year == 2013 & month == 6 & parity == "0") %>% 
  filter(Peer == "quota")

edu_levels <- c("Basic", "Upper Secondary", "Lower tertiary", "Higher tertiary")
timegap_levels <- c("0-3", "3-6", "6-9", "9-12", "12-15", "15-18", "18-21", "21-24", 
                   "24-27", "27-30", "30-33", "33-36", "36-39", "39-42", "42-45", 
                   "45-48", "48+")
dev.new(width = 180, height = 120, unit = "mm", noRstudioGD = TRUE)
theme_set(theme_minimal(base_size = 12))


alpha <- 0.25
# Figure of peer effect  E(leave | do(peer_leave = 1)) - E(leave |do(peer_leave = 0))
aces$month_year_education_parity %>% filter(parity == "0") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Time") + ylab("Peer effect") +
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt"))
ggsave("figures/peer_effect_first_birth_bw.png",
  width = 180, height = 120, unit = "mm", dpi = 300)

# Figure of peer effect  E(leave | do(peer_leave = 1)) - E(leave |do(peer_leave = 0))
aces$month_year_education_parity %>% filter(parity == "0") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = alpha, fill = "#990099FF") +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(linewidth = 0.5, colour = "#990099FF") +
  facet_wrap(~ education) + xlab("Time") + ylab("Peer effect") +
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt"))
ggsave("figures/peer_effect_first_birth_colors.png",
  width = 180, height = 120, unit = "mm", dpi = 300)

# Figure with E(leave | do(peer_leave = 1)) and E(leave |do(peer_leave = 0)) ####

interventional_means %>% filter(parity == "0") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, 
                  fill = Peer), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Peer), linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Time") + ylab("E(quota | do(peer))") +
  scale_fill_grey() + 
  scale_colour_grey(end = 0.5) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt")) +
  ylim(c(0.1, 0.8))
ggsave("figures/expected_values_first_birth_bw.png",
       width = 180, height = 120, unit = "mm", dpi = 300)

interventional_means %>% filter(parity == "0") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, 
                  fill = Peer), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Peer), linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Time") + ylab("E(quota | do(peer))") +
  scale_fill_paletteer_d("suffrager::classic") + 
  scale_colour_paletteer_d("suffrager::classic") + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt")) +
  ylim(c(0.1, 0.8))
ggsave("figures/expected_values_first_birth_colors.png",
       width = 180, height = 120, unit = "mm", dpi = 300)


aces$past_leaves_education_parity %>% 
  filter(parity == "0") %>% 
  ungroup() %>% 
  mutate(education = ordered(education, levels = edu_levels),
         past_leaves = ordered(
           past_leaves,
           labels = c("0", "1", "2-4", "5+"))) %>% 
  ggplot(aes(past_leaves, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.25) +
  geom_pointrange(aes(ymin = q2.5, ymax = q97.5)) +
  xlab("Number of quota users before the peer") + ylab("Peer effect") +
  facet_wrap(~education)
ggsave("figures/ace_earlier_first_birth_bw.png",
       width = 180, height = 120, unit = "mm", dpi = 300)

aces$past_leaves_education_parity %>% 
  filter(parity == "0") %>% 
  ungroup() %>% 
  mutate(education = ordered(education, levels = edu_levels),
         past_leaves = ordered(
           past_leaves,
           labels = c("0", "1", "2-4", "5+"))) %>% 
  ggplot(aes(past_leaves, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.25) +
  geom_pointrange(aes(ymin = q2.5, ymax = q97.5), colour = "#990099FF") +
  xlab("Number of quota users before the peer") + ylab("Peer effect") +
  facet_wrap(~education)
ggsave("figures/ace_earlier_first_birth_colors.png",
       width = 180, height = 120, unit = "mm", dpi = 300)


## Figures for appendix

ace_magnitude_prob %>% filter(parity == "0") %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(y = probability, x = year, fill = between)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~education) +
  scale_fill_grey(start = 0.05, end = 0.95)+
  theme_minimal()
ggsave("figures/ace_magnitude_first_birth_bw.png",
  width = 180, height = 120, unit = "mm", dpi = 300)


ace_magnitude_prob %>% filter(parity == "0") %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(y = probability, x = year, fill = between)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~education) +
  scale_fill_paletteer_d("rcartocolor::PurpOr") + 
  theme_minimal()
ggsave("figures/ace_magnitude_first_birth_colors.png",
  width = 180, height = 120, unit = "mm", dpi = 300)


aces$timegap_education_parity %>% 
  filter(parity == "0") %>% 
  mutate(education = ordered(education, levels = edu_levels),
         timegap = ordered(timegap, levels = timegap_levels)) %>% 
  ggplot(aes(timegap, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.25) +
  geom_pointrange(aes(ymin = q2.5, ymax = q97.5)) +
  xlab("Time gap") + ylab("Peer effect") +
  facet_wrap(~education)
ggsave("figures/ace_by_timegap_first_birth.png",
       width = 180, height = 120, unit = "mm", dpi = 300)


aces$timegap_education_parity %>% 
  filter(parity == "1+") %>% 
  mutate(education = ordered(education, levels = edu_levels),
         timegap = ordered(timegap, levels = timegap_levels)) %>% 
  ggplot(aes(timegap, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.25) +
  geom_pointrange(aes(ymin = q2.5, ymax = q97.5)) +
  xlab("Time gap") + ylab("Peer effect") +
  facet_wrap(~education)
ggsave("figures/ace_by_timegap_experienced.png",
       width = 180, height = 120, unit = "mm", dpi = 300)

ace_magnitude_prob %>% filter(parity == "1+") %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(y = probability, x = year, fill = between)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~education) +
  scale_fill_grey(start = 0.05, end = 0.95)+
  theme_minimal()
ggsave("figures/ace_magnitude_experienced_bw.png",
  width = 180, height = 120, unit = "mm", dpi = 300)


ace_magnitude_prob %>% filter(parity == "1+") %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(y = probability, x = year, fill = between)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap(~education) +
  scale_fill_paletteer_d("rcartocolor::PurpOr") + 
  theme_minimal()
ggsave("figures/ace_magnitude_experienced_colors.png",
  width = 180, height = 120, unit = "mm", dpi = 300)

alpha <- 0.25
# Figure of peer effect  E(leave | do(peer_leave = 1)) - E(leave |do(peer_leave = 0))
aces$month_year_education_parity %>% filter(parity == "1+") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Time") + ylab("Peer effect") +
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt"))
ggsave("figures/peer_effect_experienced_bw.png",
  width = 180, height = 120, unit = "mm", dpi = 300)

# Figure of peer effect  E(leave | do(peer_leave = 1)) - E(leave |do(peer_leave = 0))
aces$month_year_education_parity %>% filter(parity == "1+") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), alpha = alpha, fill = "#990099FF") +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(linewidth = 0.5, colour = "#990099FF") +
  facet_wrap(~ education) + xlab("Time") + ylab("Peer effect") +
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt"))
ggsave("figures/peer_effect_experienced_colors.png",
  width = 180, height = 120, unit = "mm", dpi = 300)

# Figure with E(leave | do(peer_leave = 1)) and E(leave |do(peer_leave = 0)) ####

interventional_means %>% filter(parity == "1+") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, 
                  fill = Peer), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Peer), linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Time") + ylab("E(quota | do(peer))") +
  scale_fill_grey() + 
  scale_colour_grey(end = 0.5) +
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt")) +
  ylim(c(0.1, 0.8))
ggsave("figures/expected_values_experienced_bw.png",
       width = 180, height = 120, unit = "mm", dpi = 300)

interventional_means %>% filter(parity == "1+") %>% 
  mutate(time = as.Date(paste0(year, "-", month, "-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, 
                  fill = Peer), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Peer), linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Time") + ylab("E(quota | do(peer))") +
  scale_fill_paletteer_d("suffrager::classic") + 
  scale_colour_paletteer_d("suffrager::classic") + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt")) +
  ylim(c(0.1, 0.8))
ggsave("figures/expected_values_experienced_colors.png",
       width = 180, height = 120, unit = "mm", dpi = 300)


aces$past_leaves_education_parity %>% 
  filter(parity == "1+") %>% 
  ungroup() %>% 
  mutate(education = ordered(education, levels = edu_levels),
         past_leaves = ordered(
           past_leaves,
           labels = c("0", "1", "2-4", "5+"))) %>% 
  ggplot(aes(past_leaves, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.25) +
  geom_pointrange(aes(ymin = q2.5, ymax = q97.5)) +
  xlab("Number of quota users before the peer") + ylab("Peer effect") +
  facet_wrap(~education)
ggsave("figures/ace_earlier_experienced_bw.png",
       width = 180, height = 120, unit = "mm", dpi = 300)

aces$past_leaves_education_parity %>% 
  filter(parity == "1+") %>% 
  ungroup() %>% 
  mutate(education = ordered(education, levels = edu_levels),
         past_leaves = ordered(
           past_leaves,
           labels = c("0", "1", "2-4", "5+"))) %>% 
  ggplot(aes(past_leaves, mean)) + 
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.25) +
  geom_pointrange(aes(ymin = q2.5, ymax = q97.5), colour = "#990099FF") +
  xlab("Number of quota users before the peer") + ylab("Peer effect") +
  facet_wrap(~education)
ggsave("figures/ace_earlier_experienced_colors.png",
       width = 180, height = 120, unit = "mm", dpi = 300)

yearly_interventional_means %>% filter(parity == "0") %>% 
  mutate(time = as.Date(paste0(year, "-1-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, 
                  fill = Peer), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Peer), linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Year") + ylab("E(quota | do(peer))") +
  scale_fill_paletteer_d("suffrager::classic") + 
  scale_colour_paletteer_d("suffrager::classic") + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt")) +
  ylim(c(0.1, 0.8))
ggsave("figures/yearly_expected_values_fb_colors.png",
       width = 180, height = 120, unit = "mm", dpi = 300)

yearly_interventional_means %>% filter(parity == "1+") %>% 
  mutate(time = as.Date(paste0(year, "-1-1"))) %>% 
  mutate(education = ordered(education, levels = edu_levels)) %>% 
  ggplot(aes(time, mean)) + 
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5, 
                  fill = Peer), alpha = alpha) +
  geom_vline(xintercept = as.Date("2013-01-01"), linetype = "dashed", 
             linewidth = 0.25) +
  geom_line(aes(colour = Peer), linewidth = 0.5) +
  facet_wrap(~ education) + xlab("Year") + ylab("E(quota | do(peer))") +
  scale_fill_paletteer_d("suffrager::classic") + 
  scale_colour_paletteer_d("suffrager::classic") + 
  scale_x_date(date_breaks = "years", date_labels = "%Y", minor_breaks = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.x = unit(25, unit = "pt")) +
  ylim(c(0.1, 0.8))
ggsave("figures/yearly_expected_values_experienced_colors.png",
       width = 180, height = 120, unit = "mm", dpi = 300)
