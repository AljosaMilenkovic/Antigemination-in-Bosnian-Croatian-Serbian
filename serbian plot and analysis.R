library(dplyr)
library(tidyr)
library(stringr)
library(lme4)
library(binom)
library(ggplot2)

sa_data_serbian <- read.csv("sa serbian.csv", header = TRUE)

#bar plot for ocp
sa_data_serbian %>%
  filter(!(context == "dzh" | context == "dj" | context == "sh" | context == "z" | context == "zh")) %>%
  mutate(agree_voi = if_else((context == "b" | context == "d" | context == "g"), "1", "0")) %>%
  mutate(agree_ant = if_else((context == "ch" | context == "tj"), "1", "0")) %>%
  mutate(no_gem = if_else((context == "s"), "1", "0")) %>%
  mutate(violation_profile = if_else((agree_voi == "1"), "Agree_voi",
                                     if_else((agree_ant == "1"), "Agree_ant",
                                             if_else((no_gem == "1"), "NoGem", "baseline")))) %>%
  group_by(realization,violation_profile) %>%
  summarise(count = n()) %>%
  group_by(violation_profile) %>%
  mutate(total = sum(count), percentage = count/total) %>%
  ggplot(aes(x = factor(violation_profile,
                        levels = c("baseline", "Agree_voi", "Agree_ant", "NoGem"),
                        labels = c("No viol.", "Agr_voi", "Agr_ant", "NoGem")),
             y = percentage,
             fill = realization)) +
  geom_bar(stat = "identity", width = .7, color = "black") +
  theme_bw() +
  scale_fill_grey(start = .7, end = .3) +
  labs(x = "Violation Profile", y = "Proportion of allomorphs", fill = "Realization") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) -> ocp_plot_serbian

#saves the plot
ggsave("ocp plot serbian.png", ocp_plot_serbian, dpi = 600, width = 6, height = 6)


#############
#logistic regression analysis
#constraint coding
regression_serbian <- sa_data_serbian %>%
  mutate(agree_voi = if_else((context == "b" | context == "d" | context == "g" | context == "z" | context == "zh" | context == "dzh" | context == "dj"), "1", "0")) %>%
  mutate(agree_ant = if_else((context == "ch" | context == "tj" | context == "zh" | context == "sh" | context == "dzh" | context == "dj"), "1", "0")) %>%
  mutate(no_gem = if_else((context == "s"| context == "z" | context == "zh" | context == "sh"), "1", "0")) 

#aggregarion by lemma
regression_serbian_aggregated <- regression_serbian %>%
  group_by(lemma, realization, agree_voi, agree_ant, no_gem) %>%
  summarise(frequency = n(), .groups = "drop") %>%
  pivot_wider(names_from = realization, values_from = frequency, values_fill = 0) %>%
  group_by(lemma, agree_voi, agree_ant, no_gem) %>%
  summarize(
    lemma = paste(unique(lemma), collapse = ", "),
    sa = sum(sa, na.rm = TRUE),
    s = sum(s, na.rm = TRUE),
    agree_voi = paste(unique(agree_voi), collapse = ", "),
    agree_ant = paste(unique(agree_ant), collapse = ", "),
    no_gem = paste(unique(no_gem), collapse = ", "),
    .groups = "drop") %>%
  arrange(desc(sa))

#fits the model
model_aggregated <- glm(cbind(sa,s) ~ agree_voi*agree_ant*no_gem, data = regression_serbian_aggregated, family = "binomial")  
#returns model output
summary(model_aggregated)