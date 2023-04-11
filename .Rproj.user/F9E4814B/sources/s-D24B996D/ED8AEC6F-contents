source(here::here("scripts", "00_libs.R"))
source(here("scripts", "01_helpers.R"))
source(here("scripts", "03_load_data.R"))


full_df %>%
  group_by(participant, segment, n_actual) %>%
  summarize(n = sum(sig_tost)) %>%
  ggplot(aes(x = as.factor(n_actual), y = as.numeric(n), fill = segment)) +
  geom_boxplot(outlier.size = 0) +
  ylab("Percentage") + xlab("Number of stimuli") +
  geom_hline(yintercept = 80, linetype = "dashed", alpha = .4) +
  theme_minimal() +
  theme(text = element_text(size = 20)) +
  theme(legend.position = "bottom") + ggsave(here("docs", "includes",
                                                  "figures", "tost.png"))

pct_chart = full_df %>%
  group_by(n_actual, segment) %>%
  summarize(n = round(sum(sig_tost)/1000, digits = 2)) %>%
  pivot_wider(names_from = segment,
              values_from = n)

library(xtable)
full_df %>%
  group_by(n_actual, segment) %>%
  summarize(n = round(sum(sig_tost)/1000, digits = 2)) %>%
  pivot_wider(names_from = segment,
              values_from = n) %>%
  xtable()

full_df %>%
  group_by(n_actual, segment) %>%
  summarize(n = round(sum(sig_ttest)/1000, digits = 2)) %>%
  pivot_wider(names_from = segment,
              values_from = n) %>%
  xtable()



full_df %>%
  group_by(participant, segment, n_actual) %>%
  summarize(n = sum(sig_ttest)) %>%
  ggplot(aes(x = as.factor(n_actual), y = as.numeric(n), fill = segment)) +
  geom_boxplot(outlier.size = 0) +
  ylab("No. false positive") + xlab("Number of stimuli") +
  geom_hline(yintercept = 5, linetype = "dashed", alpha = .4) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 20)) +
  ggsave(here("docs", "includes",
              "figures", "fp_tost.png"))


