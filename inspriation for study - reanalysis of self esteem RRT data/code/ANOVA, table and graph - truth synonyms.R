# title: summary table and plot
# author: Ian Hussey (ian.hussey@ugent.be)
# license: GPLv3+ 


# dependencies ------------------------------------------------------------


library(tidyverse)
library(schoRsch)
library(ez)
library(gridExtra)  # for multiplots
library(cowplot)  # for multiplots
library(effsize)


# data acquisition --------------------------------------------------------


setwd("/Users/Ian/Dropbox/Work/Projects/Suicide and self-harm/RRT self-esteem CC/Processed data/Inducer trials/")

data <- 
  read.csv("processed data - inducer trials.csv") %>%
  mutate(participant = as.factor(participant))

data_outliers_removed <-
  data %>%
  schoRsch::outlier(dv = "rt", 
                    todo="elim", 
                    upper.z = 2.5, 
                    lower.z = -2.5)

# check that what should be factors are indeed factors
sapply(data, class)


# plots -------------------------------------------------------------------


# raw 
plot(density(data$rt), col = "red")
lines(density(data_outliers_removed$rt), col = "blue")
# rescaled
plot(density(data_outliers_removed$rt), col = "blue")


# mean latencies ----------------------------------------------------------


summary_table <- 
  data_outliers_removed %>%
  select(stimulus, rt) %>%
  group_by(stimulus) %>%
  summarize(M = round(mean(rt), 0),
            SD = round(sd(rt), 0),
            n = n(),
            CI = round(1.96*SD/sqrt(n), 0),  # calculate 95% confidence intervals
            CI_lower = M - CI,
            CI_upper = M + CI) %>%
  ungroup() %>%
  arrange(M) %>%
  rownames_to_column() %>%
  rename(rank = rowname) %>%
  mutate(rank = as.numeric(rank)) %>%
  select(-n)

summary_table %>% write.csv("analysis/summary table - truth.csv", row.names = FALSE)


# plot --------------------------------------------------------------------


# sort x axis by other variable (rank). solution here, don't understand why it works though:
# http://stackoverflow.com/a/3744432
summary_table <- transform(summary_table, stimulus = reorder(stimulus, -rank)) 

# true synonyms
summary_table_true <- 
  summary_table %>%
  filter(stimulus == "Goed" |  # Good
           stimulus == "Juist" |  # just
           stimulus == "Correct" |  # correct
           stimulus == "Exact" |  # exact
           stimulus == "In orde")

# true plot
true_plot <-
  ggplot(summary_table_true, 
       aes(x = stimulus, y = M)) +
  theme_minimal() +
  scale_y_continuous(name = "Mean RT", limits = c(1300, 1600), breaks = seq(1300, 1600, 50), minor_breaks = NULL) +
  xlab("True synonym") +
  geom_point(size = 2, shape = 15) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) + # error bars are 95% CIs
  coord_flip()


# false synonyms
summary_table_false <- 
  summary_table %>%
  filter(stimulus == "Mis" |  # wrong
           stimulus == "Onjuist" |  # false
           stimulus == "Incorrect" |  # incorrect
           stimulus == "Verkeerd" |  # wrong
           stimulus == "Fout")

# false plot
false_plot <- 
ggplot(summary_table_false, 
       aes(x = stimulus, y = M)) +
  theme_minimal() +
  scale_y_continuous(name = "Mean RT", limits = c(1300, 1600), breaks = seq(1300, 1600, 50), minor_breaks = NULL) +
  xlab("False synonym") +
  geom_point(size = 2, shape = 15) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) + # error bars are 95% CIs
  coord_flip()

# combine plots
true_false_plot <- plot_grid(true_plot, false_plot, ncol = 1, nrow = 2, align = 'v')

# save
ggsave("plot - true-false synonyms.pdf",
       path = "analysis/",
       device = "pdf",
       plot = last_plot(),
       width = 10,
       height = 10,
       units = "cm")


# analyses ----------------------------------------------------------------


# 1 synonym category differences 
data_outliers_removed <- 
  data_outliers_removed %>%
  mutate(stimulus_category = as.factor(ifelse(stimulus == "Goed" |  # Good
                                                stimulus == "Juist" |  # just
                                                stimulus == "Correct" |  # correct
                                                stimulus == "Exact" |  # exact
                                                stimulus == "In orde",
                                              "true",
                                              ifelse(stimulus == "Mis" |  # wrong
                                                       stimulus == "Onjuist" |  # false
                                                       stimulus == "Incorrect" |  # incorrect
                                                       stimulus == "Verkeerd" |  # wrong
                                                       stimulus == "Fout", 
                                                     "false",
                                                     NA))))

# t test
t_test_1 <- t.test(formula = rt ~ stimulus_category,
                   data = data_outliers_removed,
                   paired = FALSE)

cohens_d_1 <- cohen.d(formula = rt ~ stimulus_category,
                      data = data_outliers_removed,
                      paired = FALSE)

# save
sink("analysis/ttest true vs false.txt")
t_test_1
cohens_d_1
sink()


# 2 true synonym differences
data_outliers_removed_true <- 
  data_outliers_removed %>%
  filter(stimulus == "Goed" |  # Good
           stimulus == "Juist" |  # just
           stimulus == "Correct" |  # correct
           stimulus == "Exact" |  # exact
           stimulus == "In orde")

# within group anova comparing rts between stimuli exemplars 
anova_1 <- ez::ezANOVA(data = data_outliers_removed_true,
                       dv = rt,
                       within = stimulus,
                       wid = participant,
                       type = 3,
                       detailed = TRUE)

# summarise output
anova_1_summary <- schoRsch::anova_out(anova_1, 
                                       print = TRUE, 
                                       sph.cor = "GG", 
                                       mau.p = 0.05,
                                       etasq = "partial", 
                                       dfsep = ", ")

# save
sink("analysis/ANOVA true synonyms.txt")
anova_1_summary
sink()


# 3 false synonym differences
data_outliers_removed_false <- 
  data_outliers_removed %>%
  filter(stimulus == "Mis" |  # wrong
           stimulus == "Onjuist" |  # false
           stimulus == "Incorrect" |  # incorrect
           stimulus == "Verkeerd" |  # wrong
           stimulus == "Fout")

# within group anova comparing rts between stimuli exemplars 
anova_2 <- ez::ezANOVA(data = data_outliers_removed_false,
                       dv = rt,
                       within = stimulus,
                       wid = participant,
                       type = 3,
                       detailed = TRUE)

# summarise output
anova_2_summary <- schoRsch::anova_out(anova_2, 
                                       print = TRUE, 
                                       sph.cor = "GG", 
                                       mau.p = 0.05,
                                       etasq = "partial", 
                                       dfsep = ", ")


# save
sink("analysis/ANOVA false synonyms.txt")
anova_2_summary
sink()

