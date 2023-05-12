source('fsl_glm_matrices.R')
# using tables generated from MatLab tbss_means
matlab_folder <- "/Volumes/Research/lazarm03lab/labspace/AD/camcan995/derivatives/MatLab/tbss_means"
csv_files <- list.files(matlab_folder, ".csv$")
file.copy(file.path(matlab_folder, csv_files), getwd())

library(tidyverse)
library(gridExtra)
library(ggpmisc)
library(officer)

ismrm_plots_ppts <- read_pptx()
layout_summary(ismrm_plots_ppts)


scd_table <- read_csv('scd_table.csv')
scd_table$cohort <- 'scd'
names(scd_table) <- sub('scd_', '', names(scd_table))
scd_table_l_lower_cingulum <- read_csv('scd_table_l_lower_cingulm.csv')
scd_table_l_lower_cingulum$cohort <- 'scd'
names(scd_table_l_lower_cingulum) <- sub('scd_', '', names(scd_table_l_lower_cingulum))
ctl_table <- read_csv('ctl_table.csv')
ctl_table$cohort <- 'ctl'
names(ctl_table) <- sub('ctl_', '', names(ctl_table))
df <- rbind(ctl_table, scd_table)
ctl_table_l_lower_cingulum <- read_csv('ctl_table_l_lower_cingulum.csv')
ctl_table_l_lower_cingulum$cohort <- 'ctl'
names(ctl_table_l_lower_cingulum) <- sub('ctl_', '', names(ctl_table_l_lower_cingulum))
df_l_lower_cingulum <- rbind(ctl_table_l_lower_cingulum, scd_table_l_lower_cingulum)
df <- cbind(df, df_l_lower_cingulum)
duplicated_columns <- duplicated(colnames(df))
df <- df[!duplicated_columns]
df <- df %>% relocate(cohort, .after = last_col())

# add age and story_d
age_mat <- read_tsv('glm/age_mat.txt', col_names = FALSE)
df$age <- age_mat$X2
story_d_mat <- read_tsv('glm/story_d_mat.txt', col_names = FALSE)
df$story_d <- story_d_mat$X2

FA_r_lower_cingulum_scd_story_d <- lm(mean_FA_r_lower_cingulum_mask ~ story_d * cohort, df)
FA_r_lower_cingulum_scd_story_d_age <- lm(mean_FA_r_lower_cingulum_mask ~ story_d * cohort + age, df)
anova(FA_r_lower_cingulum_scd_story_d, FA_r_lower_cingulum_scd_story_d_age)
summary(FA_r_lower_cingulum_scd_story_d_age)
fig1FA <- ggplot(df, aes(story_d, mean_FA_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean FA",
       color = "Cohort", title = "Right Mean FA",
       subtitle = paste0(
                         "interaction p = ", 
                         signif(summary(FA_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1FA
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1FA, location = ph_location_type(type = "body"))

MD_r_lower_cingulum_scd_story_d <- lm(mean_MD_r_lower_cingulum_mask ~ story_d + cohort + cohort * story_d, df)
summary(MD_r_lower_cingulum_scd_story_d)
fig1MD <- ggplot(df, aes(story_d, mean_MD_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean MD",
       color = "Cohort", title = "Right Mean MD",
       subtitle = paste0(
                         "interaction p = ", 
                         signif(summary(MD_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1MD
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1MD, location = ph_location_type(type = "body"))

L1_r_lower_cingulum_scd_story_d <- lm(mean_L1_r_lower_cingulum_mask ~ story_d + cohort + cohort * story_d, df)
summary(L1_r_lower_cingulum_scd_story_d)
fig1AD <- ggplot(df, aes(story_d, mean_L1_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean AD",
       color = "Cohort", title = "Right Mean AD",
       subtitle = paste0(
                         "interaction p = ", 
                         signif(summary(L1_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
fig1AD
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1AD, location = ph_location_type(type = "body"))

RD_r_lower_cingulum_scd_story_d <- lm(mean_RD_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(RD_r_lower_cingulum_scd_story_d)
fig1RD <- ggplot(df, aes(story_d, mean_RD_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean RD",
       color = "Cohort", title = "Right Mean RD ",
       subtitle = paste0(
                         "interaction p = ", 
                         signif(summary(RD_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

fig1RD
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1RD, location = ph_location_type(type = "body"))

ISOVF_r_lower_cingulum_scd_story_d <- lm(mean_ISOVF_r_lower_cingulum_mask ~ story_d * cohort, df)
summary(ISOVF_r_lower_cingulum_scd_story_d)

fig1FWVF <- ggplot(df, aes(story_d, mean_ISOVF_r_lower_cingulum_mask, color = cohort)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x) +
  stat_poly_eq(use_label("P"), small.p = T, formula = y ~ x, label.x = "right") +
  labs(x = "Story Delayed Recall", y = "Mean FWVF",
       color = "Cohort", title = "Right Mean FWVF ",
       subtitle = paste0(
                        "interaction p = ", 
                        signif(summary(ISOVF_r_lower_cingulum_scd_story_d)$coefficients[4,4], 2))) +
  scale_color_hue(labels = c("Control", "SCD")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

fig1FWVF
ismrm_plots_ppts <- add_slide(ismrm_plots_ppts)
ismrm_plots_ppts <- ph_with(x = ismrm_plots_ppts, fig1FWVF, location = ph_location_type(type = "body"))

print(ismrm_plots_ppts, target='ismrm_plots.pptx')
