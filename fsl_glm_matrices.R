source('imaging_available.R')
library(tidyverse)

#pull story recall and anxiety from df_old
story_anxiety <- df_old %>% 
  select(CCID, homeint_storyrecall_i, homeint_storyrecall_d, additional_HADS_anxiety, additional_HADS_depression) %>% 
  rename("participant_id"="CCID")
#pull story and anxiety for each participant in df_old_dwi
df_old_dwi <- left_join(df_old_dwi, story_anxiety) %>% arrange(scd, participant_id)

#create design matrices
df_old_dwi <- df_old_dwi %>% mutate(EV1 = ifelse(scd == FALSE, 1, 0),
                                    EV2 = ifelse(scd == TRUE, 1, 0),
                                    EV3 = age,
                                    EV4 = homeint_storyrecall_i,
                                    EV5 = homeint_storyrecall_d,
                                    EV6 = ifelse(gender_code == 1, 1, 0),
                                    EV7 = additional_HADS_anxiety,
                                    EV8 = additional_HADS_depression)
#mean center covariates
#df_old_dwi <- df_old_dwi %>% filter(!participant_id %in% c('CC710551', 'CC722421', 'CC722891', 'CC620821', 'CC721519', 'CC621199', 'CC721504', 'CC520585'))
df_old_dwi$EV3 <- scale(df_old_dwi$EV3, scale = FALSE)
df_old_dwi$EV4 <- scale(df_old_dwi$EV4, scale = FALSE)
df_old_dwi$EV5 <- scale(df_old_dwi$EV5, scale = FALSE)
df_old_dwi$EV6 <- scale(df_old_dwi$EV6, scale = FALSE)
df_old_dwi$EV7 <- scale(df_old_dwi$EV7, scale = FALSE)
df_old_dwi$EV8 <- scale(df_old_dwi$EV8, scale = FALSE)

#cohort-depression interaction
cohort_dep_mat <- df_old_dwi %>% select(EV1, EV2, EV3, EV6, EV8)
cohort_dep_mat$EV7 <- cohort_dep_mat$EV8
cohort_dep_mat <- cohort_dep_mat %>% mutate(EV7 = ifelse(EV1 == 1, EV7, 0),
                                            EV8 = ifelse(EV1 == 0, EV8, 0))
cohort_dep_mat <- unname(as.matrix(cohort_dep_mat))
write.table(cohort_dep_mat, file = "glm/cohort_dep_mat.txt", sep = "\t", row.names = F, col.names = F)
cohort_dep_con <- rbind(c(0,0,0,0,1,-1), c(0,0,0,0,-1,1))
write.table(cohort_dep_con, file = "glm/cohort_dep_con.txt", sep = "\t", row.names = F, col.names = F)
#ctl depression
ctl_dep_mat <- df_old_dwi %>% select(EV1, EV3, EV6, EV8) %>% filter(EV1 == 1) %>% select(-EV1)
ctl_dep_mat <- unname(as.matrix(ctl_dep_mat))
write.table(ctl_dep_mat, file = "glm/ctl_dep_mat.txt", sep = "\t", row.names = F, col.names = F)
ctl_dep_con <- rbind(c(0,0,1), c(0,0,-1))
write.table(ctl_dep_con, file = "glm/ctl_dep_con.txt", sep = "\t", row.names = F, col.names = F)
#scd depression
scd_dep_mat <- df_old_dwi %>% select(EV2, EV3, EV6, EV8) %>% filter(EV2 == 1) %>% select(-EV2)
scd_dep_mat <- unname(as.matrix(scd_dep_mat))
write.table(scd_dep_mat, file = "glm/scd_dep_mat.txt", sep = "\t", row.names = F, col.names = F)
scd_dep_con <- rbind(c(0,0,1), c(0,0,-1))
write.table(scd_dep_con, file = "glm/scd_dep_con.txt", sep = "\t", row.names = F, col.names = F)

#cohort-anxiety interaction
cohort_anx_mat <- df_old_dwi %>% select(EV1, EV2, EV3, EV6, EV7)
cohort_anx_mat$EV8 <- cohort_anx_mat$EV7
cohort_anx_mat <- cohort_anx_mat %>% mutate(EV7 = ifelse(EV1 == 1, EV7, 0),
                                      EV8 = ifelse(EV1 == 0, EV8, 0))
cohort_anx_mat <- unname(as.matrix(cohort_anx_mat))
write.table(cohort_anx_mat, file = "glm/cohort_anx_mat.txt", sep = "\t", row.names = F, col.names = F)
cohort_anx_con <- rbind(c(0,0,0,0,1,-1), c(0,0,0,0,-1,1))
write.table(cohort_anx_con, file = "glm/cohort_anx_con.txt", sep = "\t", row.names = F, col.names = F)
#ctl anxiety
ctl_anx_mat <- df_old_dwi %>% select(EV1, EV3, EV6, EV7) %>% filter(EV1 == 1) %>% select(-EV1)
ctl_anx_mat <- unname(as.matrix(ctl_anx_mat))
write.table(ctl_anx_mat, file = "glm/ctl_anx_mat.txt", sep = "\t", row.names = F, col.names = F)
ctl_anx_con <- rbind(c(0,0,1), c(0,0,-1))
write.table(ctl_anx_con, file = "glm/ctl_anx_con.txt", sep = "\t", row.names = F, col.names = F)
#scd anxiety
scd_anx_mat <- df_old_dwi %>% select(EV2, EV3, EV6, EV7) %>% filter(EV2 == 1, !is.na(EV7)) %>% select(-EV2)
scd_anx_mat <- unname(as.matrix(scd_anx_mat))
write.table(scd_anx_mat, file = "glm/scd_anx_mat.txt", sep = "\t", row.names = F, col.names = F)
scd_anx_con <- rbind(c(0,0,1), c(0,0,-1))
write.table(scd_anx_con, file = "glm/scd_anx_con.txt", sep = "\t", row.names = F, col.names = F)

#age only
age_mat = df_old_dwi %>% select(EV3)
age_mat = cbind(EV1=1, age_mat)
age_mat = unname(as.matrix(age_mat))
write.table(age_mat, file = "glm/age_mat.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
age_con = rbind(c(1,0), c(0,1), c(-1,0), c(0,-1))
write.table(age_con, file = "glm/age_con.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
#scd and age
scd_age_mat = df_old_dwi %>% select(EV1,EV2,EV3) 
scd_age_mat$EV4 = scd_age_mat$EV3
scd_age_mat <- scd_age_mat %>% mutate(EV3 = ifelse(EV1 == 1, EV3, 0),
                                      EV4 = ifelse(EV1 == 0, EV4, 0))
scd_age_mat = unname(as.matrix(scd_age_mat))
write.table(scd_age_mat, file = "glm/scd_age_mat.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
scd_age_con <- rbind(c(0,0,1,-1), c(0,0,-1,1))
write.table(scd_age_con, file = "glm/scd_age_con.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

#story immediate only
story_i_mat = df_old_dwi %>% select(EV1, EV4)
story_i_mat$EV1 = 1
story_i_mat = unname(as.matrix(story_i_mat))
write.table(story_i_mat, file = "glm/story_i_mat.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
story_i_con = rbind(c(1,0), c(0,1), c(-1,0), c(0,-1))
write.table(story_i_con, file = "glm/story_i_con.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
#scd and story immediate
scd_story_i_mat = df_old_dwi %>% select(EV1, EV2, EV4)
scd_story_i_mat$EV5 = scd_story_i_mat$EV4
scd_story_i_mat <- scd_story_i_mat %>% mutate(EV4 = ifelse(EV1 == 1, EV4, 0),
                                              EV5 = ifelse(EV1 == 0, EV5, 0))
scd_story_i_mat = unname(as.matrix(scd_story_i_mat))
write.table(scd_story_i_mat, file = "glm/scd_story_i_mat.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
scd_story_i_con = rbind(c(0,0,1,-1), c(0,0,-1,1))
write.table(scd_story_i_con, file = "glm/scd_story_i_con.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

#story delayed only
story_d_mat = df_old_dwi %>% select(EV1, EV5)
story_d_mat$EV1 = 1
story_d_mat = unname(as.matrix(story_d_mat))
write.table(story_d_mat, file = "glm/story_d_mat.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
story_d_con = rbind(c(1,0), c(0,1), c(-1,0), c(0,-1))
write.table(story_d_con, file = "glm/story_d_con.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
#scd and story delay
scd_story_d_mat = df_old_dwi %>% select(EV1, EV2, EV5)
scd_story_d_mat$EV6 = scd_story_d_mat$EV5
scd_story_d_mat <- scd_story_d_mat %>% mutate(EV5 = ifelse(EV1 == 1, EV5, 0),
                                              EV6 = ifelse(EV1 == 0, EV6, 0))
scd_story_d_mat = unname(as.matrix(scd_story_d_mat))
write.table(scd_story_d_mat, file = "glm/scd_story_d_mat.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
scd_story_d_con = scd_story_i_con = rbind(c(0,0,1,-1), c(0,0,-1,1))
write.table(scd_story_d_con, file = "glm/scd_story_d_con.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

#gender only
gender_mat = df_old_dwi %>% select(EV6,EV7)
gender_mat = cbind(EV1=1, gender_mat)
gender_mat = unname(as.matrix(gender_mat))
write.table(gender_mat, file = "glm/gender_mat.txt", sep = "\t", row.names = FALSE, col.names = FALSE)

