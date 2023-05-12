library(ggpubr)
library(tidyverse)

df = read_tsv('standard_data.tsv')
available_data = read_tsv('homeinterview_data.tsv')
df = full_join(df, available_data)
df = df %>% filter(Coil != 'NaN')

df <- df %>% mutate(SCD = ifelse(homeint_v233 != 'NaN' |
                                            homeint_v234 != 'NaN' |
                                            homeint_v235 != 'NaN' |
                                            homeint_v236 != 'NaN' |
                                            homeint_v237 != 'NaN' |
                                            homeint_v238 != 'NaN' |
                                            homeint_v239 != 'NaN' |
                                            homeint_v240 != 'NaN', TRUE, FALSE))
df$SCD_new <- df$homeint_v230
df$SCD_new[df$SCD_new == 1] <- FALSE
df$SCD_new[df$SCD_new == 2] <- TRUE

total = length(df$CCID)
scd_total = sum(df$SCD, na.rm = TRUE)
scd_new_total = sum(df$SCD, na.rm = TRUE)
scd_discrepancy <- df %>% filter(SCD != SCD_new) #no difference between verified and imputed data, remaining code can stay the same

df_scd_old <- df %>% filter(SCD == 2 & Age > 55)
df_old <- df %>% filter(Age > 55)
oldest <- max(df$Age)


#g ratio: combo of MT and diffusion

ggboxplot(df_old, x = "SCD", y = "homeint_mmse_i") +
  stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format..)))

ggboxplot(df_old, x = "SCD", y = "homeint_sevens") +
  stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format..)))

ggboxplot(df_old, x= "SCD", y = "homeint_storyrecall_i") +
  stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format..)))

ggboxplot(df_old, x= "SCD", y = "homeint_storyrecall_d") +
  stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format..)))

ggboxplot(df_old, x= "SCD", y = "Age") +
  stat_compare_means(method = 't.test', label.x.npc = 'center', position = 'jitter',
                     aes(label = paste0("p = ", ..p.format..))) #no difference in age

write.csv(df_old, file="old_homeint_and_standard.csv", fileEncoding = 'UTF-8', row.names = FALSE)
