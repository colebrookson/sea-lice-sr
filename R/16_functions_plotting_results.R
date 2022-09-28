########## 
##########
# Plot output from all the options we have 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

library(here)
library(tidyverse)
library(ggthemes)

df_c = read_csv(here("./outputs/model-outputs/stock-recruit-models/joined-c-estimates.csv"))

df_c$min_pop = as.factor(df_c$min_pop)
df_c_wide = df_c %>% 
  tidyr::pivot_wider(., 
                     id_cols = c(scenario, min_pop),
                     names_from = value,
                     values_from = c)

ggplot(data = df_c_wide, aes(x = scenario, y = -MLE, fill = min_pop)) + 
  geom_errorbar(mapping = aes(ymin = -lower, ymax = -upper),
                position = position_dodge(width = 0.5),
                width = 0) +
  geom_point(position = position_dodge(width = 0.5), shape = 21, size = 3) +
  ylim(c(0, 0.5)) + 
  ggthemes::theme_base() + 
  labs(x = "Scenario", y = "MLE and 95% CI's") + 
  scale_fill_manual("Min. # of S-R \npairs per pop'n", 
                    values = wesanderson::wes_palette("Royal1", 2)) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)
  ) + 
  scale_x_discrete(
    labels = c("Scenario 1 (Indiv.)", "Scenario 1 (Year)", "Scenario 2", 
               "Scenario 3", "Scenario 4")
  )
  
