#----------------------------
## Setup 
#----------------------------

library(tidyverse)
library(emmeans)
library(ggeffects)

library(glmmTMB)
library(DHARMa)
source("Code/theme.R")


df <- read.csv("Data/Rawdata_20240118.csv")%>% 
  janitor::clean_names() %>%
  select(trap_pair, buoy_number, bait_type, date_set, date_retrieved, deployment_nights, lobster, num_short, num_legalsize, num_vnotch, num_egged, total_crab, rock_crab, green_crab, bait_remaining) %>%
  mutate(year = 2023) %>%
  mutate(bait_type = case_when(bait_type == "Herring" ~ "HERRING", 
                               bait_type == "50:50:00" ~ "50:50",
                               bait_type == "STUCK" ~ NA,
                               .default = bait_type), 
         alt_trad = ifelse(bait_type != "HERRING", "b_alt", "a_herring"), 
         year = as.factor(year)) %>%
  drop_na(alt_trad) %>%
  mutate(bait_type = forcats::fct_reorder(bait_type, lobster, .fun = mean, .na_rm = T, .desc = T))



mod1 <- glmmTMB(lobster ~ bait_type*deployment_nights + (1|trap_pair), data = df, family = nbinom2(link = "log"))
summary(mod1)
simulated_residuals <- DHARMa::simulateResiduals(mod1)
plot(simulated_residuals)


predictions <- ggeffects::ggpredict(mod1, terms = ~bait_type, condition = c(deployment_nights = 5))
plot(predictions, add.data = F)

pred_df <- as.data.frame(predictions)


cols_ramp <- colorRampPalette(c("#F7C7BD", "#CD3718"))
alt_cols <- rev(cols_ramp(3))


p1 <- df %>%
  ggplot(aes(x = bait_type, y = lobster))+
  geom_jitter(aes(fill = bait_type), show.legend = F, height = 0, width = 0.2, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", alt_cols))+
  geom_pointrange(data = pred_df, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  # scale_x_discrete(labels = c("Herring", "Alternative"))+
  labs(y = "Lobster catch (ind. per haul)", x = "")+
  theme_bd()





#------------------------------------------------
## Bait degradation rates
#------------------------------------------------
df_deg <- df %>% 
  mutate(bait_remaining = bait_remaining / 100)

dummy <- data.frame(distinct(df, bait_type), deployment_nights = 0, mean_remaining = 100, n = 10, se = NA)

p2 <- df %>% 
  group_by(bait_type, deployment_nights) %>% 
  summarize(mean_remaining = mean(bait_remaining, na.rm = T), 
            n = n(),
            se = mean_remaining/n) %>%
  bind_rows(dummy) %>%
  filter(bait_type %in% c("HERRING", "100F", "100T", "50:50")) %>% 
  filter(n > 1) %>%
  ggplot(aes(x = deployment_nights, y = mean_remaining, group = bait_type))+
  geom_pointrange(aes(ymin = mean_remaining - se, ymax = mean_remaining + se, color = bait_type))+
  geom_line(aes(color = bait_type), linewidth = 1.0)+
  scale_color_manual(values = c("#18AFCD", rev(cols_ramp(3))))+
  labs(x = "Length of deployment (nights)", y = "Bait remaining (%)", color = "Bait type")+
  theme_bd()+
  theme(legend.position = c(0.2, 0.2))


cowplot::plot_grid(p1, p2, nrow =1, align = "h")


ggsave("Figures/for_ALImeeting_default.png", device = "png", width = 10, height = 7.5)
ggsave("Figures/for_ALImeeting_wide.png", device = "png", width = 16*0.8, height = 9*0.8)













