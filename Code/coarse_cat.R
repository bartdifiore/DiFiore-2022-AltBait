#----------------------------
## Setup 
#----------------------------

library(tidyverse)
library(emmeans)
library(ggeffects)
library(glmmTMB)
library(DHARMa)
source("Code/theme.R")

# Get 2022 data

df_22 <- read.csv("Data/Rawdata_20221017.csv") %>% 
  janitor::clean_names() %>%
  select(trap_pair, buoy_number, alternative_label, date_set, date_retrieved, deployment_nights, lobster, num_short, num_legalsize, num_vnotch, num_egged, total_crab, rock_crab, green_crab, bait_remaining) %>%
  mutate(year = 2022, 
         trap_pair = as.character(trap_pair)) %>% 
  rename(bait_type = alternative_label)

df_23 <- read.csv("Data/Rawdata_20240118.csv")%>% 
  janitor::clean_names() %>%
  select(trap_pair, buoy_number, bait_type, date_set, date_retrieved, deployment_nights, lobster, num_short, num_legalsize, num_vnotch, num_egged, total_crab, rock_crab, green_crab, bait_remaining) %>%
  mutate(year = 2023)

df <- bind_rows(df_22, df_23) %>%
  mutate(bait_type = case_when(bait_type == "Herring" ~ "HERRING", 
                               bait_type == "50:50:00" ~ "50:50",
                               bait_type == "STUCK" ~ NA,
                               .default = bait_type), 
         coarse_cat = case_when(year == 2022 & bait_type != "HERRING" ~ "Alt_2022", 
                                .default = bait_type), 
         year = as.factor(year)) %>% 
  drop_na(coarse_cat) %>% 
  filter(deployment_nights <= 10)



#------------------------------------------------
## Model difference between alt and herring
#------------------------------------------------


mod1 <- glmmTMB(lobster ~ coarse_cat*deployment_nights + (1|trap_pair) + (1|year), data = df, family = nbinom2(link = "log"))
summary(mod1)
simulated_residuals <- DHARMa::simulateResiduals(mod1)
plot(simulated_residuals)

mod1.0 <- glmmTMB(lobster ~ coarse_cat*deployment_nights + (1|trap_pair), data = df, family = nbinom2(link = "log"))
summary(mod1.0)
simulated_residuals <- DHARMa::simulateResiduals(mod1.0)
plot(simulated_residuals)

anova(mod1, mod1.0)


mod2.0 <- glmmTMB(lobster ~ coarse_cat*deployment_nights + year + (1|trap_pair), data = df, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod2.0)
simulated_residuals <- DHARMa::simulateResiduals(mod2.0)
plot(simulated_residuals)

em <- emmeans(mod2.0, ~coarse_cat | deployment_nights)
contrast(em, "pairwise", adjust = "Tukey")


anova(mod1, mod2.0) # Appears that mod2.0 is the best candidate model

out2.0 <- ggeffects::ggpredict(mod2.0, terms = ~deployment_nights*coarse_cat)
plot(out2.0, add.data = F)

out2.0.b <- ggeffects::ggpredict(mod2.0, terms = ~coarse_cat, condition = c(year = "2023", deployment_nights = 5))
plot(out2.0.b, add.data = F)

pred_2.0.b <- as.data.frame(out2.0.b)
pred_2.0 <- as.data.frame(out2.0)


cols_ramp <- colorRampPalette(c("#F7C7BD", "#CD3718"))

p1 <- df %>%
  mutate(coarse_cat = forcats::fct_reorder(coarse_cat, lobster, mean, .desc = T, .na_rm = T)) %>%
  ggplot(aes(x = coarse_cat, y = lobster))+
  geom_jitter(aes(fill = coarse_cat), show.legend = F, height = 0, width = 0.2, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", rev(cols_ramp(4))))+
  geom_pointrange(data = pred_2.0.b, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  # scale_x_discrete(labels = c("Herring", "Alternative"))+
  labs(y = "Number of lobster (per haul)", x = "")+
  theme_bd()

p2 <- df %>% 
  filter(coarse_cat %in% c("HERRING", "50:50")) %>%
  ggplot(aes(x = deployment_nights, y = lobster))+
  geom_jitter(aes(fill = coarse_cat), show.legend = F, height = 0, width = 0.1, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = rev(c("#18AFCD", "#CD3718")))+
  geom_line(data = pred_2.0 %>% filter(group %in% c("HERRING", "50:50")), aes(x = x, y = predicted, color = group), linewidth = 1.5, show.legend = F)+
  scale_color_manual(values = rev(c("#18AFCD", "#CD3718")))+
  geom_ribbon(data = pred_2.0 %>% filter(group %in% c("HERRING", "50:50")), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.2, show.legend = F)+
  coord_cartesian(ylim = c(0, 15), xlim = c(0, 11))+
  labs(y = "", x = "Length of deployment (days)")+
  theme_bd()


cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.33, 0.66))

