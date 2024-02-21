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
         alt_trad = ifelse(bait_type != "HERRING", "b_alt", "a_herring"), 
         year = as.factor(year)) %>%
  drop_na(alt_trad)



#------------------------------------------------
## Model difference between alt and herring
#------------------------------------------------


mod1 <- glmmTMB(lobster ~ alt_trad + deployment_nights + (1|trap_pair) + (1|year), data = df, family = nbinom2(link = "log"))
summary(mod1)
simulated_residuals <- DHARMa::simulateResiduals(mod1)
plot(simulated_residuals)

mod1.0 <- glmmTMB(lobster ~ alt_trad + deployment_nights + (1|trap_pair), data = df, family = nbinom2(link = "log"))
summary(mod1.0)
simulated_residuals <- DHARMa::simulateResiduals(mod1.0)
plot(simulated_residuals)

anova(mod1, mod1.0)


mod2 <- glmmTMB(lobster ~ alt_trad*deployment_nights + year + I(deployment_nights^2) + (1|trap_pair), data = df, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod2)
simulated_residuals <- DHARMa::simulateResiduals(mod2)
plot(simulated_residuals)

out2 <- ggeffects::ggpredict(mod2, terms = ~alt_trad+year)
plot(out2)

out3 <- ggeffects::ggpredict(mod2, terms = c("deployment_nights[all]", "alt_trad"))
plot(out3, add.data = T)

mod2.0 <- glmmTMB(lobster ~ alt_trad*deployment_nights + year + (1|trap_pair), data = df, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod2.0)
simulated_residuals <- DHARMa::simulateResiduals(mod2.0)
plot(simulated_residuals)

anova(mod2, mod2.0) # No evidence for a quadratic effect
anova(mod1, mod2) # Year as a fixed effect appears to be best
anova(mod1, mod2.0) # Appears that mod2.0 is the best candidate model

out2.0 <- ggeffects::ggpredict(mod2.0, terms = ~deployment_nights*alt_trad)
plot(out2.0, add.data = T)

out2.0.b <- ggeffects::ggpredict(mod2.0, terms = ~alt_trad, condition = c(year = "2023", deployment_nights = 5))
plot(out2.0.b, add.data = F)

pred_2.0.b <- as.data.frame(out2.0.b)
pred_2.0 <- as.data.frame(out2.0)

p1 <- df %>%
  ggplot(aes(x = alt_trad, y = lobster))+
  geom_jitter(aes(fill = alt_trad), show.legend = F, height = 0, width = 0.2, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", "#CD3718"))+
  geom_pointrange(data = pred_2.0.b, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  scale_x_discrete(labels = c("Herring", "Alternative"))+
  labs(y = "Number of lobster (per haul)", x = "")+
  theme_bd()

p2 <- df %>% 
  ggplot(aes(x = deployment_nights, y = lobster))+
  geom_jitter(aes(fill = alt_trad), show.legend = F, height = 0, width = 0.1, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", "#CD3718"))+
  geom_line(data = pred_2.0, aes(x = x, y = predicted, color = group), linewidth = 1.5, show.legend = F)+
  scale_color_manual(values = c("#18AFCD", "#CD3718"))+
  geom_ribbon(data = pred_2.0, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.2, show.legend = F)+
  labs(y = "", x = "Length of deployment (days)")+
  theme_bd()
  

cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.33, 0.66))

ggsave("Figures/all_baits.png", device = "png", width = 13.33*0.5, height = 7.5*0.5)


#-----------------------------
## Bait type comparison
#----------------------------

lm1 <- MASS::glm.nb(lobster ~ bait_type, data = df)
summary(lm1)

mod_baittype <- glmmTMB(lobster ~ bait_type + deployment_nights + (1|trap_pair), data = df, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod_baittype)
em <- emmeans(mod_baittype, "bait_type")
contrast(em, "pairwise", adjust = "Tukey")

em <- emmeans(lm1, "bait_type")
contrasts <- contrast(em, "pairwise", adjust = "Tukey")
temp <- as.data.frame(contrasts)
temp[temp$p.value <= 0.1, ]

means <- as.data.frame(ggpredict(lm1, terms = ~bait_type))

counts <- df %>% 
  group_by(bait_type) %>% 
  filter(bait_type != "B5") %>%
  summarize(count = n(), 
            mean_lob = mean(lobster, na.rm = T)) %>% 
  arrange(mean_lob)


cols_ramp <- colorRampPalette(c("#F7C7BD", "#CD3718"))

cols_ramp(15)

means %>%
  filter(x != "B5") %>%
  mutate(x = forcats::fct_reorder(x, predicted)) %>%
  ggplot(aes(y = x, x = predicted))+
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high, color = x), show.legend = F)+
  scale_color_manual(values = c(cols_ramp(14), "#18AFCD"))+
  annotate(geom = "text", x = 6.5, y = 1:15, label = paste("n = ", counts$count))+
  coord_cartesian(xlim = c(0, 7))+
  labs(x = "Number of lobster captured (per haul)", y = "Bait type")+
  theme_bd()

ggsave("Figures/by_baittype.png", device = "png", width = 8*0.75, height = 7*0.75)



#------------------------------------------------
## Crabs
#------------------------------------------------

mod_crabs <- glmmTMB(total_crab ~ alt_trad*deployment_nights + year + (1|trap_pair), data = df, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod_crabs)
simulated_residuals <- DHARMa::simulateResiduals(mod_crabs)
plot(simulated_residuals)


out_crab <- as.data.frame(ggeffects::ggpredict(mod_crabs, terms = ~alt_trad, condition = c(year = "2023", deployment_nights = 5)))


df %>% 
  ggplot(aes(x = alt_trad, y = total_crab))+
  geom_jitter(aes(fill = alt_trad), show.legend = F, height = 0, width = 0.2, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", "#CD3718"))+
  geom_pointrange(data = out_crab, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  scale_x_discrete(labels = c("Herring", "Alternative"))+
  labs(y = "Number of crab (per haul)", x = "")+
  theme_bd()

ggsave("Figures/crab_catch.png", width = 5, height = 4)


#------------------------------------------------
## By size/sex class
#------------------------------------------------

df2 <- df %>%
  select(trap_pair, alt_trad, num_short:num_egged, year, deployment_nights, bait_type) %>%
  pivot_longer(num_short:num_egged)

mod_size <- glmmTMB(value ~ alt_trad*name*deployment_nights + year + (1|trap_pair), data = df2, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod_size)
simulated_residuals <- DHARMa::simulateResiduals(mod_crabs)
plot(simulated_residuals)


out_size <- ggeffects::ggpredict(mod_size, terms = ~name*alt_trad, condition = c(year = "2023", deployment_nights = 5))

plot(out_size)+
  scale_x_discrete(labels = c("Short", "Legal sized", "V-notched", "Egged female"))

as.data.frame(out_size) %>%
  mutate(group = ifelse(group == "a_herring", "Herring", "Alternative")) %>%
  ggplot(aes(x = x, y = predicted, color = group))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.2))+
  scale_x_discrete(labels = c("Sub-legal", "Legal", "V-notched", "Egged female"))+
  scale_color_manual(values = rev(c("#18AFCD", "#CD3718")))+
  labs(x = "", y = "Number of individuals (per haul)", color = "")+
  theme_bd()+
  theme(legend.position = c(0.8, 0.8))

ggsave("Figures/by_lobsize-sex.png", device = "png", width = 5, height = 3.5)


#------------------------------------------------
## Bait degradation rates
#------------------------------------------------
df_deg <- df %>% 
  mutate(bait_remaining = bait_remaining / 100)
mod_degredation <- glmmTMB(bait_remaining ~ deployment_nights + bait_type, df_deg, family = "ordbeta")
summary(mod_degredation)

dummy <- data.frame(distinct(df, bait_type), deployment_nights = 0, mean_remaining = 100, n = 10, se = NA)

df %>% 
  group_by(bait_type, deployment_nights) %>% 
  summarize(mean_remaining = mean(bait_remaining, na.rm = T), 
            n = n(),
            se = mean_remaining/n) %>%
  bind_rows(dummy) %>%
  filter(bait_type %in% c("HERRING", "100F", "100T", "50:50")) %>% 
  filter(n > 1) %>%
ggplot(aes(x = deployment_nights, y = mean_remaining, group = bait_type))+
  geom_pointrange(aes(ymin = mean_remaining - se, ymax = mean_remaining + se, color = bait_type))+
  geom_line(aes(color = bait_type))+
  scale_color_manual(values = rev(c("#18AFCD", cols_ramp(3))))+
  labs(x = "Length of deployment (nights)", y = "Bait remaining (%)", color = "Bait type")+
  theme_bd()

ggsave("Figures/bait_degredation.png", device = "png", width = 5, height = 3.5)
















  
