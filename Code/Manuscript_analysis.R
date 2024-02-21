#--------------------------------------
## Setup
#--------------------------------------

library(tidyverse)
library(emmeans)
library(ggeffects)
library(glmmTMB)
library(DHARMa)
source("Code/theme.R")


#--------------------------------------
## Get data
#--------------------------------------

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
         year = as.factor(year), 
         bait_remaining = bait_remaining / 100) %>%
  drop_na(alt_trad)

#----------------------------------------
## Summary stats for manuscript
#----------------------------------------

dim(df)
sum(df$lobster, na.rm = T)
sum(df$rock_crab, na.rm =T)

df %>%
  group_by(alt_trad) %>%
  summarize(total = sum(lobster, na.rm = T), 
            n = n()) %>%
  mutate(cpue = total / n)

1.70/3.69


#----------------------------------------
## Analysis of 2022 trials
#----------------------------------------


df_2022 <- df %>% 
  filter(year == 2022) %>%
  filter(!bait_type %in% c("B3", "B4.2"))

length(unique(df_2022$bait_type))

mod1 <- glmmTMB(lobster ~ alt_trad*deployment_nights + (1|trap_pair), data = df_2022, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod1)
simulated_residuals <- DHARMa::simulateResiduals(mod1)
plot(simulated_residuals)


predictions <- ggeffects::ggpredict(mod1, terms = ~alt_trad, condition = c(deployment_nights = 5))
plot(predictions, add.data = F)

pred_df <- as.data.frame(predictions)

cols = c("predicted", "conf.low", "conf.high")
pred_df %>% summarize(across(all_of(cols), ~lead(.x)/.x))

dummy <- data.frame(distinct(df_2022, alt_trad), deployment_nights = 0, mean_remaining = 100, n = 10, se = NA)

cols_ramp <- colorRampPalette(c("#F7C7BD", "#CD3718"))
alt_cols <- rev(cols_ramp(3))


p1 <- df_2022 %>%
  ggplot(aes(x = alt_trad, y = lobster))+
  geom_jitter(aes(fill = alt_trad), show.legend = F, height = 0, width = 0.2, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", "#CD3718"))+
  geom_pointrange(data = pred_df, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  scale_x_discrete(labels = c("Herring", "Alternative"))+
  labs(y = expression(paste("Lobster catch (ind. ", haul^-1, ")")), x = "")+
  theme_bd()

ggsave("Figures/figure_s1.png", p1, width = 4, height = 3, dpi = 600)

p2 <- df_2022 %>% 
  group_by(alt_trad, deployment_nights) %>% 
  summarize(mean_remaining = mean(bait_remaining, na.rm = T)*100, 
            n = n(),
            se = mean_remaining/n) %>%
  bind_rows(dummy) %>%
  ggplot(aes(x = deployment_nights, y = mean_remaining, group = alt_trad))+
  geom_pointrange(aes(ymin = mean_remaining - se, ymax = mean_remaining + se, color = alt_trad))+
  geom_line(aes(color = alt_trad), linewidth = 1.0)+
  scale_color_manual(values = c("#18AFCD", "#CD3718"), labels = c("Herring", "Alternative"))+
  labs(x = "Length of deployment (nights)", y = "Bait remaining (%)", color = "Bait type")+
  scale_x_continuous(breaks = seq(0,10, by = 2))+
  theme_bd()+
  theme(legend.position = c(0.8, 0.8))

ggsave("Figures/figure_s2.png", p2, width = 5, height = 4, dpi = 600)

#----------------------------------------
## Analysis of 2023 trials
#----------------------------------------

df_2023 <- df %>% 
  filter(year == 2023) %>%
  mutate(bait_type = forcats::fct_reorder(bait_type, lobster, .fun = mean, .na_rm = T, .desc = T), 
         cat_deployment = ifelse(deployment_nights < 5, "short", "long"))

# Full model
mod2 <- glmmTMB(lobster ~ bait_type*deployment_nights + (1|trap_pair), data = df_2023, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod2)
simulated_residuals <- DHARMa::simulateResiduals(mod2)
plot(simulated_residuals)

  # Model selection
      mod2b <- glmmTMB(lobster ~ bait_type + deployment_nights + (1|trap_pair), data = df_2023, family = nbinom2(link = "log"))
      summary(mod2b)
      
      mod2c <- glmmTMB(lobster ~ bait_type + (1|trap_pair), data = df_2023, family = nbinom2(link = "log"))
      summary(mod2c)
      
      anova(mod2, mod2b)
      anova(mod2, mod2c)
      
      mod2d <- MASS::glm.nb(lobster ~ bait_type*deployment_nights, data = df_2023)
      summary(mod2d)
      
      AIC(mod2, mod2d)



post_hoc <- emtrends(mod2, pairwise ~ bait_type, var = "deployment_nights")
pairs(post_hoc)

post_hoc2 <- emmeans::emmeans(mod2, ~bait_type | deployment_nights)
pairs(post_hoc2)


predictions <- ggeffects::ggpredict(mod2, terms = ~bait_type, condition = c(deployment_nights = 5))
plot(predictions, add.data = F)
pred_df <- as.data.frame(predictions)


cols_ramp <- colorRampPalette(c("#F7C7BD", "#CD3718"))
alt_cols <- rev(cols_ramp(3))


p3 <- df_2023 %>%
  ggplot(aes(x = bait_type, y = lobster))+
  geom_jitter(aes(fill = bait_type), show.legend = F, height = 0, width = 0.2, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", alt_cols))+
  geom_pointrange(data = pred_df, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  annotate(geom = "text", x = 1:4, y = c(15, 11, 7, 7), label = c("A", "B", "B,C", "C"))+
  labs(y = expression(paste("Lobster catch (ind. ", haul^-1, ")")), x = "")+
  theme_bd()

ggsave("Figures/figure_1.png", p3, width = 5, height = 4, dpi = 600)

df_2023_filt <- df_2023 %>%
  filter(deployment_nights <= 10)

mod3 <- glmmTMB(lobster ~ alt_trad*deployment_nights + (1|trap_pair), data = df_2023_filt, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod3)
simulated_residuals <- DHARMa::simulateResiduals(mod3)
plot(simulated_residuals)

post_hoc <- emtrends(mod3, pairwise ~ alt_trad, var = "deployment_nights")
pairs(post_hoc, type = "response")


mod3b <- glmmTMB(lobster ~ alt_trad + deployment_nights + (1|trap_pair), data = df_2023_filt, family = nbinom2(link = "log"))

mod3c <- glmmTMB(lobster ~ alt_trad + (1|trap_pair), data = df_2023_filt, family = nbinom2(link = "log"))

anova(mod3, mod3b)
anova(mod3, mod3c)


predictions <- ggeffects::ggpredict(mod3, terms = ~deployment_nights*alt_trad)
plot(predictions, add.data = T)

pred_df <- as.data.frame(predictions)

p4.b <- df_2023_filt %>% 
  ggplot(aes(x = deployment_nights, y = lobster))+
  geom_jitter(aes(fill = alt_trad), height = 0.1, width = 0.1, alpha = 0.5, pch = 21, color = "grey", size = 1.9, show.legend = F)+
  scale_fill_manual(values = rev(c("#CD3718", "#18AFCD")), labels = c("Alternative", "Herring"))+
  geom_ribbon(data = pred_df, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.2, show.legend = F)+
  geom_line(data = pred_df, aes(x = x, y = predicted, color = group), linewidth = 1.5)+
  scale_color_manual(values = c("#CD3718", "#18AFCD"), labels = c("Alternative", "Herring"))+
  labs(y = expression(paste("Lobster catch (ind. ", haul^-1, ")")), x = "Length of deployment (days)", color = "Bait type")+
  theme_bd()+
  theme(legend.position = c(0.8,0.8), legend.background = element_blank())



dummy <- data.frame(distinct(df_2023, bait_type), deployment_nights = 0, mean_remaining = 100, n = 10, se = NA)


p4.a <- df_2023 %>% 
  group_by(bait_type, deployment_nights) %>% 
  summarize(mean_remaining = mean(bait_remaining, na.rm = T)*100, 
            n = n(),
            se = mean_remaining/n) %>%
  bind_rows(dummy) %>%
  filter(deployment_nights < 10) %>%
  ggplot(aes(x = deployment_nights, y = mean_remaining, group = bait_type))+
  geom_pointrange(aes(ymin = mean_remaining - se, ymax = mean_remaining + se, color = bait_type))+
  geom_line(aes(color = bait_type), linewidth = 1.0)+
  scale_color_manual(values = c("#18AFCD", alt_cols))+
  labs(x = "Length of deployment (nights)", y = "Bait remaining (%)", color = "Bait type")+
  theme_bd()+
  theme(legend.position = c(0.8, 0.8))

cowplot::plot_grid(p4.a, p4.b, align = "h", nrow = 1, labels = "AUTO")

ggsave("Figures/figure_2.png", width = 8, height = 4, dpi = 600)


#----------------------------------------
## Temperature analysis
#----------------------------------------

df_2023_temp <- read.csv("Data/Fieldtrials_2023_wtemp.csv") %>% as_tibble() %>%
  drop_na(avg_temp_c)

mod_temp <- glmmTMB(lobster ~ alt_trad*(deployment_nights + avg_temp_c) + (1|trap_pair), data = df_2023_temp, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod_temp)

predictions <- ggeffects::ggpredict(mod_temp, terms = ~avg_temp_c*alt_trad)
plot(predictions, add.data = T) # Some evidence, albeit weak that the alternative fished better at warmer temperatures. 
pred_df <- as.data.frame(predictions)


temp_p2 <- df_2023_temp %>% 
  ggplot(aes(x = avg_temp_c, y = lobster))+
  geom_jitter(aes(fill = alt_trad), height = 0.1, width = 0.1, alpha = 0.5, pch = 21, color = "grey", size = 1.9, show.legend = F)+
  scale_fill_manual(values = rev(c("#CD3718", "#18AFCD")), labels = c("Alternative", "Herring"))+
  geom_ribbon(data = pred_df, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group), alpha = 0.2, show.legend = F)+
  geom_line(data = pred_df, aes(x = x, y = predicted, color = group), linewidth = 1.5)+
  scale_color_manual(values = c("#CD3718", "#18AFCD"), labels = c("Alternative", "Herring"))+
  labs(y = expression(paste("Lobster catch (ind. ", haul^-1, ")")), x = expression(paste("Temperature (", degree, "C)")), color = "Bait type")+
  theme_bd()+
  theme(legend.position = c(0.8, 0.8), legend.background = element_blank())

source("Code/pull_temp_data.R")

cowplot::plot_grid(temp_plot, temp_p2, nrow = 2, align = "v", labels = "AUTO")

ggsave("Figures/figure3.png", width = 8, height = 7, dpi = 600)

predict(mod_temp, newdata = data.frame(alt_trad = "b_alt", deployment_nights = 5, avg_temp_c = c(12, 15)), type = "response", re.form = NA)


#----------------------------------------
## By lobster size/sex
#----------------------------------------

df_2023_long <- df_2023 %>%
  select(trap_pair, alt_trad, num_short:num_egged, year, deployment_nights, bait_type) %>%
  pivot_longer(num_short:num_egged)

mod_size <- glmmTMB(value ~ alt_trad*name*deployment_nights + (1|trap_pair), data = df_2023_long, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod_size)
simulated_residuals <- DHARMa::simulateResiduals(mod_size)
plot(simulated_residuals)


post_hoc3 <- emmeans::emmeans(mod_size, ~ alt_trad * name | deployment_nights)

v1 <- c(1, 0, 0, 0, 0, 0, 0, 0)
v2 <- c(0, 1, 0, 0, 0, 0, 0, 0)
v3 <- c(0, 0, 1, 0, 0, 0, 0, 0)
v4 <- c(0, 0, 0, 1, 0, 0, 0, 0)
v5 <- c(0, 0, 0, 0, 1, 0, 0, 0)
v6 <- c(0, 0, 0, 0, 0, 1, 0, 0)
v7 <- c(0, 0, 0, 0, 0, 0, 1, 0)
v8 <- c(0, 0, 0, 0, 0, 0, 0, 1)

contrast(post_hoc3, method = list("H_egged - A_egged" = v1 - v2, 
                                  "H_legal - A_legal" = v3 - v4, 
                                  "H_short - A_short" = v5 - v6, 
                                  "H_v - A_v" = v7 - v8))

contrast(post_hoc3, method = list("Egged vs. legal" = (v1-v2) - (v3-v4), 
                                  "Egged vs. short" = (v1 - v2) - (v5 - v6), 
                                  "Egged vs. v-notch" = (v1 - v2) - (v7 - v8), 
                                  "Legal vs. short" = (v3 - v4) - (v5 - v6), 
                                  "Legal vs. v-notch" = (v3 - v4) - (v7 - v8), 
                                  "Short vs. v-notch" = (v5 - v6) - (v7 - v8)))

contrast(post_hoc3, method = list("Egged vs. legal" = (v1-v2) - (v3-v4), 
                                  "Egged vs. short" = (v1 - v2) - (v5 - v6), 
                                  "Egged vs. v-notch" = (v1 - v2) - (v7 - v8), 
                                  "Legal vs. short" = (v3 - v4) - (v5 - v6), 
                                  "Legal vs. v-notch" = (v3 - v4) - (v7 - v8),
                                  "Legal vs. egged" = (v3 - v4) - (v1 - v2), 
                                  "Short vs. egged" = (v5 - v6) - (v1 - v2), 
                                  "Short vs. legal" = (v5 - v6) - (v3 - v4),
                                  "Short vs. v-notch" = (v5 - v6) - (v7 - v8), 
                                  "V-notch vs. egged" = (v7 - v8) - (v1 - v2), 
                                  "V-notch vs. legal" = (v7 - v8) - (v3 - v4), 
                                  "V-notch vs. short" = (v7 - v8) - (v5 - v6)))


out_size <- ggeffects::ggpredict(mod_size, terms = ~deployment_nights*alt_trad*name)
plot(out_size)

as.data.frame(out_size) %>%
  mutate(group = ifelse(group == "a_herring", "Herring", "Alternative")) %>%
  ggplot(aes(x = x, y = predicted))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = group), alpha = 0.25)+
  geom_line(aes(color = group))+
  scale_color_manual(values = rev(c("#18AFCD", "#CD3718")))+
  facet_wrap(~facet, scales = "free")+
  labs(x = "Deployment length (nights)", y = "Number of individuals (per haul)", color = "")+
  theme_bd()+
  theme(legend.position = c(0.8, 0.8))


out_size <- ggeffects::ggpredict(mod_size, terms = ~name*alt_trad, condition = c(deployment_nights = 5))

p5 <- as.data.frame(out_size) %>%
  mutate(group = ifelse(group == "a_herring", "Herring", "Alternative")) %>%
  ggplot(aes(x = x, y = predicted, color = group))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.2))+
  scale_x_discrete(labels = c("Sub-legal", "Legal", "V-notched", "Egged female"))+
  scale_color_manual(values = rev(c("#18AFCD", "#CD3718")))+
  #scale_y_log10()+
  annotate(geom = "text", x = 1:4, y = c(1.25, 1.25, 0.5, 0.5), label = c("A", "B", "C", "A,B,D"))+
  labs(x = "", y = expression(paste("Lobster catch (ind. ", haul^-1, ")")), color = "")+
  theme_bd()+
  theme(legend.position = c(0.8, 0.8))

ggsave("Figures/by_lobsize-sex.png", plot = p5, device = "png", width = 5, height = 3.5, dpi = 600)


#----------------------------------------
## By crab catch
#----------------------------------------


mod_crabs <- glmmTMB(total_crab ~ alt_trad*deployment_nights + (1|trap_pair), data = df_2023, family = nbinom2(link = "log"), ziformula = ~1)
summary(mod_crabs)
simulated_residuals <- DHARMa::simulateResiduals(mod_crabs)
plot(simulated_residuals)


out_crab <- as.data.frame(ggeffects::ggpredict(mod_crabs, terms = ~alt_trad, condition = c(deployment_nights = 5)))


p6 <- df_2023 %>% 
  mutate(alt_trad = as.factor(alt_trad)) %>%
  ggplot(aes(x = alt_trad, y = total_crab))+
  geom_jitter(aes(fill = alt_trad), show.legend = F, height = 0, width = 0.2, alpha = 0.5, pch = 21, color = "grey", size = 1.9)+
  scale_fill_manual(values = c("#18AFCD", "#CD3718"))+
  geom_pointrange(data = out_crab, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  scale_x_discrete(labels = c("Herring", "Alternative"))+
  labs(y = expression(paste("Crab catch (ind. ", haul^-1, ")")), x = "")+
  theme_bd()

ggsave("Figures/crab_catch.png", p6,  width = 5, height = 4)

cowplot::plot_grid(p5, p6, nrow = 1, align = "h", labels = "AUTO", rel_widths = c(0.6, 0.4))

ggsave("Figures/figure4.png", device = "png", width = 8, height = 4, dpi = 600)

