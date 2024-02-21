library(tidyverse)

df <- read.csv("Data/Rawdata_20240118.csv") %>%
  janitor::clean_names()%>%
  filter(bait_type %in% c("100F", "100T", "50:50", "HERRING"))

mod1 <- glm(lobster ~ bait_type, data = df, family = "poisson")
summary(mod1)
out1 <- ggeffects::ggpredict(mod1)
plot(out1)

library(emmeans)
em <- emmeans(mod1, "bait_type")
contrast(em, "pairwise", adjust = "Tukey")


p1 <- df %>%
  group_by(bait_type) %>%
  ggplot(aes(x = bait_type, y = lobster))+
  geom_jitter(aes(color = bait_type), show.legend = F, height = 0, width = 0.2)+
  stat_summary(fun.data = "mean_cl_boot", colour = "black", linewidth = 1, size = 1)+
  labs(y = "Number of lobster (per haul)", x = "")+
  theme_classic()

dummy <- data.frame(distinct(df, bait_type), deployment_nights = 0, mean_remaining = 100)

p2 <- df %>%
  group_by(bait_type, deployment_nights) %>%
  summarize(mean_remaining = mean(bait_remaining, na.rm = T)) %>%
  bind_rows(dummy) %>%
  ggplot(aes(x = deployment_nights, y = mean_remaining))+
  geom_line(aes(color = bait_type), lwd = 2)+
  stat_summary(aes(color = bait_type), fun.data = "mean_cl_boot", linewidth = 1, size = 1)+
  labs(y = "Mean bait remaining (%)", x = "Deployment time (days)")+
  theme_classic()

cowplot::plot_grid(p1, p2, nrow = 1, align = "h")
ggsave("Figures/results_240118.png", width = 8, height = 4)


df %>% group_by(bait_type) %>%
  tidybayes::mean_qi(lobster, na.rm = T)


#-------------------------
## updated 7/31
#-------------------------

df2 <- df %>% 
  select(trap_pair, date_retrieved, bait_type, lobster, buoy_number) %>%
  mutate(bait_cat = ifelse(bait_type == "HERRING", "control", "alt")) %>%
  ungroup() %>%
  select(-bait_type, -buoy_number) %>%
  # group_by(date_retrieved, bait_cat) %>%
  # mutate(id = 1:n()) %>%
  pivot_wider(names_from = bait_cat, values_from = lobster, values_fill = NA, values_fn = sum) %>%
  mutate(diff = alt - control) %>%
  drop_na(diff)


length(df2$diff[df2$diff <= 0])
length(df2$diff[df2$diff > 0])


df2 %>%
  ggplot(aes(x = diff))+
  geom_histogram(fill = "gray70", color = "black")+
  geom_vline(xintercept = 0, linetype = 4, lwd = 2)+
  annotate(geom = "text", x = c(-8, 2), y = c(15, 15), label = c("Alternative caught\nless than herring", "Alternative caught\nmore than herring"))+
  labs(x = "Difference in catch between trap pairs\n(alternative-herring)", y = "Count")+
  theme_classic()
  

library(glmmTMB)
library(DHARMa)

df.mod <- df %>%
  mutate(bait_type = case_when(bait_type == "HERRING" ~ "A_herring", 
                               bait_type == "50:50" ~ "B_50:50", 
                               bait_type == "100T" ~ "C_100T", 
                               bait_type == "100F" ~ "D_100F"))

mod2 <- glmmTMB(lobster ~ bait_type + deployment_nights + (1|trap_pair), data = df.mod, family = nbinom2(link = "log"))
summary(mod2)
simulated_residuals <- DHARMa::simulateResiduals(mod2)
plot(simulated_residuals)

out2 <- ggeffects::ggpredict(mod2, terms = ~bait_type)
plot(out2, add.data = T)


library(emmeans)
em <- emmeans(mod2, "bait_type")
contrast(em, "pairwise", adjust = "Tukey")



mod3 <- glmmTMB(num_legalsize ~ bait_type + deployment_nights + (1|trap_pair), data = df.mod, family = nbinom2(link = "log"))
summary(mod3)
simulated_residuals <- DHARMa::simulateResiduals(mod3)
plot(simulated_residuals)

out3 <- ggeffects::ggpredict(mod3, terms = ~bait_type)
plot(out3)

em <- emmeans(mod3, "bait_type")
contrast(em, "pairwise", adjust = "Tukey")

df %>% 
  group_by(bait_type) %>% 
  tidybayes::mean_qi(num_legalsize, na.rm = T)



df.mod %>%
  group_by(bait_type) %>%
  ggplot(aes(x = bait_type, y = lobster))+
  geom_jitter(aes(color = bait_type), show.legend = F, height = 0, width = 0.2)+
  geom_pointrange(data = as.data.frame(out2), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  labs(y = "Number of lobster (per haul)", x = "")+
  theme_classic()



df.mod %>%
  group_by(bait_type) %>%
  ggplot(aes(x = bait_type, y = num_legalsize))+
  geom_jitter(aes(color = bait_type), show.legend = F, height = 0, width = 0.2)+
  geom_pointrange(data = as.data.frame(out3), aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high))+
  labs(y = "Number of legal lobster (per haul)", x = "")+
  theme_classic()












