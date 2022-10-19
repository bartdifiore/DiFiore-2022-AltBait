library(tidyverse)



df <- read.csv("~/Downloads/Rawdata_2022 - Sheet1 (2).csv") %>% 
  janitor::clean_names()


df %>% group_by(bait_type) %>%
  summarize(mean_catch = mean(lobster, na.rm = T),
            sd_catch = sd(lobster, na.rm = T),
            total_catch = sum(lobster, na.rm = T)
  )

df %>%
  group_by(buoy_number, bait_type) %>%
  summarize(total_catch = sum(lobster, na.rm = T), 
            num_legal = sum(num_legalsize, na.rm = T), 
            num_short = sum(num_short, na.rm = T)) %>%
  arrange(bait_type)

labels <- df %>% 
  group_by(bait_type) %>%
  summarize(n = n())

df %>% group_by(bait_type) %>%
  summarize(mean_catch = mean(lobster, na.rm = T),
            sd_catch = sd(lobster, na.rm = T),
            se_catch = sd(lobster, na.rm = T)/n(),
            total_catch = sum(lobster, na.rm = T)) %>%
  filter(bait_type != "") %>%
  ggplot(aes(x = bait_type, y = mean_catch))+
  geom_bar(aes(fill = bait_type), stat = "identity")+
  geom_linerange(aes(ymin = mean_catch - se_catch, ymax = mean_catch + se_catch))+
  labs(x = "Bait type", y = "Catch", title = "Lobster catch")+
  theme_classic()

ggsave(filename = "~/Google Drive/Stier Lab/People/Bart DiFiore/Projects/AlternativeBait/Figures/plot1_v2.png" ,width = 5, height = 3.5)





dummy <- data.frame(distinct(df, bait_type), deployment_time = 0, mean_remaining = 100)

p1 <- df %>%
  mutate(deployment_time = case_when(date_retrieved == "7/15" ~ 24, 
                                     date_retrieved == "7/17" ~ 72)) %>%
  group_by(bait_type, deployment_time) %>%
  summarize(mean_remaining = mean(bait_remaining, na.rm = T)) %>%
  bind_rows(dummy) %>%
  ggplot(aes(x = deployment_time, y = mean_remaining))+
  geom_line(aes(color = bait_type), lwd = 2)+
  scale_color_manual(values = c("pink", "darkred", "forestgreen", "darkblue"))+
  scale_x_continuous(breaks = c(0, 24, 72))+
  labs(y = "Percent of bait remaining", x = "Deployment time (hours)")+
  theme_classic()


dummy <- data.frame(distinct(df, bait_type), deployment_time = 0, mean_catch = 0)

p2 <- df %>%
  mutate(deployment_time = case_when(date_retrieved == "7/15" ~ 24, 
                                     date_retrieved == "7/17" ~ 72)) %>%
  group_by(bait_type, deployment_time) %>%
  summarize(mean_catch= mean(lobster, na.rm = T)) %>%
  bind_rows(dummy) %>%
  ggplot(aes(x = deployment_time, y = mean_catch))+
  geom_line(aes(color = bait_type), lwd = 2)+
  scale_color_manual(values = c("pink", "darkred", "forestgreen", "darkblue"))+
  scale_x_continuous(breaks = c(0, 24, 72))+
  labs(y = "Mean lobster catch (ind.)", x = "Deployment time (hours)")+
  theme_classic()

cowplot::plot_grid(p1 + theme(legend.position = "none"), p2, nrow = 1)

ggsave(filename = "~/Google Drive/Stier Lab/People/Bart DiFiore/Projects/AlternativeBait/Figures/plot2.png" ,width = 8, height = 4.5)


df %>% group_by(bait_type) %>%
  summarize(mean_catch = mean(total_crab, na.rm = T),
            sd_catch = sd(total_crab, na.rm = T),
            se_catch = sd(total_crab, na.rm = T)/n(),
            total_catch = sum(total_crab, na.rm = T)) %>%
  filter(bait_type != "") %>%
  ggplot(aes(x = bait_type, y = mean_catch))+
  geom_bar(aes(fill = bait_type), stat = "identity")+
  geom_linerange(aes(ymin = mean_catch - se_catch, ymax = mean_catch + se_catch))+
  labs(x = "Bait type", y = "total_crab catch (ind.)")+
  theme_classic()

ggsave(filename = "~/Google Drive/Stier Lab/People/Bart DiFiore/Projects/AlternativeBait/Figures/plot3.png" ,width = 5, height = 3.5)


formod <- filter(df, bait_type != "")
mod1 <- glm(lobster ~ bait_type, data = formod, family = "poisson")
summary(mod1)
out1 <- ggeffects::ggpredict(mod1)
plot(out)
hist(residuals(mod1))

out1 <- data.frame(out1)

df %>% 
  group_by(bait_type) %>%
  filter(bait_type != "") %>%
  ggplot()+
  geom_jitter(aes(x = bait_type, y = lobster, color = bait_type), show.legend = F, height = 0)+
  geom_point(data = out1, aes(x = bait_type.x, y = bait_type.predicted), size = 4)+
  geom_linerange(data = out1, aes(x = bait_type.x, ymin = bait_type.conf.low, ymax = bait_type.conf.high))+
  labs(y = "Number of lobster (per haul)", x = "")+
  cowplot::theme_cowplot()



mod2 <- glm(num_legalsize ~ bait_type, data = formod, family = "poisson")
summary(mod2)
out2 <- ggeffects::ggpredict(mod2)
plot(out2)
out2 <- data.frame(out2)



df %>% 
  group_by(bait_type) %>%
  filter(bait_type != "") %>%
  ggplot()+
  geom_jitter(aes(x = bait_type, y = num_legalsize, color = bait_type), show.legend = F, height = 0)+
  geom_point(data = out2, aes(x = bait_type.x, y = bait_type.predicted), size = 4)+
  geom_linerange(data = out2, aes(x = bait_type.x, ymin = bait_type.conf.low, ymax = bait_type.conf.high))+
  labs(y = "Number of lobster (per haul)", x = "")+
  cowplot::theme_cowplot()



formod3 <- df %>% 
  mutate(alternative_label = ifelse(alternative_label == "", "Herring", alternative_label))

mod3 <- glm(lobster ~ alternative_label, data = formod3, family = "poisson")
summary(mod3)
out3 <- ggeffects::ggpredict(mod3)
plot(out3)
out3 <- data.frame(out3)

df %>%
  group_by(alternative_label) %>%
  mutate(alternative_label = ifelse(alternative_label == "", "Herring", alternative_label)) %>%
  tidybayes::mean_qi(lobster, na.rm = T) %>%
  ggplot()+
  geom_point(aes(x = alternative_label, y = lobster), size = 4)+
  geom_linerange(aes(x = alternative_label, ymin = .lower, ymax = .upper))+
  labs(y = "Predicted lobster catch per haul", x = "")+
  cowplot::theme_cowplot()


df %>%
  group_by(alternative_label) %>%
  mutate(alternative_label = ifelse(alternative_label == "", "Herring", alternative_label)) %>%
  tidybayes::mean_qi(lobster, na.rm = T)


df %>%
  group_by(alternative_label, deployment_nights) %>%
  mutate(alternative_label = ifelse(alternative_label == "", "Herring", alternative_label)) %>%
  tidybayes::mean_qi(bait_remaining, na.rm = T)
  ggplot()+
  geom_point(aes(x = deployment_nights, y = bait_remaining))+
  labs(y = "Predicted lobster catch per haul")



















