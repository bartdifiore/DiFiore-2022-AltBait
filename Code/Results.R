df <- read.csv("Data/Rawdata_20230621.csv") %>%
  janitor::clean_names()%>%
  filter(bait_type %in% c("100F", "100T", "50:50", "HERRING"))

mod1 <- glm(lobster ~ bait_type, data = df, family = "poisson")
summary(mod1)
out1 <- ggeffects::ggpredict(mod1)
plot(out1)


em <- emmeans(mod1, "bait_type")
contrast(em, "pairwise", adjust = "Tukey") # Not much of a difference...


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
ggsave("Figures/results_230621.png", width = 8, height = 4)
