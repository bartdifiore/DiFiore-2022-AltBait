#--------------------------------------
## Setup
#--------------------------------------

library(tidyverse)
library(lubridate)
source("Code/theme.R")


#--------------------------------------
## Get field trial data
#--------------------------------------

df <- read.csv("Data/Rawdata_20240118.csv") %>% 
  janitor::clean_names() %>%
  as_tibble() %>%
  select(trap_pair, buoy_number, bait_type, date_set, date_retrieved, deployment_nights, lobster, num_short, num_legalsize, num_vnotch, num_egged, total_crab, rock_crab, green_crab, bait_remaining) %>%
  mutate(year = 2023, 
         date_set = paste(date_set, "/2023", sep = ""),
         date_retrieved = paste(date_retrieved, "/2023", sep = ""),
         clean_date_set = lubridate::mdy(date_set), 
         clean_date_retrieved = lubridate::mdy(date_retrieved))


start_time <- lubridate::as_date("2023-07-12")
end_time <- max(df$clean_date_retrieved)

#--------------------------------------
## Get temperature data
#--------------------------------------

temp <- read.csv("Data/Temperature_logs/all_loggers.csv") %>% 
  mutate(clean_time = lubridate::mdy_hm(time)) %>% 
  filter(clean_time >= start_time & clean_time <= end_time) %>% 
  pivot_longer(cols = temp_21299522:temp_pendant) %>%
  mutate(value_f = (value * (9/5)) + 32)

temp_lm <- lm(value ~ name, temp)
summary(temp_lm) # so differences in the mean are very close. within 1 degree of each other.


#--------------------------------------
## Plot temperature data
#--------------------------------------

temp_wide <- read.csv("Data/Temperature_logs/all_loggers.csv") %>% 
  mutate(clean_time = lubridate::mdy_hm(time)) %>% 
  filter(clean_time >= start_time & clean_time <= end_time)

temp_mat <- temp_wide %>% select(-time, -clean_time)

png("Figures/temp_comparison.png", width = 1000, height = 800)
psych::pairs.panels(temp_mat, main = "Comparison of different temperature loggers")
dev.off()


daily_temp <- temp %>%
  mutate(date = date(clean_time)) %>% 
  group_by(date) %>%
  summarize(mean_value = mean(value)) %>% 
  mutate(date = as.POSIXct(date)+12*60*60)


temp_plot <- ggplot(temp, aes(x = clean_time, y = value))+
  geom_line(aes(color = name), alpha = 0.5, show.legend = F)+
  geom_hline(yintercept = mean(temp$value), linetype = 4, color = "gray")+
  geom_line(data = daily_temp, aes(x = date, y = mean_value), color = "black", linewidth = 1)+
  scale_y_continuous(sec.axis = sec_axis(~ . *(9/5) + 32, name = expression(paste("Temperature (", degree, "F)"))))+
  labs(x = "", y = expression(paste("Temperature (", degree, "C)")))+
  theme_bd()

ggsave(filename = "Figures/temperature_plot.png", plot = temp_plot, device = "png", width = 8, height = 4)


read.csv("Data/Rawdata_20240118.csv") %>% 
  janitor::clean_names() %>%
  select(date_set, sst_set) %>%
  mutate(date_set = paste(date_set, "/2023", sep = ""), 
         date_set = mdy(date_set)) %>%
  ggplot(aes(x = date_set, y = sst_set))+
  geom_line()



#------------------------------------------------
## Aggregate temperature data to deployments
#------------------------------------------------

temp %>% 
  group_by(clean_time) %>%
  summarize(mean_temp = mean(value))


time_interval <- function(start_time, end_time){
  if(temp_time >= start_time & temp_time <= end_time){
    mean(temp)
  }
}

deployment_time <- df %>%
  distinct(clean_date_set, clean_date_retrieved) %>%
  mutate(period = interval(start = clean_date_set, end = clean_date_retrieved),
         deployment = 1:n())

period <- deployment_time$period
clean_time <- temp$clean_time

# temp$value[clean_time %within% period == T]

out <- vector()
for(k in 1:dim(deployment_time)[1]){
    out[k] = mean(temp$value[clean_time %within% period[k] == T])
}

deployment_time$avg_temp_c <- out
  

df_wtemp <- df %>% 
  left_join(deployment_time) %>% 
  mutate(bait_type = case_when(bait_type == "Herring" ~ "HERRING", 
                               bait_type == "50:50:00" ~ "50:50",
                               bait_type == "STUCK" ~ NA,
                               .default = bait_type), 
         alt_trad = ifelse(bait_type != "HERRING", "b_alt", "a_herring")) %>% 
  drop_na(alt_trad)

write.csv(df_wtemp, "Data/Fieldtrials_2023_wtemp.csv", row.names = F, quote = F)

ggplot(df_wtemp, aes(x = avg_temp_c, y = lobster))+
  geom_jitter(aes(color = alt_trad))+
  geom_smooth(aes(color = alt_trad), method = "lm")



