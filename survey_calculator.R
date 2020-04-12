library(rcompanion)
library(tidyverse)

## import file - in future create new data.frame from CSV, using vectors to import count & mass into individual columns

temp_cockle <- read.csv("data/temp_cockle.csv")

# define 250grid stations that overlap 100grid
north_stations <- c(187, 188, 189, 207, 208, 209, 227, 228, 229)
south_stations <- c(391, 392, 393, 395, 396, 400, 401, 402, 478)

# number of stations visited in each grid
sampled_250 <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Grid =="250m", na.rm = TRUE)
sampled_100n <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Block =="ZN", na.rm = TRUE)
sampled_100s <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Block =="ZS", na.rm = TRUE)
sampled_250_n <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Stn %in% north_stations, na.rm = TRUE)
sampled_250_s <- sum(temp_cockle$Sampled == "Y" & temp_cockle$Stn %in% south_stations, na.rm = TRUE)



# import and tidy count data
cockle_counts <- temp_cockle %>%
  select(Stn, Block, Grid, Sampled, Y0Count, Y1Count, Y2Count, Y3Count) %>% 
  mutate(total_count = Y0Count + Y1Count + Y2Count + Y3Count) %>% 
  gather(year_class, count, 'Y0Count', 'Y1Count', 'Y2Count', 'Y3Count', 'total_count')

# summary table count
summary_counts <- cockle_counts %>% 
  filter(Grid %in% c ("250m", "100m_n", "100m_s")) %>%
  mutate(count_sum = case_when(Grid == "250m" ~ count*10*250^2, Grid != "250m" ~ count*10*100^2)) %>%
  group_by(year_class, Grid) %>%
  summarize(count_totals = sum(count_sum, na.rm = TRUE))

# import and tidy mass data
cockle_mass <- temp_cockle %>%
  select(Stn, Block, Grid, Sampled, Y0Weight, Y1Weight, Y2Weight, Y3Weight) %>%
  mutate(total_weight = Y0Weight + Y1Weight + Y2Weight+ Y3Weight) %>%
  gather(year_class, mass, 'Y0Weight', 'Y1Weight', 'Y2Weight', 'Y3Weight', 'total_weight')

# summary table mass
summary_mass <- cockle_mass %>%
  filter(Grid %in% c("250m", "100m_n", "100m_s")) %>%
  mutate(mass_sum = case_when(Grid =="250m" ~ (mass/100)*250^2, Grid != "250m" ~ (mass/100)*100^2)) %>%
  group_by(year_class, Grid) %>%
  summarise(mass_totals = sum(mass_sum, na.rm = TRUE))

# groupwise means for counts

# groupwise means for main survey grids, filtered to remove NAs

g_250 <- cockle_counts %>% 
  filter(year_class %in% c("Y1Count", "Y2Count", "Y3Count", "total_count")) %>%
  filter(Grid %in% c("250m", "100m_n", "100m_s")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_g_250 <- groupwiseMean(count ~ year_class + Grid, data = g_250, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
count_intervals_250 <- count_conf_g_250 %>%  
  mutate(count_lower = case_when(Grid == "250m" ~ Bca.lower * 10 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.lower * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.lower * 10 * 100^2 * sampled_100s)) %>%
  mutate(count_upper = case_when(Grid == "250m" ~ Bca.upper * 10 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.upper * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.upper * 10 * 100^2 * sampled_100s)) %>%
  mutate(total_mean = case_when(Grid == "250m" ~ Mean * 10 * 250^2*sampled_250, Grid == "100m_n" ~ Mean * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Mean * 10 * 100^2 * sampled_100s))
# write file to csv
write.csv(count_intervals_250, "tabs/count_intervals_250.csv")

# groupwise means for north counts, filtered to remove NAs
n_250_c <- cockle_counts %>%
  filter(Stn %in% north_stations) %>%
  filter(Sampled == "Y") %>%
  filter(year_class %in% c("Y1Count", "Y2Count", "Y3Count", "total_count")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_n_250 <- groupwiseMean(count ~ year_class + Grid, data = n_250_c, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
count_intervals_n250 <- count_conf_n_250 %>%  
  mutate(count_lower = case_when(Grid == "250m" ~ Bca.lower * 10 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.lower * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.lower * 10 * 100^2 * sampled_100s)) %>%
  mutate(count_upper = case_when(Grid == "250m" ~ Bca.upper * 10 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.upper * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.upper * 10 * 100^2 * sampled_100s)) %>%
  mutate(total_mean = case_when(Grid == "250m" ~ Mean * 10 * 250^2*sampled_250, Grid == "100m_n" ~ Mean * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Mean * 10 * 100^2 * sampled_100s))

# write file to csv
write.csv(count_intervals_n250, "tabs/count_intervals_n250.csv")

# groupwise means for south counts, filtered to remove NAs
s_250_c <- cockle_counts %>%
  filter(Stn %in% south_stations) %>%
  filter(Sampled == "Y") %>%
  filter(year_class %in% c("Y1Count", "Y2Count", "Y3Count", "total_count")) %>%
  filter(!is.na(count))

# perform the groupwisemean selecting 10000 replicates and Bca
count_conf_s_250 <- groupwiseMean(count ~ year_class + Grid, data = s_250_c, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
count_intervals_s250 <- count_conf_s_250 %>%  
  mutate(count_lower = case_when(Grid == "250m" ~ Bca.lower * 10 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.lower * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.lower * 10 * 100^2 * sampled_100s)) %>%
  mutate(count_upper = case_when(Grid == "250m" ~ Bca.upper * 10 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.upper * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.upper * 10 * 100^2 * sampled_100s)) %>%
  mutate(total_mean = case_when(Grid == "250m" ~ Mean * 10 * 250^2*sampled_250, Grid == "100m_n" ~ Mean * 10 * 100^2 * sampled_100n, Grid == "100m_s" ~ Mean * 10 * 100^2 * sampled_100s))

# write file to csv
write.csv(count_intervals_s250, "tabs/count_intervals_s250.csv")

# groupwise means for mass

# groupwise means for main survey grids, filtered to remove NAs
g_250_m <- cockle_mass %>% 
  filter(year_class %in% c("Y1Weight", "Y2Weight", "Y3Weight", "total_weight")) %>%
  filter(Grid %in% c("250m", "100m_n", "100m_s")) %>%
  filter(!is.na(mass))
  
# perform the groupwisemean selecting 10000 replicates and Bca
mass_conf_g_250 <- groupwiseMean(mass ~ year_class + Grid, data = g_250_m, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
mass_intervals_250 <- mass_conf_g_250 %>%  
  mutate(mass_lower = case_when(Grid == "250m" ~ Bca.lower * 0.01 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.lower * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.lower * 0.01 * 100^2 * sampled_100s)) %>%
  mutate(mass_upper = case_when(Grid == "250m" ~ Bca.upper * 0.01 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.upper * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.upper * 0.01 * 100^2 * sampled_100s)) %>%
  mutate(total_mean = case_when(Grid == "250m" ~ Mean * 0.01 * 250^2* sampled_250, Grid == "100m_n" ~ Mean * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Mean * 0.01 * 100^2 * sampled_100s))

# write file to csv
write.csv(mass_intervals_250, "tabs/mass_intervals_250.csv")

# groupwise means for north mass, filtered to remove NAs
n_250_m <- cockle_mass %>%
  filter(Stn %in% north_stations) %>%
  filter(Sampled == "Y") %>%
  filter(year_class %in% c("Y1Weight", "Y2Weight", "Y3Weight", "total_weight")) %>%
  filter(!is.na(mass))

# perform the groupwisemean selecting 10000 replicates and Bca
mass_conf_n_250 <- groupwiseMean(mass ~ year_class + Grid, data = n_250_m, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
mass_intervals_n250 <- mass_conf_n_250 %>%  
  mutate(mass_lower = case_when(Grid == "250m" ~ Bca.lower * 0.01 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.lower * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.lower * 0.01 * 100^2 * sampled_100s)) %>%
  mutate(mass_upper = case_when(Grid == "250m" ~ Bca.upper * 0.01 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.upper * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.upper * 0.01 * 100^2 * sampled_100s)) %>%
  mutate(total_mean = case_when(Grid == "250m" ~ Mean * 0.01 * 250^2*sampled_250, Grid == "100m_n" ~ Mean * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Mean * 0.01 * 100^2 * sampled_100s))

# write file to csv
write.csv(mass_intervals_n250, "tabs/mass_intervals_n250.csv")

# groupwise means for south mass, filtered to remove NAs
s_250_m <- cockle_mass %>%
  filter(Stn %in% south_stations) %>%
  filter(Sampled == "Y") %>%
  filter(year_class %in% c("Y1Weight", "Y2Weight", "Y3Weight", "total_weight")) %>%
  filter(!is.na(mass))

# perform the groupwisemean selecting 10000 replicates and Bca
mass_conf_s_250 <- groupwiseMean(mass ~ year_class + Grid, data = s_250_m, conf = 0.95, digits = 3, R = 10000, boot = TRUE, traditional = FALSE, normal = FALSE, basic = FALSE, percentile = FALSE, bca = TRUE)

# calculate confidence intervals
mass_intervals_s250 <- mass_conf_s_250 %>%  
  mutate(mass_lower = case_when(Grid == "250m" ~ Bca.lower * 0.01 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.lower * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.lower * 0.01 * 100^2 * sampled_100s)) %>%
  mutate(mass_upper = case_when(Grid == "250m" ~ Bca.upper * 0.01 * 250^2 * sampled_250, Grid == "100m_n" ~ Bca.upper * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Bca.upper * 0.01 * 100^2 * sampled_100s)) %>%
  mutate(total_mean = case_when(Grid == "250m" ~ Mean * 0.01 * 250^2*sampled_250, Grid == "100m_n" ~ Mean * 0.01 * 100^2 * sampled_100n, Grid == "100m_s" ~ Mean * 0.01 * 100^2 * sampled_100s))

# write file to csv
write.csv(mass_intervals_s250, "tabs/mass_intervals_s250.csv")

