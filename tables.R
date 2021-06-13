#Load packages
library(tidyverse)
library(xlsx)
library(ggpubr)
library(scales)
library(ggforce)
library(zoo)

options(digits=2)

# Table - median below 50 % -----------------------------------------------

# Pull list of unique drugs
df_6_1 <- df_6 %>%
  distinct(Scope,.keep_all = TRUE) %>%
  pull(Scope) %>%
  as_tibble()

# Pull list of unique categories
df_6_2 <- df_6 %>%
  distinct(Scope,.keep_all = TRUE) %>%
  pull(Category) %>%
  as_tibble()

# Count median
df_6_3 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Median = median(Availability))

# Count mean and change number of decimal places to 2
df_6_4 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Mean = mean(Availability)) 

df_6_4 <- format(round(df_6_4$Mean, 2), nsmall = 2)

df_6_4 <- df_6_4 %>%
  as_tibble()

# Count min
df_6_5 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Min = min(Availability))

# Count max
df_6_6 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Max = max(Availability))

# Count moving average

df_6_7 <- df_6%>%
  mutate(MA_7 = rollmean(Availability, k = 7, fill = NA))

# Bind columns
df_6_7 <- bind_cols(df_6_1,df_6_2,df_6_3,df_6_4,df_6_5,df_6_6)

# Delete unused columns
df_6_8 <- df_6_7 %>%
  select(-c(Scope...3,Scope...6,Scope...8))

# Rename columns
df_6_8 <- df_6_8 %>%
  rename(Drug = value...1) %>%
  rename(Category = value...2) %>%
  rename(Mean = value...5)

# Count range
df_6_8 <- df_6_8 %>%
  mutate(Amplitude  = Max -Min)

# Save file 
write.xlsx(df_6_8, file = "D:/covid_drugs/Tables/median_below_50_1.xlsx")

# Table - decrease more than 20 %  -----------------------------------------

df_6 <- df_3 %>%
  filter(Scope == c("acenocoumarol","nitrendipine"))

df_6 <- df_6 %>%
  filter(between(Date,as.Date("2020-03-04"), as.Date("2020-12-31")))

# Pull list of unique drugs
df_7_1 <- df_6 %>%
  distinct(Scope,.keep_all = TRUE) %>%
  pull(Scope) %>%
  as_tibble()

# Pull list of unique categories
df_7_2 <- df_6 %>%
  distinct(Scope,.keep_all = TRUE) %>%
  pull(Category) %>%
  as_tibble()

# Count median
df_7_3 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Median = median(Availability))

# Count mean and change number of decimal places to 2
df_7_4 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Mean = mean(Availability)) 

df_7_4 <- format(round(df_7_4$Mean, 2), nsmall = 2)

df_7_4 <- df_7_4 %>%
  as_tibble()

# Count min
df_7_5 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Min = min(Availability))

# Count max
df_7_6 <- df_6 %>% 
  group_by(Scope) %>%
  summarise(Max = max(Availability))

# Bind columns
df_7_7 <- bind_cols(df_7_1,df_7_2,df_7_3,df_7_4,df_7_5,df_7_6)

# Delete unused columns
df_7_8 <- df_7_7 %>%
  select(-c(Scope...3,Scope...6,Scope...8))

# Rename columns
df_7_8 <- df_7_8 %>%
  rename(Drug = value...1) %>%
  rename(Category = value...2) %>%
  rename(Mean = value...5)

# Count range
df_7_8 <- df_7_8 %>%
  mutate(Amplitude  = Max -Min)

# Bind two tabels

# Bind columns
df_8_8 <- bind_cols(df_6_8,df_7_8)

# Save file 
write.xlsx(df_8_8, file = "D:/covid_drugs/Tables/decreasing_combined.xlsx")


# Table - before pandemics  -----------------------------------------------

df_6 <- df_6 %>%
  filter(between(Date,as.Date("2019-01-01"), as.Date("2020-04-03")))


