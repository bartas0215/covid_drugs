
# Packages ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(xlsx)
library(trelliscopejs)


# Data_preparation --------------------------------------------------------

#Load list of drugs
drugs <- read_xls("C:/Users/Bartek/Desktop/Gdzie_po_lek_ publikacja/lista_lekow_internistycznych.xls")
colnames(drugs)[2] <- "Scope"
drugs

#Load availability data
df <- list.files(path = "C:/Users/Bartek/Desktop/Gdzie_po_lek_ publikacja",
                 full.names = TRUE,
                 recursive = TRUE,
                 pattern = "*.csv") %>% 
  tbl_df() %>%
  mutate(data = map(value, read.csv)) %>%
  arrange(value) %>%
  unnest(data) 

# Filter data by region (RegionId = 0 indicates the entire country)
df_1 <- df %>%
  filter(RegionId ==0)

# Delete unnecessary data 
df_1 <- df_1 %>%
  select(-value)

# Join list of drugs with availability data
df_2 <- df_1 %>%
  left_join(drugs, by = "Scope")

# Unmatch unused data
umatch <- drugs %>%
  anti_join(df_2, by = 'Scope')

#Filter by date 
df_2$Date <- as.Date(df_2$Date, format= "%Y-%m-%d")
df_3 <- df_2 %>%
  filter(between(Date,as.Date("2019-01-01"), as.Date("2020-12-31")))


# Exploratory analysis with trelliscopejs ---------------------------------
df_3 %>%
  ggplot(aes(Date, Availability)) + geom_line(aes(col=kategoria))+
  facet_trelliscope(~ Scope)

#long - ponad 1900 obrazkow
df_3 %>%
  ggplot(aes(Date, Availability)) + geom_line(aes(col=kategoria))+
  facet_trelliscope(~ Scope+RegionId)

#cala grupa mean
df_3 %>%
  group_by(kategoria, Date) %>%
  summarize(aval = mean(Availability)) %>%
  ggplot(aes(Date, aval)) + geom_line(aes(col=kategoria))+
  facet_trelliscope(~ kategoria)

#cala grupa - median
df_3 %>%
  group_by(kategoria, Date) %>%
  summarize(aval = median(Availability)) %>%
  ggplot(aes(Date, aval)) + geom_line(aes(col=kategoria))+
  facet_trelliscope(~ kategoria)

#grupy lekow
df_3 %>%
  ggplot() + geom_line(aes(Date, Availability, col=as.factor(Scope)))+
  facet_trelliscope(~ kategoria)


#antihypertensive_combined
df_3 %>%
  filter(kategoria=='antihypertensive_combined') %>%
  ggplot() + geom_line(aes(Date, Availability, col=as.factor(Scope)))+ theme(legend.position = "none")
