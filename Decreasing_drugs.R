#Load packages
library(tidyverse)
library(xlsx)
library(ggpubr)

# Rename column name
df_3 <- df_3 %>%
  rename(Category = kategoria)

# Get median of each drug availability 
df_4 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(median_aval = median(Availability))

# Plot acenocoumarol
ace <-df_3 %>%
  filter(Scope == "acenocoumarol") %>%
  ggplot(aes(Date, Availability)) + geom_line() + scale_y_continuous(limits = c(0,100)) + theme(panel.background = element_rect(fill = 'grey93')) + 
  ggtitle("Acenocumarol") 

#Plot nitrendipine
nitr <- df_3 %>%
  filter(Scope == "nitrendipine") %>%
  ggplot(aes(Date, Availability)) + geom_line() + scale_y_continuous(limits = c(0,100)) + theme(panel.background = element_rect(fill = 'grey93')) +
  ggtitle("Nitrendipine")

#Plot diltiazem
dilt <-df_3 %>%
  filter(Scope == "diltiazem") %>%
  ggplot(aes(Date, Availability)) + geom_line() + scale_y_continuous(limits = c(0,100)) + theme(panel.background = element_rect(fill = 'grey93')) +
  ggtitle("Diltiazem")


# Align all plots
dy <- ggarrange(ace, nitr, dilt, labels = c("A","B","C"), ncol = 1, nrow = 3)

#Save on disc
ggsave(plot = dy,filename = "decreasing_drugs.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600 )

