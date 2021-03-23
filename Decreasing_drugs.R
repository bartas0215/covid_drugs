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
  ggplot(aes(Date, Availability)) + geom_line() + scale_y_continuous(limits = c(0,100)) + theme(panel.background = element_rect(fill = 'grey93'), axis.title.x=element_blank()) + 
  ggtitle("Acenocumarol") + scale_x_date(labels = date_format("%m/%y")) + labs(y = "Availability [%]")

#Plot nitrendipine
nitr <- df_3 %>%
  filter(Scope == "nitrendipine") %>%
  ggplot(aes(Date, Availability)) + geom_line() + scale_y_continuous(limits = c(0,100)) + theme(panel.background = element_rect(fill = 'grey93'),axis.title.x=element_blank()) +
  ggtitle("Nitrendipine") + scale_x_date(labels = date_format("%m/%y")) + labs(y = "Availability [%]")

#Plot diltiazem
dilt <-df_3 %>%
  filter(Scope == "diltiazem") %>%
  ggplot(aes(Date, Availability)) + geom_line() + scale_y_continuous(limits = c(0,100)) + theme(panel.background = element_rect(fill = 'grey93'),axis.title.x=element_blank()) +
  ggtitle("Diltiazem") + scale_x_date(labels = date_format("%m/%y")) + labs(y = "Availability [%]")


# Align all plots
dy <- ggarrange(ace, nitr, dilt, labels = c("A","B","C"), ncol = 1, nrow = 3)

###FIX###
annotate_figure(dy, left = text_grob("Availability [%]"), rot = 90)

#Save on disc
ggsave(plot = dy,filename = "decreasing_drugs.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600 )

