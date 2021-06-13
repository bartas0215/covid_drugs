#Load packages
library(tidyverse)
library(xlsx)
library(ggpubr)
library(scales)
library(ggforce)
library(tools)
library(zoo)
library(ggrepel)

# Median below 50 - manuscript ------------------------------------------


# Find drugs with median availability below 50 % 
df_4 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(median_aval = median(Availability)) %>%
  filter(median_aval < 50)

# Pull list of drugs from the median table
df_5 <- df_4 %>%
  pull(Scope) %>%
  as.list()

# Filter main df to have only the needed drugs
df_6 <- df_3 %>% 
  filter(Scope %in% df_5) %>%
  as_tibble()

# First letter to uppercase
df_6 <- df_6 %>%
 mutate(Scope = toTitleCase(Scope))

# Second word to lowercase
df_6 <- df_6 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Medoxomil"), replacement = "medoxomil"))

# First letter in Category to uppercase
df_6 <- df_6 %>%
  mutate(Category = toTitleCase(Category))

#Make space between plus sign
df_6$Scope<-gsub("+"," + ",df_6$Scope,fixed = TRUE)

# Check structure
str(df_6)

# Build unique factors
df_6$Category <- factor(df_6$Category,levels=unique(df_6$Category))

# Add moving average
df_6_7 <- df_6%>%
  arrange(desc(Date)) %>% 
  group_by(Scope) %>%
  mutate(MA_7 = rollmean(Availability, k = 7, fill = NA)) %>%
  ungroup()

# Plot all drugs in a list
my_plots <- list( df_6_7 %>%
  ggplot(aes(Date, MA_7)) + geom_line(aes(col=Category))  +  facet_wrap(~ Category + Scope) + theme_bw() + scale_x_date(labels = date_format("%m/%y"))+
    theme(axis.title.x=element_blank(),axis.title.y = element_text(size = 20),panel.spacing.x = unit(1, "lines"),legend.position = "none")+ 
    labs(y="Availability [%]")  + annotate(geom = "point", x = as.Date("2020-03-04"),y = df_3$Availability , colour = "orange", size = 0.01)) 


# Put all plots in one figure
xx <-ggarrange(plotlist = my_plots)

# Save results on disc
ggsave(plot = xx,filename = "median_below_50_4.jpeg",path = "D:/covid_drugs/Images", device = "jpeg", dpi = 600,width = 20, height = 8, units = "in" )


# Median above 50 % - supplementary  -------------------------------------------------------

# Find drugs with median availability below 50 % 
df_4_1 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(median_aval = median(Availability)) %>%
  filter(median_aval > 50)

# Pull list of drugs from the median table
df_5_1 <- df_4_1 %>%
  pull(Scope) %>%
  as.list()

# Filter main df to have only the needed drugs
df_6_1 <- df_3 %>% 
  filter(Scope %in% df_5_1) %>%
  as_tibble()

# First letter to uppercase
df_6_1 <- df_6_1 %>%
  mutate(Scope = toTitleCase(Scope))

# Second word to lowercase
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Medoxomil"), replacement = "medoxomil"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Acid"), replacement = "acid"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Etexilate"), replacement = "etexilate"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Trinitrate"), replacement = "trinitrate"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Mononitrate"), replacement = "mononitrate"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Aspart"), replacement = "aspart"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Detemir"), replacement = "detemir"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Glargine"), replacement = "glargine"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Glulisine"), replacement = "glulisine"))
df_6_1 <- df_6_1 %>%
  mutate(Scope = str_replace_all(Scope, fixed("Lispro"), replacement = "lispro"))


#Make space between plus sign
df_6_1$Scope<-gsub("+"," + ",df_6_1$Scope,fixed = TRUE)

# Check structure
str(df_6_1)

# Build unique factors
df_6_1$Category <- factor(df_6_1$Category,levels=unique(df_6_1$Category))

# Add moving average
df_6_2 <- df_6_1%>%
  arrange(desc(Date)) %>% 
  group_by(Scope) %>%
  mutate(MA_7 = rollmean(Availability, k = 7, fill = NA)) %>%
  ungroup()

# Plot all drugs in a list
my_plots_1 <- list( df_6_2 %>%
                    ggplot(aes(Date, MA_7))  +  geom_line(aes(col=Category))+ facet_wrap_paginate(~ Category +Scope, ncol = 6, nrow = 7, page = 1) + theme_bw() + scale_x_date(labels = date_format("%m/%y"))+
                    theme(axis.title.x=element_blank(),axis.title.y = element_text(size = 25),panel.spacing.x = unit(1, "lines"),legend.position = "none")+ 
                    labs(y="Availability [%]") + annotate(geom = "point", x = as.Date("2020-03-04"),y = df_3$Availability , colour = "orange", size = 0.01))

my_plots_2 <- list( df_6_2 %>%
                      ggplot(aes(Date, MA_7))  +  geom_line(aes(col=Category))+ facet_wrap_paginate(~ Category +Scope, ncol = 7, nrow = 7, page = 2) + theme_bw() + scale_x_date(labels = date_format("%m/%y"))+
                      theme(axis.title.x=element_blank(),axis.title.y = element_text(size = 25),panel.spacing.x = unit(1, "lines"),legend.position = "none")+ 
                      labs(y="Availability [%]")  + annotate(geom = "point", x = as.Date("2020-03-04"),y = df_3$Availability , colour = "orange", size = 0.01))

# Put all plots in one figure
xx_1 <-ggarrange(plotlist = my_plots_1)
xx_2 <-ggarrange(plotlist = my_plots_2)

# Save results on disc
ggsave(plot = xx_1,filename = "median_above_50_1.jpeg",path = "D:/covid_drugs/Images", device = "jpeg", dpi = 600,width = 18, height = 8, units = "in" )
ggsave(plot = xx_2,filename = "median_above_50_2.jpeg",path = "D:/covid_drugs/Images", device = "jpeg", dpi = 600,width = 18, height = 8, units = "in" )          


