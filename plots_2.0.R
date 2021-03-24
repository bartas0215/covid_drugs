#Load packages
library(tidyverse)
library(xlsx)
library(ggpubr)
library(scales)
library(ggforce)

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

# Plot all drugs in a list
my_plots <- list( df_6 %>%
  ggplot(aes(Date, Availability))  +  geom_line()+ facet_wrap(~ Scope) + scale_x_date(labels = date_format("%m/%y"))+
    theme(axis.title.x=element_blank(),axis.title.y = element_text(size = 25),panel.spacing.x = unit(1, "lines"))+ 
    labs(y="Availability [%]"))

# Put all plots in one figure
xx <-ggarrange(plotlist = my_plots)

# Save results on disc
ggsave(plot = xx,filename = "median_below_50.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600,width = 17, height = 8, units = "in" )


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

# Plot all drugs in a list
my_plots_1 <- list( df_6_1 %>%
                    ggplot(aes(Date, Availability))  +  geom_line()+ facet_wrap_paginate(~Scope, ncol = 7, nrow = 7, page = 1) + scale_x_date(labels = date_format("%m/%y"))+
                    theme(axis.title.x=element_blank(),axis.title.y = element_text(size = 25),panel.spacing.x = unit(1, "lines"))+ 
                    labs(y="Availability [%]"))

my_plots_2 <- list( df_6_1 %>%
                      ggplot(aes(Date, Availability))  +  geom_line()+ facet_wrap_paginate(~Scope, ncol = 7, nrow = 6, page = 2) + scale_x_date(labels = date_format("%m/%y"))+
                      theme(axis.title.x=element_blank(),axis.title.y = element_text(size = 25),panel.spacing.x = unit(1, "lines"))+ 
                      labs(y="Availability [%]"))

# Put all plots in one figure
xx_1 <-ggarrange(plotlist = my_plots_1)
xx_2 <-ggarrange(plotlist = my_plots_2)

# Save results on disc
ggsave(plot = xx_1,filename = "median_above_50_1.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600,width = 18, height = 8, units = "in" )
ggsave(plot = xx_2,filename = "median_above_50_2.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600,width = 18, height = 8, units = "in" )          
