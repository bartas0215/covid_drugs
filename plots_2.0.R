#Load packages
library(tidyverse)
library(xlsx)
library(ggpubr)
library(scales)

df_4 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(median_aval = median(Availability)) %>%
  filter(median_aval < 50)

df_5 <- df_4 %>%
  pull(Scope) %>%
  as.list()

df_6 <- df_3 %>% 
  filter(Scope %in% df_5) %>%
  as_tibble()

my_plots <- list( df_6 %>%
  ggplot(aes(Date, Availability))  +  geom_line()+ facet_wrap(~ Scope)) + scale_x_date(labels = date_format("%m/%d")) +
  theme(axis.text.x = element_text(angle=45))

xx <-ggarrange(plotlist = my_plots,nrow = ceiling(length(my_plots)),ncol = ceiling(length(my_plots)),
          widths = 50, heights = 200)

ggsave(plot = xx,filename = "c.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600 )
          