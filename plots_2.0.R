#Load packages
library(tidyverse)
library(xlsx)
library(ggpubr)


df_4 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(mean_aval = mean(Availability)) %>%
  filter(mean_aval < 50)

df_5 <- df_4 %>%
  pull(Scope) %>%
  as.list()

df_6 <- df_3 %>% 
  filter(Scope %in% df_5) %>%
  as_tibble()

my_plots <- list( df_6 %>%
  ggplot(aes(Date, Availability))  +  geom_line(aes(col=kategoria))+ facet_wrap(~ Scope) )

dy <-ggarrange(plotlist = my_plots,nrow = ceiling(length(my_plots)),ncol = ceiling(length(my_plots)), common.legend = TRUE,
          widths = 50, heights = 200)

ggsave(plot = dy,filename = "c.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600 )
          