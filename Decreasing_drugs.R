#Load packages
library(tidyverse)
library(xlsx)
library(ggpubr)

# Rename column name
df_3 <- df_3 %>%
  rename(Category = kategoria)


# Decrease more than 20 % during pandemics  -------------------------------------------------

# Get median of each drug availability 
df_4 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(median_aval = median(Availability))

# Plot acenocoumarol
ace <-df_3 %>%
  filter(Scope == "acenocoumarol") %>%
  ggplot(aes(Date, Availability)) + geom_line() +  theme_bw() + scale_y_continuous(limits = c(0,100)) + theme(axis.title.x=element_blank(),axis.title.y =element_blank()) + 
  ggtitle("Acenocumarol") + scale_x_date(labels = date_format("%m/%y")) 

ace_1 <- ace +   annotate(geom = "point", x = as.Date("2020-03-04"),y = df_3$Availability , colour = "orange", size = 1)

#Plot nitrendipine
nitr <- df_3 %>%
  filter(Scope == "nitrendipine") %>%
  ggplot(aes(Date, Availability)) + geom_line() + theme_bw() +scale_y_continuous(limits = c(0,100)) + theme(axis.title.x=element_blank(),axis.title.y =element_blank()) +
  ggtitle("Nitrendipine") + scale_x_date(labels = date_format("%m/%y")) 

nitr_1 <- nitr + annotate(geom = "point", x = as.Date("2020-03-04"),y = df_3$Availability , colour = "orange", size = 1)


# Align all plots
dy <- ggarrange(ace_1, nitr_1, labels = c("A","B"), ncol = 1, nrow = 2)

# Annotate figure 
dy_1 <- annotate_figure(dy, left = text_grob("Availability [%]", rot = 90, size = 15))

#Save on disc
ggsave(plot = dy_1,filename = "decreasing_drugs_2.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600,width = 5, height = 8, units = "in" )



# Decrease less than 20 % and bigger than 5 % ------------------------------------------------

# Count maximum availability 
df_4_2 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(max_aval = max(Availability)) 

# Count minimum availability 
df_4_3 <- df_3 %>% 
  group_by(Scope) %>%
  summarise(min_aval = min(Availability))

# Bind cols
df_4_4 <- bind_cols(df_4_2,df_4_3)  

# Tidy new data frame
df_4_4 <- df_4_4 %>%
  select(-Scope...3)
df_4_4 <- df_4_4 %>%
  rename(Scope = Scope...1)

# Count range
df_4_5 <- df_4_4 %>%
  mutate(Range = max_aval - min_aval)

# Filter range between 5 and 20 %
df_4_6 <- df_4_5 %>%
  filter(Range >5 & Range <20)

# Pull list of drugs from the median table
df_4_7 <- df_4_6 %>%
  pull(Scope) %>%
  as.list()

# Filter main df to have only the needed drugs
df_4_8 <- df_3 %>% 
  filter(Scope %in% df_4_7) %>%
  as_tibble()

# Filter valued drugs
df_4_9 <- df_4_8 %>%
  filter(Scope == c("vildagliptin","trandolapril","molsidomine","nadroparin","insulin glulisine","insulin detemir",
                    "hydrochlorothiazide+ramipril","glipizide","glyceryl trinitrate","dalteparin","ciprofibrate",
                    "cilazapril","amlodipine+losartan","amlodipine+atorvastatin","amiodarone"))



# Plot all drugs in a list
my_plots <- list( df_4_9 %>%
                    ggplot(aes(Date, Availability))  +  geom_line()+ facet_wrap(~ Scope) + scale_x_date(labels = date_format("%m/%y"))+
                    theme(axis.title.x=element_blank(),axis.title.y = element_text(size = 25),panel.spacing.x = unit(1, "lines"))+ 
                    labs(y="Availability [%]"))

# Put all plots in one figure
xx <-ggarrange(plotlist = my_plots)

# Save results on disc
ggsave(plot = xx,filename = "Decrease_5_20.png",path = "D:/covid_drugs/Images", device = "png", dpi = 600,width = 17, height = 8, units = "in" )
