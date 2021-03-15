#Install packages
install.packages("pastecs")

#Load packages
library(pastecs)
library(tidyverse)
library(xlsx)

# Setting options
options(scipen=999)

# Load function for analysis

drug_f <- function(x,y,z) {

### Save plot regarding drug  
# Filter data 
drug_graph <- df_3 %>%
  filter(Scope == {{x}}) %>%
  ggplot(aes(Date, Availability)) + geom_line(aes(col=kategoria))

# Save file 
ggsave(plot = drug_graph,filename = y,path = "D:/covid_drugs/Images", device = "png", dpi = 600 )


### Performing basic statistics
# Pull Availability
drug_1 <- df_3 %>%
  filter(Scope == {{x}}) %>%
  stat.desc()

drug_2 <- drug_1 %>%
  select(Availability)

write.xlsx(drug_2,file = z)

}

## Perform analysis 

# Drugs with supply shortage

drug_f(x = "acenocoumarol", y = "acenocoumarol.png", z = "D:/covid_drugs/Tables/acenocoumarol.xlsx")
drug_f(x = "nitrendipine", y = "nitrendipine.png", z = "D:/covid_drugs/Tables/nitrendipine.xlsx")
drug_f(x = "dalteparin", y = "dalteparin.png", z = "D:/covid_drugs/Tables/dalteparin.xlsx")
drug_f(x = "diltiazem", y = "diltiazem.png", z = "D:/covid_drugs/Tables/diltiazem.xlsx")

# Drugs which become unavailable during COVID pandemics

drug_f(x = "prasugrel", y = "prasugrel.png", z = "D:/covid_drugs/Tables/prasugrel.xlsx")
drug_f(x = "bemiparin", y = "bemiparin.png", z = "D:/covid_drugs/Tables/bemiparin.xlsx")
drug_f(x = "exenatide", y = "exenatide.png", z = "D:/covid_drugs/Tables/exenatide.xlsx")


# Drug within group - mean availability 

drug_f_group <- function(x,y,z) {
  
  ### Save plot regarding drug  
  # Filter data 
  drug_graph_1 <- df_3 %>%
    filter(kategoria == {{x}}) %>%
    group_by(kategoria, Date) %>%
    summarize(aval = mean(Availability)) %>%
    ggplot(aes(Date, aval)) + geom_line(aes(col=kategoria)) + scale_y_continuous(limits = c(0,100))
  
  # Save file 
  ggsave(plot = drug_graph_1,filename = y,path = "D:/covid_drugs/Images_category", device = "png", dpi = 600 )
  
  
  ### Performing basic statistics
  # Pull Availability
  drug_2 <- df_3 %>%
    filter(kategoria == {{x}}) %>%
    group_by(kategoria, Date) %>%
    summarize(aval = mean(Availability)) %>%
    stat.desc()
  
  drug_3 <- drug_2 %>%
    select(aval)
  
  write.xlsx(drug_3,file = z)
  
}

# Changes in groups
drug_f_group(x = "VKA", y = "VKA.png", z = "D:/covid_drugs/Tables_category/VKA.xlsx")
drug_f_group(x = "Ant-Ca", y = "Ant-Ca.png", z = "D:/covid_drugs/Tables_category/Ant_Ca.xlsx")
drug_f_group(x = "antiplatelets", y = "antiplatelets.png", z = "D:/covid_drugs/Tables_category/antiplatelets.xlsx")

# No significant changes in group
drug_f_group(x = "ACEI", y = "ACEI.png", z = "D:/covid_drugs/Tables_category/ACEI.xlsx")
drug_f_group(x = "IHS_drugs", y = "IHS_drugs.png", z = "D:/covid_drugs/Tables_category/IHS_drugs.xlsx")
drug_f_group(x = "NOAC", y = "NOAC.png", z = "D:/covid_drugs/Tables_category/NOAC.xlsx")
drug_f_group(x = "OAD", y = "OAD.png", z = "D:/covid_drugs/Tables_category/OAD.xlsx")
drug_f_group(x = "alpha-blockers", y = "alpha-blockers.png", z = "D:/covid_drugs/Tables_category/alpha_blockers.xlsx")
drug_f_group(x = "antiarrithmic", y = "antiarrithmic.png", z = "D:/covid_drugs/Tables_category/antiarrithmic.xlsx")
drug_f_group(x = "antidislipidemic", y = "antidislipidemic.png", z = "D:/covid_drugs/Tables_category/antidislipidemic.xlsx")
drug_f_group(x = "diuretics", y = "diuretics.png", z = "D:/covid_drugs/Tables_category/diuretics.xlsx")
drug_f_group(x = "beta-blockers", y = "beta-blockers.png", z = "D:/covid_drugs/Tables_category/beta_blockers.xlsx")
drug_f_group(x = "fibrat", y = "fibrat.png", z = "D:/covid_drugs/Tables_category/fibrat.xlsx")
drug_f_group(x = "heparin", y = "heparin.png", z = "D:/covid_drugs/Tables_category/heparin.xlsx")
drug_f_group(x = "insulins", y = "insulins.png", z = "D:/covid_drugs/Tables_category/insulins.xlsx")
drug_f_group(x = "sartan", y = "sartan.png", z = "D:/covid_drugs/Tables_category/sartan.xlsx")
drug_f_group(x = "statins", y = "statins.png", z = "D:/covid_drugs/Tables_category/statins.xlsx")
drug_f_group(x = "tyroxyne", y = "tyroxyne.png", z = "D:/covid_drugs/Tables_category/tyroxyne.xlsx")


### Combined drugs categories

drug_f_group(x = "OAD_combined", y = "OAD_combined.png", z = "D:/covid_drugs/Tables_combined/OAD_combined.xlsx")
drug_f_group(x = "antihypertensive_combined", y = "antihypertensive_combined.png", z = "D:/covid_drugs/Tables_combined/antihypertensive_combined.xlsx")
drug_f_group(x = "statin_combined", y = "statin_combined.png", z = "D:/covid_drugs/Tables_combined/statin_combined.xlsx")
