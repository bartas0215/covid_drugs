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
