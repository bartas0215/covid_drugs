#Install packages
install.packages("pastecs")

#Load packages
library(pastecs)

# Setting options
options(scipen=999)

### Save plot regarding drug

drug_f <- function(x,y) {

# Filter data 
drug_graph <- df_3 %>%
  filter(Scope == {{x}}) %>%
  ggplot(aes(Date, Availability)) + geom_line(aes(col=kategoria))

# Save file 
ggsave(plot = drug_graph,filename = y,path = "D:/covid_drugs/Images", device = "png", dpi = 600 )

}

drug_f(x = "acenocoumarol", y = "acenocoumarol.png")

          ###FIX###

drug_f_1 <- function(x,z) {

### Performing basic statistics
# Pull Availability
drug_1 <- df_3 %>%
  filter(Scope == {{X}}) %>%
  pull(Availability)

drug_1 <- as_tibble(drug_1)

#Perform basic statistics
drug_2 <- stat.desc(drug_1)

write.xlsx(drug_2,file = z)

}

drug_f_1(x = "acenocoumarol", z = "D:/covid_drugs/Tables/acenocoumarol")



