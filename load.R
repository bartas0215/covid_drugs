library(tidyverse)
library(readxl)
library(xlsx)
library(trelliscopejs)


# DATA -------------------------------------------------------------------
#Ctrl + Shift + R - create section

drugs <- read_xls("C:/Users/uzytkownik/Dropbox/Badania/Katedra/Gdziepolek_interna/Rozne/lista_lekow_internistycznych.xls")
colnames(drugs)[2] <- 'Scope'

directory <- "P:/Tymczasowe kopie/Gdziepolek_interna1/dostepnosc" 
myfiles_csv <- list.files(path = directory, pattern = "csv",  full.names = TRUE)
l <- list()

time = proc.time()
for (i in 1:length(myfiles_csv)){
  csv <- read_csv(myfiles_csv[i])
  csv2 <- csv %>%
    left_join(drugs, by = 'Scope')
  l[[i]]  <- csv2
}
d <- do.call("rbind", l)
print(proc.time()-time)

d