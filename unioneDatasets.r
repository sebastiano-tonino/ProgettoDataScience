if(!exists("foo", mode="function")) source("db_universita.r")
if(!exists("foo", mode="function")) source("db_casiAnnuale.r")
if(!exists("foo", mode="function")) source("db_residenti.r")

dbCovidProvince <- getCovidPerProvince()
dbIscrizioniUni <- iscrittiTotPercentuale() 
dbCompleto <- dbCovidProvince%>%
  left_join(dbIscrizioniUni, by="ResidenzaP")

regLin2020db <- dbCompleto %>%
  filter(AnnoA == "2020/2021")

reg <- lm(regLin2020db$IscrittiTot~regLin2020db$Totale_casi2020)
summary(reg)

casi <- dbCompleto %>% 
  group_by(ResidenzaP,Totale_casi2020,Totale_casi2021,Totale_casi2022)%>%
  summarise()

province <- dbIscrizioniUni %>%
  group_by(ResidenzaP)%>%
  summarise()

anniA <- dbIscrizioniUni %>%
  group_by(AnnoA) %>%
  summarise()

dbCompletoPerAnno <- function(anno){
  print(anno)
  x <- dbCompleto %>%
    filter(AnnoA == anno)
  return(x)
}

library(dplyr)
library(sf)
library(mapview)
library(ggplot2)












