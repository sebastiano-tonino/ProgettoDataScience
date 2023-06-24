library(dplyr)

# Dati casi covid gennaio settembre 2020,2021,2022

# dati covid da 24-02-2020 a 30-09-20
getDatiCovid2020 <- function(){
  gen20 <- read.csv("Datasets/covid-2020/20200224.csv")
  sett20 <- read.csv("Datasets/covid-2020/20200930.csv")
  
province <- gen20 %>% 
  filter(denominazione_provincia != "In fase di definizione/aggiornamento") %>%
  group_by(denominazione_provincia) %>%
  summarise()
  
  
  columns =   c("Nome_regione","Num_regione","ResidenzaP","Num_prov","Totale_casi","lat","long")
  totCasi2020 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  for (prov in province$denominazione_provincia) {
  
    gen20prov <- filter(gen20, gen20$denominazione_provincia == prov )
    sett20prov <- filter(sett20, sett20$denominazione_provincia == prov )
    p <- c(gen20prov$denominazione_regione, gen20prov$codice_regione, toupper(gen20prov$denominazione_provincia), gen20prov$codice_provincia,
           sett20prov$totale_casi-gen20prov$totale_casi, gen20prov$lat, gen20prov$long)
  
    totCasi2020 <- rbind(totCasi2020,p)
  }
  colnames(totCasi2020) <- columns
  return(totCasi2020)
}


# dati covid da 30-09-2020 a 30-09-21
getDatiCovid2021<-function(){
  sett20 <- read.csv("Datasets/covid-2020/20200930.csv")
  sett21 <- read.csv("Datasets/covid-2021/20210930.csv")
  province <- sett20 %>% 
    filter(denominazione_provincia != "In fase di definizione/aggiornamento" &
             denominazione_provincia != "Fuori Regione / Provincia Autonoma") %>%
    group_by(denominazione_provincia) %>%
    summarise()
  
  columns =   c("Nome_regione","Num_regione","ResidenzaP","Num_prov","Totale_casi","lat","long")
  totCasi2021 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
  for (prov in province$denominazione_provincia) {
    sett20prov <- filter(sett20, sett20$denominazione_provincia == prov )
    sett21prov <- filter(sett21,sett21$denominazione_provincia == prov )
    p <- c(sett20prov$denominazione_regione, sett20prov$codice_regione, 
           toupper(sett20prov$denominazione_provincia), sett20prov$codice_provincia,
           sett21prov$totale_casi - sett20prov$totale_casi
          ,sett20prov$lat, sett20prov$long)
    totCasi2021 <- rbind(totCasi2021,p)
  }
  colnames(totCasi2021) <- columns
  return(totCasi2021)
}
  # dati covid da 30-07-2021 a 30-09-22
getDatiCovid2022<-function(){    
    sett21 <- read.csv("Datasets/covid-2021/20210930.csv")
    sett22 <- read.csv("Datasets/covid-2022/20220930.csv")
    province <- sett21 %>% 
      filter(denominazione_provincia != "In fase di definizione/aggiornamento" &
               denominazione_provincia != "Fuori Regione / Provincia Autonoma") %>%
      group_by(denominazione_provincia) %>%
      summarise()
    
    columns =   c("Nome_regione","Num_regione","ResidenzaP","Num_prov","Totale_casi","lat","long")
    totCasi2022 = data.frame(matrix(nrow = 0, ncol = length(columns))) 
    for (prov in province$denominazione_provincia) {
      sett21prov <- filter(sett21, sett21$denominazione_provincia == prov )
      sett22prov <- filter(sett22,sett22$denominazione_provincia == prov )
      p <- c(sett21prov$denominazione_regione, sett21prov$codice_regione, 
             toupper(sett21prov$denominazione_provincia), sett21prov$codice_provincia,
             sett22prov$totale_casi - sett21prov$totale_casi,
             sett21prov$lat, sett21prov$long)
      totCasi2022 <- rbind(totCasi2022,p)
    }
    colnames(totCasi2022) <- columns
  return(totCasi2022)
} 
getCovidPerProvince <- function(){ 
  totCasi2020 <-getDatiCovid2020()
  totCasi2021 <- getDatiCovid2021()
  totCasi2022 <- getDatiCovid2022()
    covidPerProvince <- cbind(totCasi2020, totCasi2021$Totale_casi, totCasi2022$Totale_casi)
    columnsName =   c("Nome_regione","Num_regione","ResidenzaP","Num_prov","Totale_casi2020","lat","long","Totale_casi2021","Totale_casi2022")
    colnames(covidPerProvince) <- columnsName
    colOrder <- c("Nome_regione","Num_regione","ResidenzaP","Num_prov","lat","long","Totale_casi2020","Totale_casi2021","Totale_casi2022")
    covidPerProvince <- covidPerProvince[,colOrder]
    return(covidPerProvince)
}


getProvince <- function(){
  gen20 <- read.csv("Datasets/covid-2020/20200224.csv")
  province <- gen20 %>% 
    filter(denominazione_provincia != "In fase di definizione/aggiornamento" &
             denominazione_provincia != "Fuori Regione / Provincia Autonoma") %>%
    group_by(denominazione_provincia) %>%
    summarise()
  return(province)
}