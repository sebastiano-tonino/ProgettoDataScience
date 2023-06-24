abitanti <- read.csv("datasets/residenti/residenti5.csv")%>%
  select(TIME,Value,Territorio)
abitanti <- abitanti%>%       
  rename(Anno = TIME, Residenti = Value, ResidenzaP = Territorio)%>%
  mutate(ResidenzaP = toupper(ResidenzaP))
abitanti <-  subset(abitanti, Anno!="2019")%>%
  group_by(ResidenzaP)
  
  

unidb <- iscrittiTotPerAnno() %>%
  mutate(Anno = strtoi((substr(AnnoA,1,4))))
abitanti2020 <- read.csv2("datasets/residenti/residente2021.csv")%>%
  select(Territorio, Anno..TIME_PERIOD., Osservazione)%>%
  rename( Anno = Anno..TIME_PERIOD., Residenti = Osservazione, ResidenzaP = Territorio)%>%
  mutate(ResidenzaP = gsub("Provincia di ","",ResidenzaP))%>%
  mutate(ResidenzaP = toupper(ResidenzaP))%>%
  mutate(Residenti =strtoi(gsub(",","",Residenti)))

abitanti2022 <- read.csv("datasets/residenti/residente2022.csv")%>%
  mutate(Anno = 2022)%>%
  select(Provincia, Totale, Anno)%>%
  mutate(Provincia=toupper(Provincia))%>%
  rename( Residenti = Totale, ResidenzaP = Provincia)%>%
  relocate(Anno)
  

abitantidb <- rbind(abitanti, abitanti2020,abitanti2022)

# Correzione errori dataset 
abitantidb <- abitantidb %>%
  mutate(Residenti = ifelse(Residenti < 99999, Residenti*10, Residenti))%>%
  mutate(Residenti = ifelse(Residenti < 99999, Residenti*10, Residenti))%>%
  mutate(Residenti = ifelse(Residenti < 99999, Residenti*10, Residenti))


iscrittiTotPercentuale <- function(){
  x <- abitantidb %>%
    right_join(unidb,
               by=c("ResidenzaP"="ResidenzaP",
                    "Anno"="Anno"))%>%
    mutate(PercentIscritti = round(IscrittiTot*100/Residenti, digits = 2))
  return(x)
}
