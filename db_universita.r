library(dplyr)
xResidenza <- read.csv2("Datasets/07_iscrittixresidenza.csv")

iscrittiByProvincia<- function(Provincia){
  x <- xResidenza %>%
    filter(ResidenzaP==Provincia)%>%
    group_by(AnnoA)%>%
    summarise(ResidenzaP = ResidenzaP,IscrittiTot = sum(Isc),.groups = 'drop')%>%
    distinct()
  return(x)
  }

iscrittiByAnno <- function(Anno){
  x <- xResidenza %>%
    filter(AnnoA == Anno)
  return(x)
}

iscrittiTotPerAnno <- function(){
  x <- xResidenza %>%
    group_by(AnnoA,ResidenzaP)%>%
    summarise(ResidenzaP = ResidenzaP, IscrittiTot = sum(Isc))%>%
    distinct()
  return(x)
}


