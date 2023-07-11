mediaF <- xResidenza %>%
  filter(SESSO == "F")%>%
  group_by(AnnoA)%>%
  summarise(IscrittiFemmine = round(mean(Isc), digits = 2))%>%
  distinct()

mediaM <- xResidenza %>%
  filter(SESSO == "M")%>%
  group_by(AnnoA)%>%
  summarise(IscrittiMaschi = round(mean(Isc), digits = 2))%>%
  distinct()

medieSesso <- mediaF %>%
  right_join(mediaM, by=c("AnnoA"="AnnoA"))
medieSesso <- melt(medieSesso,id.vars="AnnoA")
medieSesso <- medieSesso %>%
  rename(Iscritti = value,
         Sesso = variable)

ggplot(medieSesso, aes(x=AnnoA, y=Iscritti, fill=Sesso)) +
  geom_bar(stat='identity', position='dodge')+
  theme(axis.text.x = element_blank())
  
dbCompleto[which.max(dbCompleto$PercentIscritti),]

UdineMF <- iscrittiProvinciaMF("UDINE")
ggplot(UdineMF, aes(AnnoA, Isc, fill=SESSO))+
  geom_bar(stat = "identity", position = 'dodge', width = 1)+
  theme(axis.text.x = element_blank())

