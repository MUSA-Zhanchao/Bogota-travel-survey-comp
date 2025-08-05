library(readxl)
library(tidyverse)

trip_2015<- read_excel("data/2015/encuesta 2015 - etapas.xlsx")

trip_2015_summary<-trip_2015%>%
  group_by(MEDIOTRASPORTE)%>%
  summarise(count=n())%>%
  mutate(percentage=count/sum(count)*100)

#write.csv(trip_2015_summary, "post-process-data/summary_trip_2015.csv", row.names = FALSE)
  
trip2011<-read.csv("data/2011/Mod_D_VIAJES2_BaseImputacion_Definitiva.csv")
trip2011_summary<-trip2011%>%
  group_by(Modo_Principal)%>%
  summarise(count=n())%>%
  mutate(percentage=count/sum(count)*100)

#write.csv(trip2011_summary, "post-process-data/summary_trip_2011.csv", row.names = FALSE)


trip_2019<-read_excel("data/2019/EtapasEODH2019.xlsx")

trip_2019_summary<-trip_2019%>%
  group_by(p18_id_medio_transporte)%>%
  summarise(count=n())%>%
  mutate(percentage=count/sum(count)*100)

#write.csv(trip_2019_summary, "post-process-data/summary_trip_2019.csv", row.names = FALSE)
