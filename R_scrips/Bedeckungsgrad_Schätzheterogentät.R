###LSS3###
#Datenbereinigung: sheet WW-E: Bedeckungsgrad1/03.05.23 geändert von 977 zu 97
                  #sheet WG: Bedeckungsgrad3:/03.05.23 geändert von 330 auf 33

#Statistische Auswertung:
#Ziel: Plot der zeigt wie unterschiedlich (innerhalb einer Gruppe der Bedeckungsgrad geschätzt wurde)
        #--> zwillingsskript zu: Bedeckungsgrad_Feldheterogenität.R (zeigt wie heterogen das Feld ist)
          #--> Vorsicht: nicht beide Skripts parallel laufen lassen, doppelbennenungen möglich!


#Benötigt: die Abweichung vom Mittel des Bedeckungsgrades pro raster, diese dann gerundet per datum und per Crops
#Vorgehen: im Loop findet berechnung pro crop statt und wird anschließend zu einem df zusammengeführt


#Erstellt: Annika

#Setup
library(tidyverse)
library(readxl)

wd<-("C:/Users/aerte/Desktop/R")
  #("C:/Users/aerte/OneDrive - Ecoselva e.V/00_MASTER/Master/Module/Semester 2/LSS3/R")

setwd(wd)


#Händisch Infos
#Namen der Feldfrüchte
#cropnames<-c("Mais", "Wintergerste", "Soja", "Hafer", "Erbse", "Bohne", "W-Weizen D", "W-Weizen E", "Kartoffel", "Durum", "Dinkel", "S-Weizen")
Feldfrucht<-c("S-Weizen", "Dinkel", "Durum", "Kartoffel", "W-Weizen E", "W-Weizen D", "Bohne", "Erbse", "Hafer", "Soja", "Wintergerste", "Mais")



###load data#
#liste aller sheets für for loop
mydata <- lapply(1:12, read_excel, path="BESTAND2.xlsx",skip = 1)



#leerer Vektor um Ergebnisse aus Loop zu speichern
Schätzheterogenität<- c()


###Loop####
for (i in 1: length(mydata)){

#Objekt aus Liste auswählen für einzelne Berrechnung
bestand<-mydata[i]
  
#nice data
bestand<- as.data.frame(bestand)
bestand$Datum <- as.POSIXct(bestand$Datum)

#"NA" to NA
bestand <- data.frame(apply(
  bestand,1:2,function(x) 
    if( x %in% 'NA') 
      return(NA) 
  else return(x)))

#"0" zu NA
bestand<-data.frame(apply(
  bestand,1:2,function(x) 
    if( x %in% '0') 
      return(NA) 
  else return(x)))

#zu numeric
bestand[,2:37] <- sapply(bestand[,2:37],as.numeric)




###Bedeckungsgrad Schätzunterschiede (SD) ####
# zeigt Heterogenität der Schätzungen
#sd pro raster(1-5) und datum
sd_bedeckung1<-apply(bestand[,13:17], FUN = sd, 1)
sd_bedeckung2<-apply(bestand[,18:22], FUN = sd, 1)
sd_bedeckung3<-apply(bestand[,23:27], FUN = sd, 1)
sd_bedeckung4<-apply(bestand[,28:32], FUN = sd, 1)
sd_bedeckung5<-apply(bestand[,33:37], FUN = sd, 1)


#Bind cols
sd_bedeckung<- cbind(sd_bedeckung1,sd_bedeckung2,sd_bedeckung3,sd_bedeckung4,sd_bedeckung5)


#Average SD per date
sd_bedeckung_date <- rowMeans(sd_bedeckung)

#SD per average sd Schätzrahmen, per date
Schätzheterogenität <- cbind(sd_bedeckung_date, Schätzheterogenität)

}



#rm(list=setdiff(ls(), "mydata"))

#include date 
Schätzheterogenität_date <- cbind(bestand["Woche"], Schätzheterogenität)
colnames(Schätzheterogenität_date)<-c("Woche", Feldfrucht)



#rename columns1
colnames(Schätzheterogenität_date)<-c("Woche", Feldfrucht)

#write.csv(bedeckung_all_crops_date, "Bedeckungsgrad_Schätzheterogenität.csv", row.names = F)




###PLOT####

#preparing: to long format + making id
bedeckung<-pivot_longer(Schätzheterogenität_date, cols = 2:13, names_to = "Feldfrucht", values_to = "Sd")
bedeckung$id<-c(1:nrow(bedeckung))
bedeckung$Feldfrucht<-as.factor(bedeckung$Feldfrucht)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 4
to_add <- data.frame(matrix(NA, empty_bar*nlevels(bedeckung$Feldfrucht), ncol(bedeckung)) )
colnames(to_add) <- colnames(bedeckung)
to_add$Feldfrucht <- rep(levels(bedeckung$Feldfrucht), each=empty_bar)
data <- rbind(bedeckung, to_add)
data <- data %>% arrange(Feldfrucht)
data$id <- seq(1, nrow(data))


#Umsortieren der Level (anders auftauchen im Plot)
data$Feldfrucht <- factor(data$Feldfrucht, levels = c("Dinkel", "Durum", "Hafer", "S-Weizen", "W-Weizen D", "W-Weizen E",
                                            "Wintergerste", "Bohne", "Erbse", "Soja", "Kartoffel", "Mais"))
data$id <- factor(data$id, levels = data$id[order(data$Feldfrucht)])

#Wochenspalte so, dass nur 1 Wert pro Feldfrucht existiert - damit sie im Plot nicht übereinander geschrieben sind
data$Woche[data$Woche == 22] <- '18-27'
data$Woche[data$Woche != '18-27'] <- ""
data$Woche[is.na(data$Woche)] <- ""


#FARBPallette
col_assignment <- 
  data.frame(
    cols = c("orange", "gold4", "yellow3", "yellow", "salmon", "coral3", "salmon4", "purple", "purple4", "magenta3", "green4", "turquoise4" ),
    Feldfrucht = levels(data$Feldfrucht)
  )


# Make the plot
plot_Schätzhetorgenität<-
ggplot(data, aes(x= id, y= Sd, fill = Feldfrucht)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar( stat="identity", alpha=1) +
  labs(title = "Schätzheterogenität", 
       subtitle = "Gemittelte Standardabweichung über alle Schätzrahmen \npro Feldfrucht und Messdatum",
      # caption = "Die Feldfruchtart ist farbkodiert, die Angabe über die jeweilige Kalenderwoche befindet sich im äußersten \nBereich des kreisförmigen Diagrams. Die Länge der Balken zeigt die jeweilige gemittelte Standardabweichung \nüber alle Plots pro Feldfucht und Messdatum. \nZur Berechnung der gemittelten Standardabweichung wurde zunächst die Standardabweichung je \nSchätzrahmen berechnet und anschließend pro Aufnahmedatum/Kalenderwoche und Feldfrucht gemittelt. \nSomit lässt sich beurteilen, wie einheitlich in Abhängigkeit von dem jeweiligen Wachstumsgrad der Feldfrucht \nder Bedeckungsgrad geschätzt wurde. Bei geringer Standardabweichung sind sich die jeweiligen Schätzungen \nnäher, bei höherer Standardabweichung sind die Schätzungen weiter voneinander entfernt. "
  ) +
  ylab("Standardabweichung") +
  xlab("Kalenderwoche")+
   theme(
    axis.title.x = element_text(vjust = -2),
    axis.text.x = element_text(),
    plot.caption.position = "plot",
  #  plot.margin = unit(rep(1,8), "cm"),
  ) +
 scale_fill_manual(values = col_assignment$cols) +
  scale_y_continuous(limits = c(0, 7), expand = c(0, 0))+
  scale_x_discrete(expand = c(.05, .05), labels = data$Woche)
ggsave(plot = plot_Schätzhetorgenität, filename = "plot_Schätzhetorgenität_Bedeckungsgrad.jpg", path = paste0(wd,"/" ,"Plots"), dpi = 500, width = 17, height = 12, units = "cm")




summary(data)
  

