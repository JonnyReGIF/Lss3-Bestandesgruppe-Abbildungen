setwd('C:/Users/Herbert/Desktop/Land System Science 3/')
library(readxl)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(reshape2)
library(pracma)
library(tidyverse)
library(ggtext)

# Sheets im Format: Wochen als x-Wert, Bestandshöhe(x̄) für y1, Bedeckungsgrad(x̄) & 
# SD des Bedeckungsgrades für y2

# Interpolation für Darstellung kontinuierlicher geomline Bestandshöhe
Mais <- read_excel('lss3_daten.xlsx', 1)
Mais$Interpol <- with(Mais, interp1(Woche, Bestandshöhe, Woche, "linear"))
WG <- read_excel('lss3_daten.xlsx', 2)
WG$Interpol <- with(WG, interp1(Woche, Bestandshöhe, Woche, "linear"))
Soja <- read_excel('lss3_daten.xlsx', 3)
Soja$Interpol <- with(Soja, interp1(Woche, Bestandshöhe, Woche, "linear"))
Haf <- read_excel('lss3_daten.xlsx', 4)
Haf$Interpol <- with(Haf, interp1(Woche, Bestandshöhe, Woche, "linear"))
Erb <- read_excel('lss3_daten.xlsx', 5)
Erb$Interpol <- with(Erb, interp1(Woche, Bestandshöhe, Woche, "linear"))
Boh <- read_excel('lss3_daten.xlsx', 6)
Boh$Interpol <- with(Boh, interp1(Woche, Bestandshöhe, Woche, "linear"))
WW_D <- read_excel('lss3_daten.xlsx', 7)
WW_D$Interpol <- with(WW_D, interp1(Woche, Bestandshöhe, Woche, "linear"))
WW_E<- read_excel('lss3_daten.xlsx', 8)
WW_E$Interpol <- with(WW_E, interp1(Woche, Bestandshöhe, Woche, "linear"))
Kart <- read_excel('lss3_daten.xlsx', 9)
Kart$Interpol <- with(Kart, interp1(Woche, Bestandshöhe, Woche, "linear"))
Dur <- read_excel('lss3_daten.xlsx', 10)
Dur$Interpol <- with(Dur, interp1(Woche, Bestandshöhe, Woche, "linear"))
Din <- read_excel('lss3_daten.xlsx', 11)
Din$Interpol <- with(Din, interp1(Woche, Bestandshöhe, Woche, "linear"))
SW <- read_excel('lss3_daten.xlsx', 12)
SW$Interpol <- with(SW, interp1(Woche, Bestandshöhe, Woche, "linear"))


#plot bestandshöhe & SD-Bedeckugnsgrad
#https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
#https://r-graph-gallery.com/4-barplot-with-error-bar.html

#Farbschema
#(Mais,WG,Soja,Haf,Erb,Boh,WW_D,WW_E,Kart,Dur,Din,SW)

#("Mais","Wintergerste","Soja","Hafer","Erbse","Bohne",
# "Winterweizen Dichter","Winterweizen Edelmann","Kartoffel",
# "Durum","Dinkel","Sommerweizen Lennox")

colors <- c("turquoise4", "salmon4", "magenta3", "yellow3", "purple4", "purple",
            "salmon", "coral3", "green4", "gold4", "orange", "yellow")

#Koeffizient für Verhältnis y1 zu y2 
coeff <- 0.2

ggplot((Mais), aes(x=Woche)) +
  geom_line( aes(y=Interpol), size = 3, color="grey70") + 
  geom_point( aes(y=Bestandshöhe), size = 3, color="black") +
 # geom_line( aes(y=Standardabweichung / coeff), size = 2, color=S_Color) +
  geom_bar( aes(y=Standardabweichung / coeff), stat="identity", size=.1, width=0.75, fill= "gold4", color="black", alpha=.4) + 

    # Custom the Y scales:
  scale_x_continuous(breaks = seq(0, 27, 1)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Höhe [cm]",
    breaks = seq(0, 200, 25),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="SD [%]", breaks = seq(0, 100, 5))
  ) +
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = "grey50", size=13),
    axis.title.y.right = element_text(color =  "gold4", size=13),
    axis.line = element_line(size = 0.5, colour = "black", linetype=1),
    axis.text=element_text(size=12)
  ) +
  ggtitle(label = "<span style='font-size: 18pt;'>Bestandshöhe & SD-Bedeckungsgrad - Durum</font>") +
  theme(plot.title = element_markdown())





#plot mit bedeckungsgrad + standardfehler


coeff <- 8/15



ggplot((Mais), aes(x=Woche)) +
  geom_line( aes(y=Interpol), size = 3, color="grey70") + 
  geom_point( aes(y=Bestandshöhe),size = 3, color="black") +
  # geom_line( aes(y=Standardabweichung / coeff), size = 2, color=S_Color) +
  geom_bar( aes(y=Bedeckungsgrad / coeff), stat="identity", size=.1, width=0.75, fill="turquoise4", color="black", alpha=.4) + 
  geom_errorbar( aes(ymin= (Bedeckungsgrad+Standardabweichung) /coeff, ymax= (Bedeckungsgrad-Standardabweichung) /coeff), width=0.4, color="grey20", alpha=0.9, size=1) +
  # Custom the Y scales:
  scale_x_continuous(breaks = seq(0, 27, 1)) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Höhe [cm]", 
    breaks = seq(0, 200, 25),
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Bedeckungsgrad [%]", breaks = seq(0, 100, 10))
  ) +
  
  theme_ipsum() +
  theme_light(base_size = 26) +
  theme(

    axis.title.y = element_text(color = "grey50", size=26),
    axis.title.y.right = element_text(color = "turquoise4", size=26),
    axis.line = element_line(size = 0.5, colour = "black", linetype=1)
    
  ) +
  ggtitle(label = "<span style='font-size: 22pt;'>Bestandshöhe & Bedeckungsgrad mit SD - Mais</font>") +
  theme(plot.title = element_markdown())



