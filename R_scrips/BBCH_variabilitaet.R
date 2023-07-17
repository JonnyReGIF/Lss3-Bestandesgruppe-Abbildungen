library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(viridis)



plot_bbch_range = function(kultur, colooor, long_name){
  
  
  data <- readxl::read_excel("D:\\Master\\BESTAND2.xlsx", sheet = kultur, skip = 1, col_types = c("date", rep("numeric", 36)))
  
  # extrat BBCH clos
  BBCH = data[2:7]
  
  # format the data for plotting
  data_long <- tidyr::pivot_longer(
    data,
    cols = starts_with("BBCH"),
    names_to = "BBCH",
    values_to = "Range"
  )
  
  # calc min and max bbch for each week 
  data_prep =data_long %>%
    group_by(Woche) %>% 
    summarise(min1 = min(Range), max1 = max(Range))
  
  dodge <- position_dodge(width=0.9)
  
  title1 =  "Spannweite der BBCH-Schätzungen für die Kultur:"
  
  # actual plotting 
  ggplot(data_prep, aes(x  = as.factor(Woche), y = max1))+
    geom_errorbar(aes(ymin = min1, ymax = max1, color= colooor), width = 0.5, position = dodge, size = 1.2)+
    #geom_point()+
    xlab("Woche")+
    ylab("BBCH")+
    scale_y_continuous(limits = c(0, 103), breaks = seq(0, 103, by = 10), expand = c(0, 0))+
    scale_x_discrete(expand = c(.1, .1)) +
    geom_hline(yintercept = 10, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 10, label = "MKS 0", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 20, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 20, label = "MKS 1", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 30, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 30, label = "MKS 2", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 40, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 40, label = "MKS 3", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 50, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 50, label = "MKS 4", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 60, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 60, label = "MKS 5", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 70, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 70, label = "MKS 6", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 80, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 80, label = "MKS 7", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 90, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 90, label = "MKS 8", vjust = 1.2, size = 2.2)+
    geom_hline(yintercept = 100, linetype = 2, size = 0.2)+
    annotate("text", x = as.factor(23), y = 100, label = "MKS 9", vjust = 1.2, size = 2.2)+
    labs(title = paste(title1, "\n" ,long_name, "(n = 5)"))+
    theme_bw()+
    theme(legend.position = "none")
  
  ggsave(paste0("D:\\Master\\plots_lss3\\", kultur, ".png"))  
}



# get crop names (abbreviations) to iterate over the .xls
crop_names = readxl::excel_sheets("D:\\Master\\BESTAND2.xlsx")

# Full names for ggtitle 
crop_names_long = c("Dinkel", "Durum", "Hafer", "S-Weizen", "W-Weizen D", "W-Weizen E",
  "Wintergerste", "Bohne", "Erbse", "Soja", "Kartoffel", "Mais")

te = c("Mais", "W-Gerste", "Soja", "Hafer","Erbse", "Bohne", "W-Weizen D", "W-Weizen E", "Kartoffel", "Durum", "Dinkel", "S-Weizen" )


length(te) == length(crop_names)

# colors for plotting
crop_color = c("orange", "gold4", "yellow3", "yellow", "salmon",
               "coral3", "salmon4", "purple", "purple4", "magenta3",
               "green4", "turquoise4" )


for (i in 1:length(crop_names)) {
  
  print(crop_names[i])
  print(crop_color[i])
  print(te[i])
  
  plot_bbch_range(kultur = crop_names[i], colooor = crop_color[i], long_name = te[i])
  
  
}





paste(title1, "\n", te[1] )







