#This script grabs data from a public google sheet and public image on my google drive plots the most recent E. coli sample results on column graph 
#and on a map of the field area and the watersheds for each sample site (Woods Creek and its larger tributary, Sarahs Run in Lexington, Virginia, 
#USA) for the most recently collected samples. The dotted horizontal lines and letters (A-D) are "grades" that are used by the Boxerwood Education 
#Center to provide context to the numbers. As of 10/13/2017, the data in the google sheet for this script are not real.

library(magick)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(gsheet)

url <- 'docs.google.com/spreadsheets/d/1R74GXZrupU3XRiOxS2oTsoF6gJvFwXSbHXnnvrhHXAw' #this is the public url for the google sheet that we are using
WCdata <- gsheet2tbl(url) #read google sheet to data table

img <- image_read("https://drive.google.com/uc?id=0B_OqDEdd_ScZaEtxTHU5NUYwVWM") #read in the background image from google drive

WCadd <- data.frame(Location=c("WGC1", "TSC1", "WRR2", "TSRM", "TSBH", "TSR2", "WWA3", "WPL4", "TBQ3", "WBQ5", "WTU6", "WPS7", "WJP8", "TOWN"), #this will add essential location information to the data frame
	Longitude=c(-79.45753333, -79.45696944, -79.455632611327, -79.450956, -79.448967, -79.453319868894, -79.452474776952, -79.45004722, -79.4500804, -79.44928918, -79.44576389, -79.44331944, -79.43033056, -79.42953611),
	Latitude=c(37.77768889, 37.7786, 37.7795066394332, 37.7709797, 37.772711, 37.7806703770563, 37.7828077174318, 37.78456667, 37.78485123, 37.78536349, 37.78863056, 37.78971667, 37.79153056, 37.78976944),
	Distance=c(2.37, 2.28, 2.2, 2.01, 2.02, 2.03, 1.89, 1.68, 1.67, 1.6, 1.24, 1.1, 0.14, 0.06),
	Category=c("wc", "tributary", "wc", "tributary", "tributary", "tributary", "wc", "wc", "tributary", "wc", "wc", "wc", "wc", "tributary"))

WCdata <- merge(WCdata, WCadd, by="Location") #merge in the month

WCdata$x <- 3849 * ((WCdata$Longitude + 79.495462) / 0.0714470000000063) #calculate longitude into pixel x
WCdata$y <- 3384 * ((WCdata$Latitude - 37.748491) / 0.0496729999999985) #calculate latitude into pixel y

WCdata$DATE <- as.Date(WCdata$Date, "%m/%d/%Y") #define date as a date

WCcsldms <- subset(WCdata, ID =="CSLDMS") # subset data to include only data from Chuck Smith's class

WCcsldms$mile <- WCcsldms$Distance 
WCcsldms$Order <- reorder(WCcsldms$Location, WCcsldms$mile) #this orders the x axis

WCcsldms$date <- as.character(WCcsldms$DATE)

latestdate <- max(WCcsldms$DATE) #find and define value of most recent date
WClatest <- subset(WCcsldms, DATE==latestdate) #subset data table to include only 

map <- ggplot ((WClatest), 
    aes (x = x, y = y, colour = Ecoli))+ 
    annotation_custom(rasterGrob(img, width=unit(1,"npc"), 
    height=unit(1,"npc")), 0, 3849, 0, 3384) + coord_equal() + 
    geom_label_repel(aes(x, y, fill = Category, label = Location),
    alpha=0.75,
    fontface = 'bold', color = 'white', box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"), segment.color = 'white') + 
    scale_colour_gradientn(colours = rev(rainbow(3.5))) + 
    geom_point(size=8) + 
    geom_point(shape = 1,size = 8,colour = "black") + 
    xlim(0, 3849) + ylim(0, 3384) + 
   scale_fill_manual(values=c("#66cc99", "#999999", "#cc6666")) +
    theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())

Ecolicolumn <- ggplot(WClatest, aes(x = Order, y = Ecoli, fill = Category)) + 
   	geom_col() + 
	theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_blank(), legend.position="none")+
	scale_fill_manual(values=c("#66cc99", "#999999", "#cc6666")) +
   	geom_text(label = " ") + annotate("text", label = "A", x = 0.5, y = 69, size = 4, color = "red", hjust = 1) +
     	annotate("text", label = "B", x = 0.5, y = 195, size = 4, color = "red", hjust = 1) +
     	annotate("text", label = "C", x = 0.5, y = 321, size = 4, color = "red", hjust = 1) +
     	annotate("text", label = "D", x = 0.5, y = 467, size = 4, color = "red", hjust = 1) +
     	annotate("text", label = "F", x = 0.5, y = 625, size = 4, color = "red", hjust = 1) +
   	geom_hline(yintercept=136, linetype="dashed", color = "red") + 
   	geom_hline(yintercept=254, linetype="dashed", color = "red") +
   	geom_hline(yintercept=388, linetype="dashed", color = "red") +
   	geom_hline(yintercept=546, linetype="dashed", color = "red") +
   	labs(title="Samples from:", subtitle = latestdate, y="E. coli (cfu/100mL)", x="sample site")

grid.arrange(Ecolicolumn, map, ncol=2, nrow=1)
