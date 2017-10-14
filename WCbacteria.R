#This scrip grabs data from a public google doc and public image on my google drive plots the most recent E. coli concentrations on a map (left) with color-coded points,
#on a bar (column) graph (center top) for the most recent sample collection date, on a box plot for sample sites on Woods Creek upstream of Sarahs Run, from Sarahs Run,
#and Woods Creek downstream of Sarahs Run for all sample collection dates (center bottom), and on a box plot for all samples for all sites (right). Samples are collected 
#and analysed (counted) by a group of volunteers on the second Tuesday of each month for July 2017 to June 2018

#In a box plot, the black horizontal bar = median, boxes = 25th and 75th percentiles, whiskers (vertical black bars) = 1.5 times the interquartile range, black dots = outliers (may not be present). These are the default settings for R ggplot2 geom_boxplot.
#dotted line = water quality standard of 235 cfu/100mL used by the VADEQ as the standard that should be exceeded no more than 10.5% of the time at any given sample location
#weather for 2017-07-11 was hot and sunny with no rain in the past few days
#the test for SR01 failed in July

library(magick)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggrepel)
library(gsheet)

url <- 'docs.google.com/spreadsheets/d/1SB2jSLL9htBb1NfGJkTa6s3zMeu453LbbFipQAGTT78' #this is the public url for the google sheet that we are using
WCbact <- gsheet2tbl(url) #read google sheet to data table

WCbact$Ecoli <- as.numeric(WCbact$Ecoli)

mindistbact <- min(WCbact$mile)
maxdistbact <- max(WCbact$mile)

img <- image_read("https://drive.google.com/uc?id=0B_OqDEdd_ScZQW5McWxuZzVRMkk") #read in the background image from google drive

hbact <- 1828 # image height
wbact <- 1828 # image width

WCbact$x <- wbact * ((WCbact$lon + 79.46286109) / 0.0385730699999982)
WCbact$y <- hbact * ((WCbact$lat - 37.7664652) / 0.0305101499999978)

WCbact$negmile <- WCbact$mile * -1
WCbact$site <- reorder(WCbact$site, WCbact$negmile) #this orders the x axis

WCbact$DATE <- as.Date(WCbact$date, "%m/%d/%Y")
latestvalue <- max(WCbact$DATE)
latestbact <- subset(WCbact, DATE==latestvalue)

earliestvalue <- min(WCbact$DATE)
daterange <- paste("monthly",earliestvalue, "to", latestvalue)

WCbact$julian <- format(WCbact$DATE, "%j")
WCbact$julian <- as.numeric(WCbact$julian)

WCcat2 <- data.frame(site=c("WC01", "WC02", "WC03", "WC04", "GC01", "GC02", "SS01", "SR01", "SR02", "SR03", "BC01", "TB01"),
	category2=c("upstream", "upstream", "downstream", "downstream", "other", "other", "other", "Sarah", "Sarah", "Sarah", "other", "other"),
	order=c(1,1,3,3,4,4,4,2,2,2,4,4))

WCbact <- merge(WCbact,WCcat2, by = "site")

WCbactcat2 <- subset(WCbact, category2 !="other")
WCbactcat2$cat2date <- paste(WCbactcat2$category2, WCbactcat2$DATE)
WCbactcat2$cat2date <- reorder(WCbactcat2$cat2date, WCbactcat2$order) #this orders the x axis

title1 <- paste("Samples from:", latestvalue)
title2 <- paste("Samples collected", daterange)

mapbact <- ggplot (subset(latestbact,category %in% c("tributary","WC")), 
    aes (x = x, y = y, colour = Ecoli))+ 
    labs(title=title1) +
    annotation_custom(rasterGrob(img, width=unit(1,"npc"), 
    height=unit(1,"npc")), 0, wbact, 0, hbact) + coord_equal() + 
    geom_label_repel(aes(x, y, fill = category, label = site),
    alpha=0.75,
    fontface = 'bold', color = 'white', box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"), segment.color = 'white') + 
    scale_colour_gradientn(colours = rev(rainbow(3.5))) + 
    geom_point(size=5) + 
    geom_point(shape = 1,size = 5,colour = "black") + 
    xlim(0, wbact) + ylim(0, hbact) + 
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

noEcoli <- subset(latestbact, Ecoli==0)
noEcolidata <- latestbact[is.na(latestbact$Ecoli),]
noEcolidata[is.na(noEcolidata)] <- -0.01

Ecolicol <- ggplot(latestbact, aes(x = site, y = Ecoli, fill = category)) + 
	geom_col() + theme_bw() +
	scale_fill_manual(values=c("#66cc99", "#999999", "#cc6666")) +
#	annotate(geom="text", x=1, y=235, label="WQS", color="black", fontface="bold", vjust=0.5)+
	geom_hline(yintercept=235, linetype="dashed", color = "black") +
	labs(title=title1, x="sample site",y="E. coli (cfu/100mL)") +
	theme(panel.background = element_rect(fill = "#fafafa"), axis.line=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_blank(),	
	axis.text.x=element_text(vjust=0), legend.position= c(0, 1), legend.justification = c(0, 1), legend.background = element_rect(fill=alpha('blue', 0.0))) +
 	theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#	geom_text(data=noEcoli, mapping=aes(x=site, y = Ecoli, label=Ecoli), color="black", size = 4, fontface="bold", vjust=0, hjust=0.5) + 
	   geom_text(data=noEcoli, mapping=aes(x=site, y = Ecoli, label=Ecoli), color="black", size = 4, fontface="bold", vjust=0, hjust=0.5) 

Ecolibox <- ggplot(WCbact, aes(x = site, y = Ecoli, fill = category)) + 
	geom_boxplot() + theme_bw() +
	scale_fill_manual(values=c("#66cc99", "#999999", "#cc6666")) +
	scale_color_manual(values=c("#66cc99", "#999999", "#cc6666")) +
#	annotate(geom="text", x=1, y=235, label="WQS", color="black", fontface="bold", vjust=0.5)+
	geom_point(alpha = 0.25, size = 2)+ 
   	geom_hline(yintercept=235, linetype="dashed", color = "black") +
	labs(title=title2, x="sample site",y="E. coli (cfu/100mL)") +
    	theme(panel.background = element_rect(fill = "#fafafa"), axis.line=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_blank(),	
		axis.text.x=element_text(angle = 90, vjust = 0.5), legend.position= c(0, 1), legend.justification = c(0, 1), legend.background = element_rect(fill=alpha('blue', 0.0)))

timebox <- ggplot() + 
	geom_boxplot(data = WCbactcat2, aes(x= as.factor(DATE), y = Ecoli, group = cat2date, fill = factor(category2, labels = c("WC upstream", "Sarahs Run", "WC Downstream"))), position = "dodge") +
   	geom_hline(yintercept=235, linetype="dashed", color = "black") +
	scale_fill_manual(values=c("#999999", "#66cc99", "#999999")) +
	labs(fill = "", x="date",y="E. coli (cfu/100mL)") +
    	theme_bw() + theme(panel.background = element_rect(fill = "#fafafa"), axis.line=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), plot.background=element_blank(),	
		axis.text.x=element_text(angle = 90, vjust = 0.5), legend.position="top")

grid.arrange(arrangeGrob(mapbact, ncol=1,nrow=1), arrangeGrob(Ecolicol, Ecolibox, ncol=2, nrow=1),heights=c(8,1), widths=c(1,1))

grid.arrange(arrangeGrob(mapbact, ncol=1,nrow=1), arrangeGrob(Ecolicol, timebox, ncol=1, nrow=2), arrangeGrob(Ecolibox, ncol=1, nrow=1), heights=c(8,1,1), widths=c(1,0.5,0.5))

