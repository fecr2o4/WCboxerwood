#This script grabs data from a public google doc and plots the most recent water temperature (left) and conductivity (center) of 
#Woods Creek (Lexington, Virginia, USA) and some of its tributaries and a comparison of these values to samples collected back to
#2016-10-25. Samples are mostly collected by me during my morning run and sometimes collected by local school children.

library(gsheet)
library(ggplot2)
library(dplyr)
library(ggrepel)

url <- 'docs.google.com/spreadsheets/d/1Ta_BlBy3bSQ1UzNJwtL8NnRG0ZAoGcxPxR8w7DeXWZU' #this is the public url for the google sheet that we are using
WC <- gsheet2tbl(url) #read google sheet to data table

WC$DATE <- as.Date(WC$date, "%m/%d/%Y")
WC <- subset(WC, ID=="pLow")
WC$location <- reorder(WC$location, WC$distance) 
latestvalue <- max(WC$DATE)
latest <- subset(WC, DATE==latestvalue)
tribs <- subset(latest, cat=="trib")
main <- subset(latest, cat=="wc")
latestairT <- mean(latest$airT)
maxdist <- max(latest$distance)
WCstats <- WC %>%
	group_by(location) %>%
	summarise(n_distinct(DATE))

WC <- merge(WC,WCstats,by="location")
colnames(WC)[18] <- "n" #renames the 16th column
head(WC, n=10)
boxtitle <- as.character(latestvalue)
boxtitle <- paste(boxtitle, "(colored circles) and previous dates (n at y = 0)")

cond <- ggplot()+
	geom_vline(data = tribs, aes(xintercept = distance), linetype = "dashed", color = "darkgreen") +
	geom_point(data = latest, aes(x = distance, y = C25, size = area2), alpha = 0.5, color = "white") +
	geom_point(data = latest, aes(x = distance, y = C25, color = cat, size = area2), alpha = 0.5) +
	geom_label_repel(data = latest, aes(x = distance, y = C25, label=location)) +	
	labs(title=latestvalue, y = "specific conductance (uS/cm)", x = "distance upstream (miles)") + 
	theme_bw() +
	theme(legend.position="none")

temp <- ggplot()+
	geom_vline(data = tribs, aes(xintercept = distance), linetype = "dashed", color = "darkgreen") +
	geom_point(data = latest, aes(x = distance, y = temp, size = area2), color = "white") +
	geom_point(data = latest, aes(x = distance, y = temp, color = cat, size = area2), alpha = 0.5) +
	geom_hline(data = tribs, aes(yintercept = airT), size = 1, alpha = 0.5, color = "lightblue") +
	geom_label_repel(data = latest, aes(x = distance, y = temp, label=location)) +	
	annotate(geom="text", x=maxdist <- max(latest$distance), y=latestairT, label="air temperature", color="lightblue", vjust=1, hjust=1) +
	labs(title=latestvalue, y = "water temperature (F)", x = "distance upstream (miles)") + 
	theme_bw() +
	theme(legend.position="none")

condbox <- ggplot() + 
	geom_boxplot(data = WC, aes(x=location, y = C25, fill = cat), alpha = 0.5)+
	geom_point(data = latest, aes(x=location, y = C25), size = 4, , colour = "white")+
	geom_point(data = latest, aes(x=location, y = C25, colour = cat), size = 4, alpha = 1.0)+
	geom_point(data = latest, aes(x=location, y = C25), size = 4, , colour = "black", shape = 1) +
	geom_label(data = WC, aes(x = location, y = 0, label = n, color = cat), position = position_stack(vjust = 0.5)) +
	labs(title=boxtitle, y = "specific conductance (uS/cm)", x = "site") + 
	theme_bw() 

grid.arrange(temp, cond, condbox, ncol=3, nrow=1)
