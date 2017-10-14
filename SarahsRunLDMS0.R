#run this before running SarahsRunLDMS1 either:
	#the first time using R on a thawed computer 
	#-or-
	#everytime on a frozen computer

install.packages("magick") #for making a raster out of an image
install.packages("ggplot2") #for basic (I mean awesome) plotting
install.packages("grid") #for custom annotation via rasterGrob
install.packages("gridExtra") #for plotting two or more things at once
install.packages("ggrepel") #for plotting labels that do not overlap
install.packages('gsheet') #for accessing a google sheet via its url