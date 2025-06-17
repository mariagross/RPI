
setwd("C:/Users/gross/OneDrive/Desktop/MSc-Paleobio/semester-2/RPI")

#read in Data
data3<- read.csv("RPI_V4.csv")

######################Version 3##############################
#refined ages and aridity measures using model MERDITH2021

#plot paleolat v paleolong
plot(data3$Paleo.Longitude, data3$Paleo.Latitude, 
     xlab="Paleo Longitude", ylab="Paleo Latitude", main= "Paleocoordinates & Climate",
     pch=21, col="black", lwd=1.5, bg="lightgray",xlim=c(-180,180), ylim=c(-90,90), axes=FALSE, cex=0.75)
axis(1, at=seq(from=-180, to=180, by=30))
axis(2, at=seq(from=-90, to=90, by=30))

#add climate points
aridpoints <- subset(data3, Climate == "Arid")
points(aridpoints$Paleo.Longitude,aridpoints$Paleo.Latitude, pch=21, col="#C04000", lwd=1.5, bg="darkorange", cex=0.75)

tropicalpoints <- subset(data3, Climate == "Tropical")
points(tropicalpoints$Paleo.Longitude,tropicalpoints$Paleo.Latitude, pch=21, col="#169893", lwd=1.5, bg="#0CFFF4", cex=0.75)

warmpoints <- subset(data3, Climate == "Warm Temperate")
points(warmpoints$Paleo.Longitude,warmpoints$Paleo.Latitude, pch=21, col="black", lwd=1.5, bg="darkred", cex=0.75)

boreopoints <- subset(data3, Climate == "Boreotropical")
points(boreopoints$Paleo.Longitude,boreopoints$Paleo.Latitude, pch=21, col="#1a7a16", lwd=1.5, bg="green", cex=0.75)

legend(-180,97,legend=c("Arid", "Tropical", "Warm Temperate", "Boreotropical"),  pch = c(21,21,21,21), 
       col = c("#C04000","#169893","black","#1a7a16"), pt.bg = c("#C04000", "#0CFFF4", "darkred", "green"), cex=0.75)

#calculate percentage of climates
arid.perc3 <-((nrow(aridpoints))/(nrow(data3)))*100
arid.perc3

tropical.perc <-((nrow(tropicalpoints))/(nrow(data3)))*100
tropical.perc

warm.perc <-((nrow(warmpoints))/(nrow(data3)))*100
warm.perc

boreo.perc <-((nrow(boreopoints))/(nrow(data3)))*100
boreo.perc

sum(arid.perc3, tropical.perc, warm.perc, boreo.perc)

#site sampling distribution with map as plot background
library(jpeg)
img <- readJPEG("Media/rect.map.jpg")

plot(1:2, type='n', xlab="Modern Longitude", ylab="Modern Latitude", main= "Vadose Silt Sampling Sites",
     xlim=c(-180,180), ylim=c(-90,90), axes=FALSE)
rasterImage(img, 
            xleft=-180, xright=180, 
            ybottom=-90, ytop=90)
points(data3$Modern.Longitude, data3$Modern.Latitude,
       col="black", pch=21, lwd=1.5, cex=1, bg="white")
axis(1, at=seq(from=-180, to=180, by=30))
axis(2, at=seq(from=-90, to=90, by=30))

#Plot Latitude against time
library(divDyn)
data(stages)
plot(data3$Approximate.Age, data3$Paleo.Latitude, 
     xlab="Time", ylab="Paleo Latitude", main= "Vadose Silt Paleocoordinate Origin Time",
     pch=21, col="black", lwd=1.5, bg="lightgray", ylim=c(-90,90))
tsplot(stages, boxes="sys", shading="sys", xlim=c(490,0), ylim=c(-90,90), ylab="Paleo Latitude")
#points(data3$Approximate.Age, data3$Paleo.Latitude,
       #pch=21, col="darkred", lwd=1.5, bg="red")
points(aridpoints$Approximate.Age,aridpoints$Paleo.Latitude, pch=21, col="#C04000", lwd=1.5, bg="darkorange", cex=0.75)
points(tropicalpoints$Approximate.Age,tropicalpoints$Paleo.Latitude, pch=21, col="#169893", lwd=1.5, bg="#0CFFF4", cex=0.75)
points(warmpoints$Approximate.Age,warmpoints$Paleo.Latitude, pch=21, col="black", lwd=1.5, bg="darkred", cex=0.75)
points(boreopoints$Approximate.Age,boreopoints$Paleo.Latitude, pch=21, col="#1a7a16", lwd=1.5, bg="green", cex=0.75)
legend(490,90,legend=c("Arid", "Tropical", "Warm Temperate", "Boreotropical"),  pch = c(21,21,21,21), 
       col = c("#C04000","#169893","black","#1a7a16"), pt.bg = c("#C04000", "#0CFFF4", "darkred", "green"), cex=0.75)

#filter and plot by unique formation to prevent overrepresentation
library(tidyverse)

Formdata3 <-
  data3 %>%
  group_by(Formation) %>%
  summarise(
    Age = mean(Approximate.Age),
    PalLat = mean(Paleo.Latitude),
    PalLong = mean(Paleo.Longitude),
    Climate = unique(Climate)[which.max(tabulate(match(Climate, unique(Climate))))]
  )

nrow(Formdata3)

Aridforms <- subset(Formdata3, Climate == "Arid")
Tropicforms <- subset(Formdata3, Climate == "Tropical")
Warmforms <- subset(Formdata3, Climate == "Warm Temperate")
Boreoforms <- subset(Formdata3, Climate == "Boreotropical")


##Average Latitude
AvgLat3 <- mean(abs(Formdata3$PalLat), na.rm = TRUE)
AvgLat3

#calculate percentage of arid origin
aridformperc<-(nrow(Aridforms))/(nrow(Formdata3))*100
aridformperc

tropicalformperc <-((nrow(Tropicforms))/(nrow(Formdata3)))*100
tropicalformperc

warmformperc <-((nrow(Warmforms))/(nrow(Formdata3)))*100
warmformperc

boreoformperc <-((nrow(Boreoforms))/(nrow(Formdata3)))*100
boreoformperc

sum(aridformperc, tropicalformperc, warmformperc, boreoformperc)

###########################################################
#Create time maps

library(rgplates)
library(divDyn)

#reconstruct tectonics
rec <- reconstruct("coastlines", 
                   age=seq(from=500, to=0, by=-5), 
                   model="MERDITH2021")
saveRDS(rec, file="plate_rec.rds")
rec <- readRDS("plate_rec.rds")

#create a for loop to subset all samples within +/-2.5mya of reconstructed age
time <- seq(from=500, to=0, by=-5)
rec_coords <- vector("list", length(time))
for(i in 1:length(time)){
  subset <- data3[(data3$Approximate.Age < time[i] + 2.5)
                  & (data3$Approximate.Age > time[i] - 2.5),]
  if(nrow(subset) == 0) {next}
  rec_coords[[i]] <- reconstruct(cbind(subset$Modern.Longitude, subset$Modern.Latitude), 
                                 age=time[i], model="MERDITH2021")
}

#create images with the subsetted samples for every 5my between 500-0mya
for (i in 1:length(rec)) {
  png(paste0("Media/MERDITHGif", time[i], ".png"), width = 900, height = 600)
  par(mar = c(1,0,5,0), oma = c(4,5,1,0))
  plot(rec[[i]]$geometry, col="grey",border=NA, ylim=c(-90,90))
  axis(1, at=seq(from=-180, to=180, by=30), pos=-100, xpd=TRUE)
  axis(2, at=seq(from=-90, to=90, by=30), pos=-195, xpd=TRUE)
  #segments(x0=c(-180,-180,-180,-180), y0=c(40,20,-20,-40), x1=c(180,180,180,180), y1=c(40,20,-20,-40))
  points(rec_coords[[i]][,1], rec_coords[[i]][,2], col="black", bg="red", pch=21, cex=1.5)
  title(main=paste0(time[i], " mya"))
  dev.off()
}

######################################
#citations
citation()
devtools::session_info()
citation("rgplates")
citation("divDyn")
citation("tidyverse")
citation("jpeg")