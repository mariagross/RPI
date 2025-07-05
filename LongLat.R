
setwd("C:/Users/gross/OneDrive/Desktop/MSc-Paleobio/semester-2/RPI")

#read in Data
data3<- read.csv("RPI_V5.csv")

######################Version 3##############################
#refined ages and aridity measures using model MERDITH2021

#plot paleolat v paleolong
png("Media/PC-Climate.png", width = 900, height = 600, pointsize=20)
plot(data3$Paleo.Longitude, data3$Paleo.Latitude, 
     xlab="Paleo Longitude", ylab="Paleo Latitude", main= "Paleocoordinates & Climate",
     pch=21, col="black", lwd=1.5, bg="lightgray",xlim=c(-180,180), ylim=c(-90,90), axes=FALSE, cex=0.75)
axis(1, at=seq(from=-180, to=180, by=30))
axis(2, at=seq(from=-90, to=90, by=30))

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
dev.off()

#plot depositional environment points
png("Media/PC-Depo.png", width = 900, height = 600, pointsize=20)
plot(data3$Paleo.Longitude, data3$Paleo.Latitude, 
     xlab="Paleo Longitude", ylab="Paleo Latitude", main= "Paleocoordinates & Climate",
     pch=21, col="black", lwd=1.5, bg="lightgray",xlim=c(-180,180), ylim=c(-90,90), axes=FALSE, cex=0.75)
axis(1, at=seq(from=-180, to=180, by=30))
axis(2, at=seq(from=-90, to=90, by=30))
Coastal <- subset(data3, Deposition.Setting == "Coastal")
points(Coastal$Paleo.Longitude,Coastal$Paleo.Latitude, pch=21, col="tan", lwd=1.5, bg="khaki", cex=0.75)

Continental <- subset(data3, Deposition.Setting == "Continental")
points(Continental$Paleo.Longitude,Continental$Paleo.Latitude, pch=21, col="darkgreen", lwd=1.5, bg="forestgreen", cex=0.75)

CoaCon <- subset(data3, Deposition.Setting == "Coastal-Continental")
points(CoaCon$Paleo.Longitude,CoaCon$Paleo.Latitude, pch=21, col="purple", lwd=1.5, bg="lavender", cex=0.75)

Marine <- subset(data3, Deposition.Setting == "Marine")
points(Marine$Paleo.Longitude,Marine$Paleo.Latitude, pch=21, col="blue", lwd=1.5, bg="lightblue", cex=0.75)

legend(-180,97,legend=c("Coastal", "Continental","Coastal-Continental", "Marine"),  pch = c(21,21,21,21), 
       col = c("tan","darkgreen","purple", "blue"), pt.bg = c("khaki", "forestgreen","lavender", "lightblue"), cex=0.75)

dev.off()


###########
#calculate percentage of climates
subset <- data3[,c("Climate", "Formation", "Paleo.Latitude")]

## Bootstrapped percentages of climate
bootstrap_percentages <- function(data, repetitions = 10000, seed = 1) {
  set.seed(seed)
  results <- matrix(0, nrow = repetitions, ncol = length(unique(data$Climate)))
  colnames(results) <- names(table(data$Climate))
  for(i in 1:repetitions) {
    boot_climates <- sample(data$Climate, length(data$Climate), replace=TRUE)
    freqs <- table(boot_climates)*100/length(boot_climates)
    for(climate in names(freqs)) {
      results[i,climate] <- freqs[climate]
    }
  }
  # Get percentages
  percentages <- table(data$Climate)*100/length(data$Climate)
  # Organize in data frame
  df <- data.frame(
    Climate = names(table(data$Climate)),
    Percentage = percentages,
    Lower_Bound = c(quantile(results[,1], 0.025),quantile(results[,2], 0.025),quantile(results[,3], 0.025),quantile(results[,4], 0.025)),
    Upper_Bound = c(quantile(results[,1], 0.975),quantile(results[,2], 0.975),quantile(results[,3], 0.975),quantile(results[,4], 0.975))
  )
  df <- df[,-2]
  colnames(df)[2] <- "Percentage"
  df
}

## Calculate percentages from original subset
per <- bootstrap_percentages(subset)
per

## Repeat, but now filter by formation
library(tidyverse)

filtered_subset <- subset %>% 
  group_by(Formation) %>%
  summarise(
    Climate = unique(Climate),
    Paleo.Latitude = mean(Paleo.Latitude)
  )
per2 <- bootstrap_percentages(filtered_subset)
per2

#calculate percentage of deposition
subset.depo <- data3[,c("Deposition.Setting", "Formation", "Paleo.Latitude")]

## Bootstrapped percentages for deposition
bootstrap_percentages_depo <- function(data, repetitions = 10000, seed = 1) {
  set.seed(seed)
  results <- matrix(0, nrow = repetitions, ncol = length(unique(data$Deposition.Setting)))
  colnames(results) <- names(table(data$Deposition.Setting))
  for(i in 1:repetitions) {
    boot_depos <- sample(data$Deposition.Setting, length(data$Deposition.Setting), replace=TRUE)
    freqs <- table(boot_depos)*100/length(boot_depos)
    for(setting in names(freqs)) {
      results[i,setting] <- freqs[setting]
    }
  }
  # Get percentages
  percentages.depo <- table(data$Deposition.Setting)*100/length(data$Deposition.Setting)
  # Organize in data frame
  df.depo <- data.frame(
    Setting = names(table(data$Deposition.Setting)),
    Percentage = percentages.depo,
    Lower_Bound = c(quantile(results[,1], 0.025),quantile(results[,2], 0.025),quantile(results[,3], 0.025),quantile(results[,4], 0.025)),
    Upper_Bound = c(quantile(results[,1], 0.975),quantile(results[,2], 0.975),quantile(results[,3], 0.975),quantile(results[,4], 0.975))
  )
  df.depo <- df.depo[,-2]
  colnames(df.depo)[2] <- "Percentage"
  df.depo
}

## Calculate percentages from original subset
per.depo <- bootstrap_percentages_depo(subset.depo)
per.depo

filtered_subset_depo <- subset.depo %>% 
  group_by(Formation) %>%
  summarise(
    Deposition.Setting = unique(Deposition.Setting),
    Paleo.Latitude = mean(Paleo.Latitude)
  )
per.depo2 <- bootstrap_percentages_depo(filtered_subset_depo)
per.depo2

##Bootstrapped latitude
bootstrap_lat <- function(data, repetitions = 10000, seed = 1) {
  set.seed(seed)
  results <- c()
  for(i in 1:repetitions) {
    boot_lat <- sample(data$Paleo.Latitude, length(data$Paleo.Latitude), replace=TRUE)
    results[i] <- mean(boot_lat)
  }
  # Get full data mean
  mean <- mean(data$Paleo.Latitude)
  # Organize in data frame
  df <- data.frame(
    Mean_Lat = mean,
    Lower_Bound = quantile(results, 0.025),
    Upper_Bound = quantile(results, 0.975)
  )
}

#average latitudes
avglat <- bootstrap_lat(subset)
avglat

avglat2 <- bootstrap_lat(filtered_subset)
avglat2

# Correlation between deposition and climate
{
set.seed(1)
chisq.test(filtered_subset$Climate, filtered_subset_depo$Deposition.Setting, 
           simulate.p.value = T, B=10000)
}

###############
#site sampling distribution with map as plot background
library(jpeg)
img <- readJPEG("Media/rect.map.jpg")

png("Media/Sampling.png", width = 900, height = 600, pointsize=17)
plot(1:2, type='n', xlab="Modern Longitude", ylab="Modern Latitude", main= "Vadose Silt Sampling Sites",
     xlim=c(-180,180), ylim=c(-90,90), axes=FALSE)
rasterImage(img, 
            xleft=-180, xright=180, 
            ybottom=-90, ytop=90)
points(data3$Modern.Longitude, data3$Modern.Latitude,
       col="black", pch=21, lwd=1.5, cex=1, bg="white")
axis(1, at=seq(from=-180, to=180, by=30))
axis(2, at=seq(from=-90, to=90, by=30))
dev.off()

#Plot Latitude against time
library(divDyn)
data(stages)

png("Media/PCtime.png", width = 900, height = 600, pointsize=20)
tsplot(stages, boxes="sys", shading="sys", xlim=c(490,0), ylim=c(-90,90), ylab="Paleo Latitude")
points(aridpoints$Approximate.Age,aridpoints$Paleo.Latitude, pch=21, col="#C04000", lwd=1.5, bg="darkorange", cex=0.75)
points(tropicalpoints$Approximate.Age,tropicalpoints$Paleo.Latitude, pch=21, col="#169893", lwd=1.5, bg="#0CFFF4", cex=0.75)
points(warmpoints$Approximate.Age,warmpoints$Paleo.Latitude, pch=21, col="black", lwd=1.5, bg="darkred", cex=0.75)
points(boreopoints$Approximate.Age,boreopoints$Paleo.Latitude, pch=21, col="#1a7a16", lwd=1.5, bg="green", cex=0.75)
legend(490,90,legend=c("Arid", "Tropical", "Warm Temperate", "Boreotropical"),  pch = c(21,21,21,21), 
       col = c("#C04000","#169893","black","#1a7a16"), pt.bg = c("#C04000", "#0CFFF4", "darkred", "green"), cex=0.75)
dev.off()

png("Media/Timedepo.png", width = 900, height = 600, pointsize=20)
tsplot(stages, boxes="sys", shading="sys", xlim=c(490,0), ylim=c(-90,90), ylab="Paleo Latitude")
points(Coastal$Approximate.Age,Coastal$Paleo.Latitude, pch=21, col="tan", lwd=1.5, bg="khaki", cex=0.75)
points(Continental$Approximate.Age,Continental$Paleo.Latitude, pch=21, col="darkgreen", lwd=1.5, bg="forestgreen", cex=0.75)
points(Marine$Approximate.Age,Marine$Paleo.Latitude, pch=21, col="blue", lwd=1.5, bg="lightblue", cex=0.75)
points(CoaCon$Approximate.Age,CoaCon$Paleo.Latitude, pch=21, col="purple", lwd=1.5, bg="lavender", cex=0.75)
legend(490,90,legend=c("Coastal", "Continental","Coastal-Continental", "Marine"),  pch = c(21,21,21,21), 
       col = c("tan","darkgreen","purple", "blue"), pt.bg = c("khaki", "forestgreen","lavender", "lightblue"), cex=0.75)

dev.off()

#######################

#filter and plot by unique formation to prevent overrepresentation
Formdata3 <-
  data3 %>%
  group_by(Formation) %>%
  summarise(
    Age = mean(Approximate.Age),
    PalLat = mean(Paleo.Latitude),
    PalLong = mean(Paleo.Longitude),
    Climate = unique(Climate)[which.max(tabulate(match(Climate, unique(Climate))))]
  )

nrow(data3)
nrow(Formdata3)

Aridforms <- Formdata3[Formdata3$Climate=='Arid',]
Tropicforms <- Formdata3[Formdata3$Climate=='Tropical',]
Warmforms <- Formdata3[Formdata3$Climate=='Warm Temperate',]
Boreoforms <- Formdata3[Formdata3$Climate=='Boreotropical',]

#plot filtered climate points
###plot paleolat v paleolong
png("Media/FormCoords.png", width = 900, height = 600, pointsize=20)
plot(Formdata3$PalLong, Formdata3$PalLat, 
     xlab="Paleo Longitude", ylab="Paleo Latitude", main= "Unique Formations: Paleocoordinates & Climate",
     pch=21, col="black", lwd=1.5, bg="lightgray",xlim=c(-180,180), ylim=c(-90,90), axes=FALSE, cex=0.75)
axis(1, at=seq(from=-180, to=180, by=30))
axis(2, at=seq(from=-90, to=90, by=30))
points(Aridforms$PalLong,Aridforms$PalLat, pch=21, col="#C04000", lwd=1.5, bg="darkorange", cex=0.75)
points(Tropicforms$PalLong,Tropicforms$PalLat, pch=21, col="#169893", lwd=1.5, bg="#0CFFF4", cex=0.75)
points(Warmforms$PalLong, Warmforms$PalLat, pch=21, col="black", lwd=1.5, bg="darkred", cex=0.75)
points(Boreoforms$PalLong, Boreoforms$PalLat, pch=21, col="#1a7a16", lwd=1.5, bg="green", cex=0.75)
legend(-180,97,legend=c("Arid", "Tropical", "Warm Temperate", "Boreotropical"),  pch = c(21,21,21,21), 
       col = c("#C04000","#169893","black","#1a7a16"), pt.bg = c("#C04000", "#0CFFF4", "darkred", "green"), cex=0.75)
dev.off()

###plot paleolat vs time
png("Media/FormTime.png", width = 900, height = 600, pointsize=20)
tsplot(stages, boxes="sys", shading="sys", xlim=c(490,0), ylim=c(-90,90), ylab="Paleo Latitude")
points(Aridforms$Age,Aridforms$PalLat, pch=21, col="#C04000", lwd=1.5, bg="darkorange", cex=0.75)
points(Tropicforms$Age,Tropicforms$PalLat, pch=21, col="#169893", lwd=1.5, bg="#0CFFF4", cex=0.75)
points(Warmforms$Age, Warmforms$PalLat, pch=21, col="black", lwd=1.5, bg="darkred", cex=0.75)
points(Boreoforms$Age, Boreoforms$PalLat, pch=21, col="#1a7a16", lwd=1.5, bg="green", cex=0.75)
legend(490,90,legend=c("Arid", "Tropical", "Warm Temperate", "Boreotropical"),  pch = c(21,21,21,21), 
       col = c("#C04000","#169893","black","#1a7a16"), pt.bg = c("#C04000", "#0CFFF4", "darkred", "green"), cex=0.75)
dev.off()

###########################################################
#Create time maps

library(rgplates)

#reconstruct tectonics
rec <- reconstruct("coastlines", 
                   age=seq(from=500, to=0, by=-5), 
                   model="MERDITH2021")
saveRDS(rec, file="plate_rec2.rds")
rec <- readRDS("plate_rec2.rds")

#create a for loop to subset all samples within +/-2.5mya of reconstructed age
time <- seq(from=500, to=0, by=-5)
rec_coords <- vector("list", length(time))
for(i in 1:length(time)){
  subset <- data3[(data3$Approximate.Age < time[i] + 2.5)
                  & (data3$Approximate.Age > time[i] - 2.5),]
  if(nrow(subset) == 0) {next}
  rec_coords[[i]] <- reconstruct(cbind(subset$Modern.Longitude, subset$Modern.Latitude), 
                                 age=time[i], model="MERDITH2021", validtime=FALSE)
}

#create images with the subsetted samples for every 5my between 500-0mya
for (i in 1:length(rec)) {
  png(paste0("Media/MERDITHGif/", time[i], ".png"), width = 900, height = 600)
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