library(rgplates)
library(divDyn)

setwd("C:/Users/gross/OneDrive/Desktop/MSc-Paleobio/semester-2/RPI")

# Load the dataset, select the relevant columns and modify some terms to the format used by the divDyn package
data <- read.csv("RPI.csv")
subset <- data[,c("Modern.Latitude", "Modern.Longitude", "Numerical.Age", "Period", "Epoch", "Age")]
subset$Epoch <- sub("Late", "Upper", subset$Epoch)
subset$Epoch <- sub("Early", "Lower", subset$Epoch)
subset$Age <- sub("Late", "Upper", subset$Age)
subset$Age <- sub("Early", "Lower", subset$Age)
subset$Numerical.Age <- sub(" ", "", subset$Numerical.Age)

# Select an age that corresponds to the middle of the most exact available date
    # Numerical Age -> Age -> Epoch -> Period
data(stages)
#for (i in 123:124) {
for (i in 1:nrow(subset)) {
    # Check if Numerical Age is present
    if ((subset[i,"Numerical.Age"] != "-") & (subset[i,"Numerical.Age"] != "")) {
        age_bottom <- as.numeric((sub("-.*", "", subset[i,"Numerical.Age"])))
        age_top <- as.numeric((sub(".*-", "", subset[i,"Numerical.Age"])))
    }
    # If not, check if Age is present
    else {
        if (subset[i,"Age"] != "-") {
            # Check if it is a range
            if (grepl("-", subset[i,"Age"])) {
                bottom_stage <- sub("-.*", "", subset[i,"Age"])
                top_stage <- sub(".*-", "", subset[i,"Age"])
                #print(bottom_stage)
                #print(top_stage)
                age_bottom <- max(stages[which(stages$stage == bottom_stage),"bottom"])
                age_top <- min(stages[which(stages$stage == top_stage),"top"])
                # Check if it is a relative age and a range
                if (grepl("Lower|Middle|Upper", subset[i,"Age"])) {
                    renamed_bottom_stage <- paste(bottom_stage, subset[i,"Epoch"])
                    age_bottom <- max(stages[which(stages$stage == renamed_bottom_stage),"bottom"])
                    renamed_top_stage <- paste(top_stage, subset[i,"Epoch"])
                    age_top <- max(stages[which(stages$stage == renamed_top_stage),"top"])
                }
            }
            else {
                # Check if it is just a relative age
                if (grepl("Lower|Middle|Upper", subset[i,"Age"])) {
                    renamed_stage <- paste(subset[i,"Age"], subset[i,"Epoch"])
                    age_bottom <- max(stages[which(stages$stage == renamed_stage),"bottom"])
                    age_top <- max(stages[which(stages$stage == renamed_stage),"top"])
                }
                # Otherwise, it is the direct name of the age
                else {
                    age_bottom <- max(stages[which(stages$stage == subset[i,"Age"]),"bottom"])
                    age_top <- min(stages[which(stages$stage == subset[i,"Age"]),"top"])
                }
            }
        }
        # If not, check if Epoch is present
        else {
            if (subset[i,"Epoch"] != "-") {
                # Check if it is a range
                if (grepl("-", subset[i,"Epoch"])) {
                    bottom_stage <- sub("-.*", "", subset[i,"Epoch"])
                    top_stage <- sub(".*-", "", subset[i,"Epoch"])
                    age_bottom <- max(stages[which(stages$series == bottom_stage),"bottom"])
                    age_top <- min(stages[which(stages$series == top_stage),"top"])
                    # Check if it is a relative epoch and a range
                    if (grepl("Lower|Middle|Upper", subset[i,"Epoch"])) {
                        renamed_bottom_stage <- paste(bottom_stage, subset[i,"Period"])
                        age_bottom <- max(stages[which(stages$series == renamed_bottom_stage),"bottom"])
                        renamed_top_stage <- paste(top_stage, subset[i,"Period"])
                        age_top <- max(stages[which(stages$series == renamed_top_stage),"top"])
                    }
                }
                else {
                    # Check if it is just a relative epoch
                    if (grepl("Lower|Middle|Upper", subset[i,"Epoch"])) {
                        renamed_stage <- paste(subset[i,"Epoch"], subset[i,"Period"])
                        age_bottom <- max(stages[which(stages$series == renamed_stage),"bottom"])
                        age_top <- max(stages[which(stages$series == renamed_stage),"top"])
                    }
                    # Otherwise, it is the direct name of the epoch
                    else {
                        age_bottom <- max(stages[which(stages$series == subset[i,"Epoch"]),"bottom"])
                        age_top <- min(stages[which(stages$series == subset[i,"Epoch"]),"top"])
                    }
                }
            }
            # If Epoch is not present, get top and bottom of Period
            else {
                age_bottom <- max(stages[which(stages$system == subset[i,"Period"]),"bottom"])
                age_top <- min(stages[which(stages$system == subset[i,"Period"]),"top"])
                # If the name corresponds to multiple periods, separate them into a bottom system and a top system
                if (is.infinite(age_bottom)) {
                    bottom_series <- sub("-.*", "", subset[i,"Period"])
                    top_series <- sub(".*-", "", subset[i,"Period"])
                    age_bottom <- max(stages[which(stages$system == bottom_series),"bottom"])
                    age_top <- min(stages[which(stages$system == top_series),"top"])
                }
            }
        }
    }
    subset$Approx_age[i] <- (age_bottom+age_top)/2
}

# Check if there is any approximate age that couldn't be calculated
subset[which(is.na(subset$Approx_age)|is.infinite(subset$Approx_age)),c("Period", "Epoch", "Age")]

# Round ages that are older than 450 mya to 450, in New South Wales
#subset[(subset$Approx_age > 450) & (subset$Modern.Latitude > -34) & (subset$Modern.Latitude < -33) & (subset$Modern.Longitude > 148) & (subset$Modern.Longitude < 149), "Approx_age"] <- 450

# Reconstruct paleocoordinates
for (i in 1:nrow(subset)) {
    rec <- reconstruct(c(subset$Modern.Longitude[i], subset$Modern.Latitude[i]), age=subset$Approx_age[i], model="MERDITH2021", validtime=FALSE)
    subset$paleolong[i] <- rec[[1]]
    subset$paleolat[i] <- rec[[2]]
}

# Add them to the dataset and save
data_with_coordinates <- data
non_na_rows <- which(!is.na(data_with_coordinates$Modern.Latitude))
data_with_coordinates[non_na_rows,c("Approximate.Age", "Paleo.Latitude", "Paleo.Longitude")] <- cbind(subset$Approx_age[non_na_rows], subset$paleolat[non_na_rows], subset$paleolong[non_na_rows])
write.csv(data_with_coordinates, "RPI_V4.csv")


# Reconstruct paleocoordinates for climate
for (i in 1:nrow(subset)) {
  rec2 <- reconstruct(c(subset$Modern.Longitude[i], subset$Modern.Latitude[i]), age=subset$Approx_age[i], model="PALEOMAP", validtime=FALSE)
  subset2$paleolong[i] <- rec2[[1]]
  subset2$paleolat[i] <- rec2[[2]]
}

# Add them to the dataset and save
data_with_coordinates <- data
non_na_rows <- which(!is.na(data_with_coordinates$Modern.Latitude))
data_with_coordinates[non_na_rows,c("Approximate.Age", "Paleo.Latitude", "Paleo.Longitude")] <- cbind(subset2$Approx_age[non_na_rows], subset2$paleolat[non_na_rows], subset2$paleolong[non_na_rows])
write.csv(data_with_coordinates, "RPI_V4.csv")

#####################################################
# Below is just testing

# Reconstruct paleocoordinates with a delay added
for (i in 1:nrow(subset)) {
    rec <- reconstruct(c(subset$Modern.Longitude[i], subset$Modern.Latitude[i]), age=subset$Approx_age[i]*0.9, model="MERDITH2021", validtime=FALSE)
    subset$paleolong_delay[i] <- rec[[1]]
    subset$paleolat_delay[i] <- rec[[2]]
}

sum((20 < abs(subset$paleolat)) & (abs(subset$paleolat) < 40), na.rm = T)/(nrow(subset))

bootstrap_paleolat_frac <- c()
for(i in 1:10000) {
    sampled_subset <- sample(abs(subset$paleolat), nrow(subset), replace=TRUE)
    bootstrap_paleolat_frac[i] <- sum((20 < sampled_subset) & (sampled_subset < 40), na.rm = T)/(nrow(subset))
}
mean(bootstrap_paleolat_frac)
quantile(bootstrap_paleolat_frac, c(0.025,0.975))

sum((20 < abs(subset$paleolat_delay)) & (abs(subset$paleolat_delay) < 40), na.rm = T)/(nrow(subset))

bootstrap_paleolat_delay_frac <- c()
for(i in 1:10000) {
    sampled_subset <- sample(abs(subset$paleolat_delay), nrow(subset), replace=TRUE)
    bootstrap_paleolat_delay_frac[i] <- sum((20 < sampled_subset) & (sampled_subset < 40), na.rm = T)/(nrow(subset))
}
mean(bootstrap_paleolat_delay_frac)
quantile(bootstrap_paleolat_delay_frac, c(0.025,0.975))


plot(subset$Approx_age, abs(subset$paleolat), col="red")
points(subset$Approx_age, abs(subset$paleolat_delay), col="blue")
abline(h=20)
abline(h=40)

problems <- which(is.na(subset$paleolong))
for (i in problems) {
    rec <- reconstruct(c(subset$Modern.Longitude[i], subset$Modern.Latitude[i]), age=subset$Approx_age[i], model="MULLER2022", anchor=1)
    subset$paleolong[i] <- rec[[1]]
    subset$paleolat[i] <- rec[[2]]
}

subset[which(subset$Approx_age<18),c("Modern.Latitude","Modern.Longitude","Approx_age","paleolong", "paleolat")]

#Not above 450
subset[which(is.na(subset$paleolong)),c("Modern.Latitude","Modern.Longitude","Approx_age","paleolong")]
#Longitude not above 180?
subset[which(is.na(subset$paleolong) & subset$Approx_age<450),c("Modern.Latitude","Modern.Longitude","Approx_age","paleolong")]

reconstruct(c(148.8947,-33.59394),age=455.465,model="GOLONKA")
reconstruct(c(131.3126, 34.26961),age=266.960, model="GOLONKA")
reconstruct(c(177.4435,-17.98283),age=17.335,model="TorsvikCocks2017")

