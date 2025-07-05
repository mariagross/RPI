library(rgplates)
library(divDyn)

setwd("C:/Users/gross/OneDrive/Desktop/MSc-Paleobio/semester-2/RPI")

# Load the dataset, select the relevant columns and modify some terms to the format used by the divDyn package
data <- read.csv("RPI_V5.csv")
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
write.csv(data_with_coordinates, "RPI_V5.csv")
