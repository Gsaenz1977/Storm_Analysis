#Storm Analysis

library(plyr) # for count & aggregate method
library(ggplot2) # for plots

# create a data dir if it doesn't exist
if(!file.exists("./data")){
        dir.create("./data")
}
# load file from URL to bz2 file in data dir
if(!file.exists("./data/StormData.csv.bz2")){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
        destPath <- "./data/StormData.csv.bz2"
        binData <- getBinaryURL(fileUrl, ssl.verifypeer=0L, followlocation=1L)
        destFileHandle <- file(destPath, open="wb")
        writeBin(binData,destFileHandle)
        close(destFileHandle)
}
# unzip bz2 file to csv
if(!file.exists("./data/StormData.csv")){
        filePath <- "./data/StormData.csv.bz2"
        destPath <- "./data/StormData.csv"
        bunzip2(filePath,destPath,overwrite=TRUE, remove=FALSE)
}
storm <- read.csv("./data/StormData.csv")


# number of unique event types
length(unique(storm$EVTYPE))
# translate all letters to lowercase
event_types <- tolower(storm$EVTYPE)
# replace all punct. characters with a space
event_types <- gsub("[[:blank:][:punct:]+]", " ", event_types)
length(unique(event_types))
# update the data frame
storm$EVTYPE <- event_types


## ------------------------------------------------------------------------
library(plyr)
casualties <- ddply(storm, .(EVTYPE), summarize,
                    fatalities = sum(FATALITIES),
                    injuries = sum(INJURIES))

# Find events that caused most death and injury
fatal_events <- head(casualties[order(casualties$fatalities, decreasing = T), ], 10)
injury_events <- head(casualties[order(casualties$injuries, decreasing = T), ], 10)


## ------------------------------------------------------------------------
fatal_events[, c("EVTYPE", "fatalities")]


## ------------------------------------------------------------------------
injury_events[, c("EVTYPE", "injuries")]


## ------------------------------------------------------------------------
exp_transform <- function(e) {
        # h -> hundred, k -> thousand, m -> million, b -> billion
        if (e %in% c('h', 'H'))
                return(2)
        else if (e %in% c('k', 'K'))
                return(3)
        else if (e %in% c('m', 'M'))
                return(6)
        else if (e %in% c('b', 'B'))
                return(9)
        else if (!is.na(as.numeric(e))) # if a digit
                return(as.numeric(e))
        else if (e %in% c('', '-', '?', '+'))
                return(0)
        else {
                stop("Invalid exponent value.")
        }
}


## ----cache=TRUE----------------------------------------------------------
prop_dmg_exp <- sapply(storm$PROPDMGEXP, FUN=exp_transform)
storm$prop_dmg <- storm$PROPDMG * (10 ** prop_dmg_exp)
crop_dmg_exp <- sapply(storm$CROPDMGEXP, FUN=exp_transform)
storm$crop_dmg <- storm$CROPDMG * (10 ** crop_dmg_exp)


## ------------------------------------------------------------------------
# Compute the economic loss by event type
econ_loss <- ddply(storm, .(EVTYPE), summarize,
                   prop_dmg = sum(prop_dmg),
                   crop_dmg = sum(crop_dmg))

# filter out events that caused no economic loss
econ_loss <- econ_loss[(econ_loss$prop_dmg > 0 | econ_loss$crop_dmg > 0), ]
prop_dmg_events <- head(econ_loss[order(econ_loss$prop_dmg, decreasing = T), ], 10)
crop_dmg_events <- head(econ_loss[order(econ_loss$crop_dmg, decreasing = T), ], 10)


## ------------------------------------------------------------------------
prop_dmg_events[, c("EVTYPE", "prop_dmg")]


## ------------------------------------------------------------------------
crop_dmg_events[, c("EVTYPE", "crop_dmg")]


## ------------------------------------------------------------------------
# Set the levels in order
library(ggplot2)
p1 <- ggplot(data=fatal_events,
             aes(x=reorder(EVTYPE, fatalities), y=fatalities, fill=fatalities)) +
        geom_bar(stat="identity") +
        coord_flip() +
        ylab("Total number of fatalities") +
        xlab("Event type") +
        theme(legend.position="none")

p2 <- ggplot(data=injury_events,
             aes(x=reorder(EVTYPE, injuries), y=injuries, fill=injuries)) +
        geom_bar(stat="identity") +
        coord_flip() + 
        ylab("Total number of injuries") +
        xlab("Event type") +
        theme(legend.position="none")

p1
p2

## ------------------------------------------------------------------------
# Set the levels in order
p3 <- ggplot(data=prop_dmg_events,
             aes(x=reorder(EVTYPE, prop_dmg), y=log10(prop_dmg), fill=prop_dmg )) +
        geom_bar(stat="identity") +
        coord_flip() +
        xlab("Event type") +
        ylab("Property damage in dollars (log-scale)") +
        theme(legend.position="none")

p3