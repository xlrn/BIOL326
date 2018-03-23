#setwd("~/Documents/Notes/BIOL326/Lab6")
data <- read.csv("CrabData.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

seaweed <- data$Kelp
banana <- data$Banana
cuttlefish <- data$Cuttlefish
chips <- data$Chip
weight <- data$Weight

