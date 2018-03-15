setwd("~/Documents/Notes/BIOL326/Lab6")
library(dplyr)
data <- read.csv("Lab data_v2.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data2 <- read.csv("Lab data_v2sorted.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

dataNoCold <- filter(data2, (data2$Condition.Code == "VT" | data2$Condition.Code == "CN"))
attached <- as.factor(data$Attached)
cold <- data$Cold
variability <- data$Var

attachedTime <- as.factor(data2$Time.to.attach..s...NR...not.recorded)
variability2 <- data2$Condition.Code

z <- glm(attached ~ variability * cold, family = binomial(link="logit"), data = data)
z
summary(z)
z_anova <- anova(z, test = "Chisq")
z_anova

test2 <- lm()