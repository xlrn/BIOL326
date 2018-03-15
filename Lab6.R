setwd("~/Documents/Notes/BIOL326/Lab6")
library(dplyr)
data <- read.csv("Lab data_v2.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data2 <- read.csv("Lab data_v2sorted.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data3 <- read.csv("tenacity.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

dataNoCold <- filter(data2, (data2$Condition.Code == "VT" | data2$Condition.Code == "CN"))
data_tenacity <- filter(data3, (data3$Condition.Code == "VT" | data3$Condition.Code == "CN"))
attached <- as.factor(data$Attached)
cold <- data$Cold
variability <- data$Var

timeToAttach <- data2$Time.to.attach..s...NR...not.recorded
footSize <- data2$Foot.size..mm.
strength <- data2$tenacity..weight.lost.attachment.

attachedTime <- as.factor(data2$Time.to.attach..s...NR...not.recorded)
variability2 <- data2$Condition.Code

footSizeTenacity <- data_tenacity$Foot.size..mm.
tenacity <- rowSums(data_tenacity[, c("X100g.weight","additional.weights..g.")]) 
var_tenacity <- data_tenacity$Condition.Code

z <- glm(attached ~ variability * cold, family = binomial(link="logit"), data = data)
z
summary(z)
z_anova <- anova(z, test = "Chisq")
z_anova

z2 <- lm(timeToAttach ~ variability2 * footSize, data = dataNoCold)
z2
summary(z2)
z2_ancova <- anova(z2)
z2_ancova

z3 <- lm(tenacity ~ var_tenacity * footSizeTenacity, data = dataNoCold)
z3
summary(z3)
z3_ancova <- anova(z3)
z3_ancova
