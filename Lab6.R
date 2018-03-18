setwd("~/Documents/Notes/BIOL326/Lab6")
library(dplyr)
data <- read.csv("Lab data_v2.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data2 <- read.csv("Lab data_v2sorted.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data3 <- read.csv("tenacity.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data4 <- read.csv("temperature.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

dadataNoCold <- filter(data2, (data2$Condition.Code == "VT" | data2$Condition.Code == "CN"))
data_tenacity <- filter(data3, (data3$Condition.Code == "VT" | data3$Condition.Code == "CN"))
data_temp <- filter(data4, (data4$Condition.Code == "VT" | data4$Condition.Code == "CN"))

attached <- as.factor(data$Attached)
cold <- data$Cold
variability <- data$Var

timeToAttach <- dataNoCold$Time.to.attach..s...NR...not.recorded
footSize <- dataNoCold$Foot.size..mm.
strength <- dataNoCold$tenacity..weight.lost.attachment.

attachedTime <- as.factor(dataNoCold$Time.to.attach..s...NR...not.recorded)
variability2 <- dataNoCold$Condition.Code

footSizeTenacity <- data_tenacity$Foot.size..mm.
tenacity <- rowSums(data_tenacity[, c("X100g.weight","additional.weights..g.")]) 
var_tenacity <- data_tenacity$Condition.Code
massTenacity <- data_tenacity$Weight..g.

detachTimeMin <- data_tenacity$tenacity..minutes.lost.attachment.
detachTimeSec <- detachTimeMin * 60

detachTimeMinTemp <- data_temp$tenacity..minutes.lost.attachment.
detachTimeSecTemp <- detachTimeMinTemp * 60
detachTemp <- data_temp$temperature.lost.attachment..celsius.
var_temp <- data_temp$Condition.Code
massTemp <- data_temp$Weight..g.

z <- glm(attached ~ variability + cold, family = binomial(link="logit"), data = data)
z
summary(z)
z_anova <- anova(z, test = "Chisq")
z_anova

z2 <- lm(timeToAttach ~ variability2 + footSize, data = dataNoCold)
z2
summary(z2)
z2_ancova <- anova(z2)
z2_ancova

z3 <- lm(tenacity ~ var_tenacity + footSizeTenacity, data = data_tenacity)
z3
summary(z3)
z3_ancova <- anova(z3)
z3_ancova

z4 <- lm(detachTimeMin ~ var_tenacity + footSizeTenacity, data = data_tenacity)
z4
summary(z4)
z4_ancova <- anova(z4)
z4_ancova

z5 <- lm(detachTemp ~ var_temp + massTemp, data = data_temp)
z5
summary(z5)
z5_ancova <- anova(z5)
z5_ancova

