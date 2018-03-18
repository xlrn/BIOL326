setwd("~/Documents/Notes/BIOL326/Lab6")
library(dplyr)
data <- read.csv("Lab data_v2.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data2 <- read.csv("Lab data_v2sorted.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data3 <- read.csv("tenacity.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))
data4 <- read.csv("temperature.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

dataNoCold <- filter(data2, (data2$Condition.Code == "VT" | data2$Condition.Code == "CN"))
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


### Data Analysis 1 (Contingency Table)
z <- glm(attached ~ variability + cold, family = binomial(link="logit"), data = data)
z
summary(z)
z_anova <- anova(z, test = "Chisq")
z_anova

### Data Analysis 2 (ANOVA)
tenacityVT <- filter(data_tenacity, (data_tenacity$Condition.Code == "VT"))
tenacityVT_Mass <- tenacityVT$Weight..g.
tenacityVT_Shell <- tenacityVT$Shell.length..mm.
tenacityVT_Foot <- tenacityVT$Foot.size..mm.
tenacityVT_Strength <- rowSums(tenacityVT[, c("X100g.weight","additional.weights..g.")])

lm <- lm(tenacityVT_Strength ~ tenacityVT_Mass, data = tenacityVT)
lm
summary(lm)
aov <- anova(lm)
aov

lm2 <- lm(tenacityVT_Strength ~ tenacityVT_Shell, data = tenacityVT)
lm2
summary(lm2)
aov2 <- anova(lm2)
aov2

lm3 <- lm(tenacityVT_Strength ~ tenacityVT_Foot, data = tenacityVT)
lm3
summary(lm3)
aov3 <- anova(lm3)
aov3

tenacityCN <- filter(data_tenacity, (data_tenacity$Condition.Code == "CN"))
tenacityCN_Mass <- tenacityCN$Weight..g.
tenacityCN_Shell <- tenacityCN$Shell.length..mm.
tenacityCN_Foot <- tenacityCN$Foot.size..mm.
tenacityCN_Strength <- rowSums(tenacityCN[, c("X100g.weight","additional.weights..g.")])

lm4 <- lm(tenacityCN_Strength ~ tenacityCN_Mass, data = tenacityCN)
lm
summary(lm4)
aov4 <- anova(lm4)
aov4

lm5 <- lm(tenacityCN_Strength ~ tenacityCN_Shell, data = tenacityCN)
lm5
summary(lm5)
aov5 <- anova(lm5)
aov5

lm6 <- lm(tenacityCN_Strength ~ tenacityCN_Foot, data = tenacityCN)
lm6
summary(lm6)
aov6 <- anova(lm6)
aov6

tempVT <- filter(data_temp, (data_temp$Condition.Code == "VT"))
tempVT_Mass <- tempVT$Weight..g.
tempVT_Shell <- tempVT$Shell.length..mm.
tempVT_Foot <- tempVT$Foot.size..mm.
tempVT_Temp <- tempVT$temperature.lost.attachment..celsius.

lm7 <- lm(tempVT_Temp ~ tempVT_Mass, data = tempVT)
lm7
summary(lm7)
aov7 <- anova(lm7)
aov7

lm8 <- lm(tempVT_Temp ~ tempVT_Shell, data = tempVT)
lm8
summary(lm8)
aov8 <- anova(lm8)
aov8

lm9 <- lm(tempVT_Temp ~ tempVT_Foot, data = tempVT)
lm9
summary(lm9)
aov9 <- anova(lm9)
aov9

tempCN <- filter(data_temp, (data_temp$Condition.Code == "CN"))
tempCN_Mass <- tempCN$Weight..g.
tempCN_Shell <- tempCN$Shell.length..mm.
tempCN_Foot <- tempCN$Foot.size..mm.
tempCN_Temp <- tempCN$temperature.lost.attachment..celsius.

lm10 <- lm(tempCN_Temp ~ tempCN_Mass, data = tempCN)
lm10
summary(lm10)
aov10 <- anova(lm10)
aov10

lm11 <- lm(tempCN_Temp ~ tempCN_Shell, data = tempCN)
lm11
summary(lm11)
aov11 <- anova(lm11)
aov11

lm12 <- lm(tempCN_Temp ~ tempCN_Foot, data = tempCN)
lm12
summary(lm12)
aov12 <- anova(lm12)
aov12

### Data Analysis 2 (ANOVA, but correct)
sorted_mass <- data2$Weight..g.
sorted_cc <- data2$Condition.Code
lm_mass <- lm(sorted_mass ~ sorted_cc, data = data2)
lm_mass
summary(lm_mass)
aov_mass <- anova(lm_mass)
aov_mass

sorted_shell <- data2$Shell.length..mm.
lm_shell <- lm(sorted_shell ~ sorted_cc, data = data2)
lm_shell
summary(lm_shell)
aov_shell <- anova(lm_shell)
aov_shell

sorted_foot <- data2$Foot.size..mm.
lm_foot <- lm(sorted_foot ~ sorted_cc, data = data2)
lm_foot
summary(lm_foot)
aov_foot <- anova(lm_foot)
aov_foot

### Data Analysis 3 (ANCOVA)
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

library(ggplot2)
average_tenacity <- aggregate(tenacity, list(data_tenacity$Condition.Code), mean, na.rm = TRUE)

graph_tenacity = ggplot(average_tenacity, aes(x = average_tenacity$Group.1, y = average_tenacity$x))
graph_tenacity +
  geom_bar(stat = "identity", width=0.5, fill="#00ffcc") +
  theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

average_temp <- aggregate(detachTemp, list(data_temp$Condition.Code), mean, na.rm = TRUE)
graph_temp = ggplot(average_tenacity, aes(x = average_temp$Group.1, y = average_temp$x))
graph_temp +
  geom_bar(stat = "identity", width=0.5, fill="#751aff") +
  theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))
