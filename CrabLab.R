#setwd("~/Documents/Notes/BIOL326/Lab6")
data <- read.csv("CrabDatav2.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA", ""))

#variables
time <- data$Time
food <- data$Food
weight <- data$Weight

#divide times by 15min to get percent values
time <- time/15
#time[time == 0] <- NA

#ancova
z <- lm(time ~ food + weight, data = data)
z
summary(z)
aov <- anova(z)
aov

#post-hoc
library(multcomp)
tuk_aov <- aov(z)
tuk <- TukeyHSD(tuk_aov, ordered = TRUE, conf.level = 0.95)
tuk

#interaction plot
library(ggplot2)
data2 <- data
data2$Time <- as.numeric(data2$Time/15)
#data2$Time [data2$Time == 0] <- NA
graph_interact <- ggplot(data2, aes(x=weight, y=time, color=food)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE) +
  theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'),
        axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))
graph_interact  


#bar graph
library(plotrix)
means <- aggregate(data2$Time, list(data2$Food), mean)
sd <- aggregate(data2$Time, list(data2$Food), sd)
se <- aggregate(data2$Time, list(data2$Food), std.error)
se <- as.numeric(se$x)

graph_bar <- ggplot (means, aes(x=means$Group.1, y=means$x, fill=means$Group.1)) +
  geom_bar(stat = "identity", width=0.5) +
  labs(x= "Food Choices", y= "Time Spent (%)", fill="Food") +
  theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
  ) +
  annotate("text", x=3, y=0.205, label="a") +
  annotate("text", x=1, y=0.245, label="a") +
  annotate("text", x=2, y=0.07, label="b") +
  annotate("text", x=4, y=0.027, label="b") +
  geom_errorbar(aes(ymin=means$x-se, ymax=means$x+se), width=0.2) + 
  theme(axis.line = element_line(color = 'black'),
        axis.text = element_text(size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14))
graph_bar

