setwd("/Users/nelon/Documents/Notes/BIOL326/Lab5")

data <- read.csv("Lab5Combined.csv")
names(data)
str(data)
print(data)

library(Publish)
ci_data <- ci.mean(speed~stressor,data=data)
ml <- as.numeric(as.character(ci_data$lower))
mu <- as.numeric(as.character(ci_data$upper))
m <- as.numeric(as.character(ci_data$mean))
mu <- mu-m
ml <- ml-m

levels(data$stressor)

data_aov <- aov(formula = speed~stressor, data=data)
data_aov
summary.aov(data_aov)

means <- aggregate(data[,5], list(data$stressor), mean)
dat1 <- as.numeric(as.character(means$x))
dat1

library(plotrix)
sd <- aggregate(data[,5], list(data$stressor), sd)
se <- aggregate(data[,5], list(data$stressor), std.error)
dat2 <- as.numeric(as.character(se$x))
dat2

library(ggplot2)
graph_data <- ggplot(means, aes(x = means$Group.1, y = means$x))

graph_data + 
  geom_bar(stat = "identity") + 
  geom_errorbar(aes(ymin=dat1+ml, ymax=dat1+mu)) + 
  theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black'))

library(multcomp)
tuk <- glht(data_aov)
summary(tuk)
tuk2 <- TukeyHSD(data_aov, ordered = TRUE, conf.level = 0.95)
tuk2
