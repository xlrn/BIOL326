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

data_lm <- lm(speed~stressor, data)
data_aov <- aov(data_lm)
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
  labs(x= "Treatments", y= "Avg. Speed (mm/s)") +
  geom_bar(stat = "identity", width=0.5, fill="#00ffcc") + 
  annotate("text", x=3, y=0.33, label="a") +
  annotate("text", x=1, y=0.52, label="b") +
  annotate("text", x=2, y=0.47, label="a/b") +
  annotate("text", x=4, y=0.57, label="b") +
  annotate("text", x=5, y=0.50, label="a/b") +
  annotate("text", x=6, y=0.52, label="b") +
  annotate("text", x=7, y=0.61, label="b") +
  geom_errorbar(aes(ymin=dat1+ml, ymax=dat1+mu), width=0.2) + 
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

tuk3 <- summary.lm(data_aov)
tuk3

##TODO: Do it again with linear regression model

csv_exp <- as.data.frame(tuk2$stressor)
write.csv(csv_exp, file="tuk.csv")

