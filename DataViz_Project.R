# Data Visualization Project
# Reproducing the  plots shown in The Economist article using ggplot2
library(ggplot2)
library(data.table)

df <- read.csv("Economist_Assignment_Data.csv")
head(df)

pl <- ggplot(df, aes(x = CPI, y = HDI)) + geom_point(aes(color = Region), shape = 1, size  = 4) 

pl2  <- pl + geom_smooth(aes(group = 1), method = "lm", formula = y ~ log(x), se = F, color = "red")
get
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", data = subset(df, Country %in% pointsToLabel), check_overlap = T)
pl4 <- pl3 + theme_bw() + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)", limits = c(1, 10), breaks = 1:10) +
                          scale_y_continuous(name = "Human Development Index, 2011 (1=Best)", limits = c(0.2, 1), breaks = c(0.2, 0.4, 0.6, 0.8, 1))
pl5 <- pl4 + ggtitle("Corruption and Human Development")
pl5
