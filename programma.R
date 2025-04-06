
kordat <- read.table("variants1.txt", dec = ",", strip.white = TRUE)

kordat[, 9:ncol(kordat)] <- lapply(kordat[, 9:ncol(kordat)], as.factor)

sink("results.txt")


print(summary(kordat))

sl.by.b <- split(kordat$Slope, kordat$b)
print(sl.by.b)

kordat$Average <- rowMeans(
  sapply(kordat[, c("Slope", "Intercept", "adj.r.squared")], function(x) {
    as.numeric(gsub(",", ".", as.character(x)))
  }),
  na.rm = TRUE)

print(tapply(kordat$Slope, kordat$f, sd, na.rm = TRUE))

kordat$adj.r.squared <- as.numeric(gsub(",", ".", as.character(kordat$adj.r.squared)))

prockordat <- kordat[
  (kordat$adj.r.squared > 0.7 & kordat$adj.r.squared >= 0) |
  (kordat$adj.r.squared > -0.3 & kordat$adj.r.squared < 0),
]

k <- as.numeric(as.character(prockordat$Slope))
prockordat$Slope <- 1 - 1 / k


print(prockordat)

library(ggplot2)

kordat$MAD <- as.numeric(gsub(",", ".", as.character(kordat$MAD)))
kordat$Average <- as.numeric(gsub(",", ".", as.character(kordat$Average)))

svg("scatter.svg", width = 7, height = 5)

ggplot(kordat, aes(x = MAD, y = Average)) + geom_point() 

ggsave("scatter.svg")
dev.off()

svg("boxplot.svg", width = 7, height = 5)
kordat <- na.omit(kordat)

kordat$Intercept <- as.numeric(gsub(",", ".", as.character(kordat$Intercept)))

ggplot(kordat, aes(x = f, y = Intercept, fill = f)) + geom_boxplot()
ggsave("boxplot.svg")
dev.off()

