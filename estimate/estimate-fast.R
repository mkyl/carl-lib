#!/usr/bin/env Rscript

library(dplyr)

# columns are Y, T, Z1, Z2, ...
args = commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1], header=FALSE)

y <- unlist(data[1],  use.names=FALSE)
X <- data
X[1] <- NULL

t <- unlist(data[2],  use.names=FALSE)
t <- t > 0.3
X[1] <- NULL

c <- kmeans(X, 25)$cluster

p <- data.frame("cluster" = c, "treatment" = t, "outcome" =  y)

h <- summarise(group_by(p, cluster, treatment), median(outcome)) %>% mutate(dValue = `median(outcome)`[treatment == TRUE] - `median(outcome)`) %>% filter(!treatment)
t <- h$dValue * table(c) / sum(table(c))
s <- paste("Average Treatment Effect: ", mean(t))

capture.output(s, file = args[3])

png(args[2], width=480, height=250)
boxplot(c(t), horizontal = TRUE, main = "Distribution of Treatment Effects", xlab="Difference in outcome")
dev.off()
