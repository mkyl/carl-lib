#!/usr/bin/env Rscript

library(bartCause)
# library(bartMachine)

# columns are Y, T, Z1, Z2, ...
args = commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1], header=FALSE)

y <- unlist(data[1],  use.names=FALSE)
X <- data
X[1] <- NULL

# bt <- bartMachine(X, y)
# pd_plot(bt, j = "V2")

t <- unlist(data[2],  use.names=FALSE)
t <- t > 0.3
X[1] <- NULL

colnames(X)

bt2 <- bartc(y, t, X)
summary(bt2)

png(args[2])
plot(fitted(bt2, type = "mu.obs"))
dev.off()
