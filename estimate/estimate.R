#!/usr/bin/env Rscript
library(bartMachine)

args = commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1], header=FALSE)

y <- unlist(data[1],  use.names=FALSE)
X <- data
X[1] <- NULL

bt <- bartMachine(X, y)
png(args[2])
pd_plot(bt, j = "V2")
dev.off()
