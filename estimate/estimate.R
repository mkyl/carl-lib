#!/usr/bin/env Rscript

#!/usr/bin/env Rscript

library(dplyr)

# columns are Y, T, Z1, Z2, ...
args = commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1], header=FALSE)

y <- unlist(data[1],  use.names=FALSE)
X <- data
X[1] <- NULL

t <- unlist(data[2],  use.names=FALSE)
t <- t > 0.7
X[1] <- NULL

c <- kmeans(X, 4)$cluster

p <- data.frame("cluster" = c, "treatment" = t, "outcome" =  y)

h <- summarise(group_by(p, cluster, treatment), median(outcome)) %>% mutate(dValue = `median(outcome)`[treatment == TRUE] - `median(outcome)`) %>% filter(!treatment)
t <- h$dValue * table(c) / sum(table(c))
s <- paste("Average Treatment Effect: ", mean(t))

capture.output(s, file = args[3])

png(args[2], width=480, height=250)
boxplot(c(t), horizontal = TRUE, main = "Distribution of Treatment Effects", xlab="Difference in outcome")
dev.off()


# library(bartCause)
# library(ggplot2)

# # columns are Y, T, Z1, Z2, ...
# args = commandArgs(trailingOnly=TRUE)
# data <- read.csv(args[1], header=FALSE)

# y <- unlist(data[1],  use.names=FALSE)
# X <- data
# X[1] <- NULL

# t <- unlist(data[2],  use.names=FALSE)
# t <- t > 0.3
# X[1] <- NULL

# bt2 <- bartc(y, t, X)#, n.samples = 2, n.burn = 2, n.chains = 2)

# s <- summary(bt2)
# capture.output(s, file = args[3])

# png(args[2])
# u <- summary(bt2)$estimates$ci.upper
# l <- summary(bt2)$estimates$ci.lower
# e <- summary(bt2)$estimates$estimate
# plot_df <- data.frame(X = "", Avg=e, SE_high=u, SE_low=l)
# ggplot(plot_df, aes(x = X, y = Avg, ymin = SE_low, ymax = SE_high)) + geom_point() + geom_errorbar(width = 0.1) + labs(x = "", y = "Average Treatment Effect")

# dev.off()
