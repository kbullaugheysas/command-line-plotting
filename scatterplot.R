#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

length(args) == 2 || stop("must supply data filename and pdf filename")

fn <- args[1]
plotFile <- args[2]

d <- read.table(file=fn, header=TRUE, sep="\t")
ncol(d) == 2 || stop("expecting data file to have two columns")

pdf(file=plotFile, width=4, height=4)
par(mar=c(4,4,1,1), mgp=c(2,0.8,0))
plot(d[,1], d[,2], xlab=names(d)[1], ylab=names(d)[2], cex.axis=0.8)
dev.off()

cor.test(d[,1], d[,2])
