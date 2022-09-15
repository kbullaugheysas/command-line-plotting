#!/usr/bin/env Rscript

# This script expects two, tab-separated columns. The first is the labels of
# the bar plot and the second column is the numerical values.

args = commandArgs(trailingOnly=TRUE)

usage <- function() {
  cat("usage: barplot.R [options] datafile out.pdf\n")
  cat("options:\n")
  cat("   -xlab STRING    Label for x axis\n")
  cat("   -ylab STRING    Label for y axis\n")
  cat("   -rotate-xlab    Rotate the x labels\n")
  cat("   -table          Send the data through table first\n")
  cat("   -space          Barplot bar separation space\n")
  cat("   -cex            text size of bar names\n")
  cat("   -width          PDF width\n")
}

usageError <- function(...) {
  cat(..., file=stderr())
  usage()
  quit()
}

if (length(args) < 2) {
  cat("too few arguments\n", file=stderr())
  usage()
  quit()
}

cmdOptions <- head(args, length(args)-2)
fn <- args[length(args)-1]
plotFile <- args[length(args)]

if (substr(fn, 1, 1) == "-" || substr(plotFile, 1, 1) == "-") {
  usageError("missing data file or pdf\n")
}

ylab <- ""
xlab <- "X"
srt <- 0
width <- 6
space <- 0.1
cexNames <- 1.0

i <- 1
while (i <= length(cmdOptions)) {
  option <- cmdOptions[i]
  if (option == "-rotate-xlab") {
    srt <- 90
  } else if (option == "-xlab") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -xlab:", value, "\n")
    }
    xlab <- value
  } else if (option == "-ylab") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -ylab:", value, "\n")
    }
    ylab <- value
  } else if (option == "-width") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -width:", value, "\n")
    }
    width <- as.numeric(value)
  } else if (option == "-space") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -space:", value, "\n")
    }
    space <- as.numeric(value)
  } else if (option == "-cex") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -cex:", value, "\n")
    }
    cexNames <- as.numeric(value)
  } else {
    usageError("Invalid option", option, "\n")
  }
  i <- i+1
}

d <- read.table(file=fn, sep="\t", stringsAsFactors=FALSE, header=FALSE)

x <- d$V2
labels <- d$V1
n <- length(x)
height <- 4
bottomMar <- 4
adj <- c(0.5, 1)

if (srt == 90) {
  height <- 5
  bottomMar <- 7
  adj <- c(1, 0.5)
}

pdf(file=plotFile, width=width, height=height)
par(mar=c(bottomMar, 4, 1, 1), mgp=c(2.1,0.8,0), xpd=NA)
barplot(x, names.arg=NA, xlab=xlab, cex.axis=0.9, space=space)
text((0:(n-1)) * (1+space) + 0.5+space, max(x)*(-0.03), labels, cex=cexNames, srt=srt, adj=adj)
mtext(ylab, side=2, line=2.5)
dev.off()
