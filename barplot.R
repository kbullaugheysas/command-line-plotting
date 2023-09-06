#!/usr/bin/env Rscript

# This script expects two, tab-separated columns. The first is the labels of
# the bar plot and the second column is the numerical values.

args = commandArgs(trailingOnly=TRUE)

usage <- function() {
  cat("usage: barplot.R [options] datafile out.pdf\n")
  cat("options:\n")
  cat("   -ylab STRING      Label for y axis\n")
  cat("   -xlab STRING      Label for x axis\n")
  cat("   -rotate-xlab      Rotate the x labels\n")
  cat("   -width INT        PDF width\n")
  cat("   -xlab-cex FLOAT   PDF width\n")
  cat("   -main STRING      title for plot\n")
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

xlab <- "X"
ylab <- "Y"
srt <- 0
width <- 6
plotTitle <- ""
xlabCex <- NULL

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
  } else if (option == "-main") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -main:", value, "\n")
    }
    plotTitle <- value
  } else if (option == "-width") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -width:", value, "\n")
    }
    width <- as.numeric(value)
  } else if (option == "-xlab-cex") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -xlab-cex:", value, "\n")
    }
    xlabCex <- as.numeric(value)
  } else {
    usageError("Invalid option", option, "\n")
  }
  i <- i+1
}

d <- read.table(file=fn, sep="\t", stringsAsFactors=FALSE, header=FALSE)

x <- d$V2
labels <- d$V1
n <- length(x)
height <- 4.5
bottomMar <- 4
adj <- c(0.5, 1)

if (srt == 90) {
  height <- 5.2
  bottomMar <- 9
  adj <- c(1, 0.5)
  if (is.null(xlabCex)) {
    xlabCex <- 0.65
  }
} else {
  if (is.null(xlabCex)) {
    xlabCex <- 0.8
  }
}

pdf(file=plotFile, width=width, height=height)
par(mar=c(bottomMar, 4, 3, 1), mgp=c(2,0.8,0), xpd=NA)
barplot(x, names.arg=NA, xlab=xlab, ylab=ylab, cex.axis=0.9, space=0.1, xaxs="i", yaxs="i", main=plotTitle)
text((0:(n-1)) * 1.1 + 0.6, max(x, na.rm=TRUE)*(-0.03), labels, cex=xlabCex, srt=srt, adj=adj)
dev.off()
