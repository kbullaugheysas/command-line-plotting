#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

usage <- function() {
  cat("usage: histogram.R [options] datafile out.pdf\n")
  cat("options:\n")
  cat("   -bins NUMBER      Number of bins for hist() function\n")
  cat("   -xlab STRING      Label for x axis\n")
  cat("   -ylab STRING      Label for y axis\n")
  cat("   -title STRING     Plot label at the top\n")
  cat("   -rotate-xlab      Rotate the x labels\n")
  cat("   -max NUMBER       Values above this will be filtered out\n")
  cat("   -ylim NUMBER      Set high end of the vertial axis\n")
  cat("   -width NUMBER     PDF width in inches\n")
  cat("   -cex NUMBER       Relative size of axis labels\n")
  cat("   -barplot          Count distinct values instead and draw a barplot\n")
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

breaks <- 20
xlab <- "X"
ylab <- "Frequency"
doTable <- FALSE
width <- 4
maxVal <- Inf
srt <- 0
adj <- c(0.5, 1)
title <- ""
ylim <- NULL
cex <- 1.0

i <- 1
while (i <= length(cmdOptions)) {
  option <- cmdOptions[i]
  if (option == "-xlab") {
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
  } else if (option == "-title") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -title:", value, "\n")
    }
    title <- value
  } else if (option == "-rotate-xlab") {
    srt <- 90
  } else if (option == "-bins") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -bins:", value, "\n")
    }
    breaks <- as.numeric(value)
  } else if (option == "-max") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -max:", value, "\n")
    }
    maxVal <- as.numeric(value)
  } else if (option == "-ylim") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -ylim:", value, "\n")
    }
    ylim <- c(0, as.numeric(value))
  } else if (option == "-width") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -width:", value, "\n")
    }
    width <- as.numeric(value)
  } else if (option == "-cex") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -cex:", value, "\n")
    }
    cex <- as.numeric(value)
  } else if (option == "-barplot") {
    doTable <- TRUE
  } else {
    usageError("Invalid option", option, "\n")
  }
  i <- i+1
}

if (srt == 90) {
  adj <- c(1, 0.5)
}

x <- scan(file=fn, what=0)
tooBig <- x > maxVal
x <- x[!tooBig] 

cat("filtered out", sum(tooBig), "values larger than", maxVal, "\n")

pdf(file=plotFile, width=width, height=4)
par(mar=c(4,4,3,1), mgp=c(2.2,0.8,0), xpd=NA, cex.lab=cex)

if (doTable) {
  tbl <- table(x)
  labels <- names(tbl)
  x <- as.numeric(tbl)
  n <- length(x)
  cat("there are", n, "bars\n")
  barplot(x, names.arg=NA, xlab=xlab, cex.axis=0.9, ylab=ylab, space=0)
  text((0:(n-1)) + 0.5, max(x)*(-0.03), labels, cex=0.5, srt=srt, adj=adj)
} else {
  hist(x, breaks=breaks, col="grey70", border="grey30", ylab=ylab, xlab=xlab, main=title, cex.axis=0.8, ylim=ylim)
}

dev.off()
