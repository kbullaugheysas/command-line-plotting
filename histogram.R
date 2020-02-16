#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

usage <- function() {
  cat("usage: histogram.R [options] datafile out.pdf\n")
  cat("options:\n")
  cat("   -bins NUMBER      Number of bins for hist() function\n")
  cat("   -xlab STRING      Label for x axis\n")
  cat("   -width NUMBER     PDF width in inches\n")
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
doTable <- FALSE
width <- 4

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
  } else if (option == "-bins") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -bins:", value, "\n")
    }
    breaks <- as.numeric(value)
  } else if (option == "-width") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -width:", value, "\n")
    }
    width <- as.numeric(value)
  } else if (option == "-table") {
    doTable <- TRUE
  } else {
    usageError("Invalid option", option, "\n")
  }
  i <- i+1
}

x = scan(file=fn, what=0)

pdf(file=plotFile, width=width, height=4)
par(mar=c(4,4,1,1), mgp=c(2,0.8,0), xpd=NA)

if (doTable) {
  tbl <- table(x)
  labels <- names(tbl)
  x <- as.numeric(tbl)
  n <- length(x)
  cat("there are", n, "bars\n")
  barplot(x, names.arg=NA, xlab=xlab, cex.axis=0.9, space=0)
  text((0:(n-1)) + 0.5, max(x)*(-0.03), labels, adj=c(0.5,1), cex=0.5)
} else {
  hist(x, breaks=breaks, col="grey70", border="grey30", xlab=xlab, main="", cex.axis=0.8)
}

dev.off()
