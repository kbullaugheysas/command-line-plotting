#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

usage <- function() {
  cat("usage: scatterplot.R [options] datafile out.pdf\n")
  cat("options:\n")
  cat("   -width INCHES         PDF width\n")
  cat("   -xlab STRING          x axis label\n")
  cat("   -ymin FLOAT           y axis minimum\n")
  cat("   -ymax FLOAT           y axis maximum\n")
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

width <- 6
ymin <- NULL
ymax <- NULL
xlab <- NULL

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
  } else if (option == "-width") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -width:", value, "\n")
    }
    width <- as.numeric(value)
  } else if (option == "-ymin") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -ymin:", value, "\n")
    }
    ymin <- as.numeric(value)
  } else if (option == "-ymax") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -ymax:", value, "\n")
    }
    ymax <- as.numeric(value)
  } else {
    usageError("Invalid option", option, "\n")
  }
  i <- i+1
}

d <- read.table(file=fn, header=TRUE, sep="\t")
ncol(d) == 2 || stop("expecting data file to have two columns")

ylim <- range(d[,2])
if (!is.null(ymin)) {
  ylim[1] <- ymin
}
if (!is.null(ymax)) {
  ylim[2] <- ymax
}

pdf(file=plotFile, width=width, height=4)
par(mar=c(4,4,1,1), mgp=c(2,0.8,0))
plot(d[,1], d[,2], xlab=names(d)[1], ylab=names(d)[2], cex.axis=0.8, ylim=ylim)
dev.off()

cor.test(d[,1], d[,2])
