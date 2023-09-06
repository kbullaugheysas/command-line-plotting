#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

usage <- function() {
  cat("usage: scatter_plot.R [options] datafile out.pdf\n")
  cat("options:\n")
  cat("   -xlab STRING      Label for x axis\n")
  cat("   -ylab STRING      Label for y axis\n")
  cat("   -size NUMBER      PDF width and height in inches\n")
  cat("   -header           Assume there is a header line\n")
  cat("   -square           Make the area coordinates a square region\n")
  cat("   -jitter           add jitter to both axes\n")
  cat("   -diagonal         Draw a diagonal dotted line\n")
  cat("   -little-dots      Draw points as little dots\n")
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
size <- 4
hasHeader <- FALSE
square <- FALSE
diagonal <- FALSE
littleDots <- FALSE
addJitter <- FALSE
pch <- 1
cex.points  <- 1

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
  } else if (option == "-header") {
    hasHeader <- TRUE
  } else if (option == "-little-dots") {
    littleDots <- TRUE
  } else if (option == "-diagonal") {
    diagonal <- TRUE
  } else if (option == "-jitter") {
    addJitter <- TRUE
  } else if (option == "-square") {
    square <- TRUE
  } else if (option == "-size") {
    i <- i+1
    value <- cmdOptions[i]
    if (substr(value, 1, 1) == "-") {
      usageError("Invalid value for -size:", value, "\n")
    }
    size <- as.numeric(value)
  } else {
    usageError("Invalid option", option, "\n")
  }
  i <- i+1
}


d <- read.table(file=fn, header=hasHeader, sep="\t")
ncol(d) == 2 || stop("expecting data file to have two columns")

if (hasHeader) {
  if (xlab == "X") { 
    xlab <- names(d)[1]
  }
  if (ylab == "Y") { 
    ylab <- names(d)[2]
  }
}

x <- d[,1]
y <- d[,2]

if (addJitter) {
  x <- jitter(x)
  y <- jitter(y)
}

xlim <- NULL
ylim <- NULL
if (square) {
  xlim <- ylim <- range(c(x,y))
}

if (littleDots) {
  pch <- 20
  cex.points <- 0.5
}

pdf(file=plotFile, width=size, height=size)
par(mar=c(4,4,1,1), mgp=c(2,0.8,0))
plot(x, y, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim, cex.axis=0.8, type="n")
if (diagonal) {
  abline(a=0, b=1, lty=2);
}
points(x, y, pch=pch, cex=cex.points)
dev.off()

cat("data points:", length(x), "\n")

cor.test(x, y)

# END
