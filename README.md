# Command line plotting

Simple plotting scripts in R for quickly plotting from the command line

There are three scripts:

1. `barplot.R`
2. `scatterplot.R`
3. `histogram.R`

# barplot.R

    usage: barplot.R [options] datafile out.pdf
    options:
       -xlab STRING    Label for x axis
       -rotate-xlab    Rotate the x labels
       -table          Send the data through table first
       -width          PDF width

# histogram.R

    usage: histogram.R [options] datafile out.pdf
    options:
       -bins NUMBER      Number of bins for hist() function
       -xlab STRING      Label for x axis
       -width NUMBER     PDF width in inches
       -barplot          Count distinct values instead and draw a barplot
