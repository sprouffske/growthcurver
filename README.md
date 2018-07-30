# growthcurver
[![Travis-CI Build Status](https://travis-ci.org/sprouffske/growthcurver.svg?branch=master)](https://travis-ci.org/sprouffske/growthcurver)

Growthcurver is an R package that fits growth curve data to a standard form of the logistic equation common in ecology and evolution whose parameters (the growth rate, the initial population size, and the carrying capacity) provide meaningful population-level information with straight-forward biological interpretation. 

You can install the latest released version from CRAN from within R with
  ```R
  install.packages("growthcurver")
  ````

You can install the latest development version from github with
  ```R
  # install devtools first if you don't already have the package
  install.packages("devtools")

  # then install growthcurver
  devtools::install_github("sprouffske/growthcurver")
  ```

## Using growthcurver

The easiest way to get started with growthcurver is to work through the examples in the vignette. In the vignette, you can find information on
* What your input data should look like
* How to use growthcurver to get summary metrics on a single growth curve sample
* How to use growthcurver to get summary metrics on an entire plate of growth curves
* What those metrics mean and some best practices for quality control

You can find the vignette at bit.ly/1p7w6dJ. 

## A simple working example

This code loads the `growthcurver` package and some sample data. Then, it calls `SummarizeGrowth` to do the analysis. 
```R
library(growthcurver)                    # load the package
d <- growthdata                          # load some sample, simulated data
gc_fit <- SummarizeGrowth(d$time, d$A1)  # do the analysis
plot(gc_fit)                             # plot your data and the best fit
gc_fit                                   # view some returned metrics
```

 
  



