[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/funplot.svg?branch=master)](https://travis-ci.org/JohnCoene/funplot)

# funplot

Function Plot is a plotting library built on top of [D3.js](https://d3js.org/) used to render functions with little configuration (think of it as a clone of Google's plotting utility: y=x2)

The library currently supports interactive line charts and scatterplots, whenever the graph scale is modified the function is evaluated again with the new bounds, result: infinite graphs!

## Installation

``` r
# install.packages("devtools")
devtools::install_github("JohnCoene/funplot")
```
