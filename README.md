[![Travis-CI Build Status](https://travis-ci.org/JohnCoene/funplot.svg?branch=master)](https://travis-ci.org/JohnCoene/funplot)

# funplot

* [Installation](#installation)
* [Examples](#examples)
* [Website](http://john-coene.com/funplot)

Function Plot is a plotting library built on top of [D3.js](https://d3js.org/) used to render functions with little configuration (think of it as a clone of Google's plotting utility: y=x2)

The library currently supports interactive line charts and scatterplots, whenever the graph scale is modified the function is evaluated again with the new bounds, result: infinite graphs!

## Installation 

``` r
# install.packages("devtools")
devtools::install_github("JohnCoene/funplot")
```
# Examples

```r
library(funplot)

# basic equation
funplot() %>%
  fun_add(fun = "sin(x)")

# parametric equation
funplot() %>%
  fun_add_param(
    x = "sin(t) * (exp(cos(t)) - 2 cos(4t) - sin(t/12)^5)",
    y = "cos(t) * (exp(cos(t)) - 2 cos(4t) - sin(t/12)^5)"
  )

# polar equation
funplot() %>%
  fun_add_polar(r = "2 * sin(4 theta)")

# implicit function
funplot() %>%
  fun_add_imp("x ^ 2 + y ^ 2 - 1")

# multiple functions
funplot() %>%
  fun_add("sqrt(1 - x * x)") %>%
  fun_add("-sqrt(1 - x * x)")

# n samples (precision)
funplot() %>%
  fun_add(fun = "sin(x)", samples = 1000)

# area
funplot() %>%
  fun_add("1/x * cos(1/x)", closed = TRUE) %>%
  fun_x("log", domain = list(0.01, 1)) %>%
  fun_y(domain = list(-100, 100))

# color and type
funplot() %>%
  fun_add("x", color = "black") %>%
  fun_add("-x") %>%
  fun_add("-sqrt(x)", type = "scatter", samples = 100) %>%
  fun_add("sqrt(x)", tip = TRUE)

# nthRoot
funplot() %>%
  fun_add("nthRoot(x, 3)^2")

# secants and derivative
funplot() %>%
  fun_add("x * x") %>%
  fun_deriv("2 * x", mouse = TRUE) %>%
  fun_add("x * x * x") %>%
  fun_secants(x0 = 5, mouse = TRUE)

# lines and points
funplot() %>%
  fun_points(points) %>%
  fun_lines(lines)

# annotate
funplot() %>%
  fun_add("x^2") %>%
  fun_annot(x = 1, text = "x = 1") %>%
  fun_annot(y = 2, text = "y = 2")

# custom axis and grid
# disable zoom
funplot() %>%
  fun_add("x ^ 2") %>%
  fun_x(label = "x axis", domain = list(-6, 6)) %>%
  fun_y(type = "log", "log scale") %>%
  fun_grid() %>% 
  fun_zoom() %>% 
  fun_title("Custom axis")
  
# use math.js
funplot() %>%
  fun_math("atan2(x, x ^ 2) / pi", samples = 4000) %>%
  fun_y(domain = list(-1, 1))
```
