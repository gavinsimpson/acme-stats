# ACME Palaeo data science and statistical modelling workshop

## November 14&ndash;16, 2022

### Gavin Simpson

#### R and Packages

##### Install R

Install R, ideally version 4.2.2 (latest), but anything 4.1.0 or later should work. Download R from <https://cran.r-project.org/> and install it.

##### Install RStudio

Install RStudio. Download RStudio from <https://posit.co/download/rstudio-desktop/> and install it.

##### Update & install packages
```r
# update packages
cores <- parallel::detectCores() - 1
update.packages(ask = FALSE, Ncpus = cores)

# install new packaes
pkgs <- c("gapminder", "tidyverse", "readr", "readxl", "tidypaleo", "rioja",
    "analogue", "vegan", "pangaear", "janitor", "palmerpenguins", "here")

install.packages(pkgs, Ncpus = cores)

# install some dev packages
options(repos = c(
    gavinsimpson = 'https://gavinsimpson.r-universe.dev',
    nsj3 = "https://nsj3.r-universe.dev",
    CRAN = "https://cloud.r-project.org"))

install.packages(c("riojaPlot", "gratia"), Ncpus = cores)
```

## Slides

### Monday

1. [Introduction](https://gavinsimpson.github.io/acme-stats/01-monday/01-introduction.html)

2. [R and RStudio](https://gavinsimpson.github.io/acme-stats/01-monday/02-r-and-rstudio.html)

3. [Importing Data](https://gavinsimpson.github.io/acme-stats/01-monday/03-importing-data.html)

4. [Data Wrangling](https://gavinsimpson.github.io/acme-stats/01-monday/04-data-wrangling.html)

5. [Data Visualization](https://gavinsimpson.github.io/acme-stats/01-monday/05-data-viz.html)

### Tuesday

1. [Ordination I](https://gavinsimpson.github.io/acme-stats/02-tuesday/01-ordination-1.html)

2. [Ordination II](https://gavinsimpson.github.io/acme-stats/02-tuesday/02-ordination-2.html)

3. [Generalized additive models](https://gavinsimpson.github.io/acme-stats/02-tuesday/03-gams.html)

### Wednesday

1. [Transfer functions](https://gavinsimpson.github.io/acme-stats/03-tuesday/tranfun_talk_2.pdf)

## Download materials

Materials for the workshop can be downloaded from the workshop's [GitHub repo](https://github.com/gavinsimpson/acme-stats)