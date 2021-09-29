# GenderInfer


The goal of `GenderInfer` is to analysed data for gender differences in publishing.
It assigns the gender based on the first name. It should only be used when
self-reported gender is unavailable. 
This package let possible to find if there are significant differences between 
male and female from a specified baselines.

## Installation

You can install the released version of GenderInfer from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("GenderInfer")
```
It is also possible to install the package directly from bitbucket 

``` r
devtools::install_bitbucket("rscapplications/genderinfer")
```


The package use the following packages as dependencies:
``` r
  binom
  ggplot2
```
  
## Example

This is a basic example which shows you how to assign gender to a data frame containing first names:

``` r
library(GenderInfer)
## assign gender
authors_df <- assign_gender(data_df = authors, first_name_col = first_name)
head(authors_df)
```

