
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maRsden

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of maRsden is to provide easy access to all data collected at
the Marsden Farm Long Term Cropping Systems Research Farm, managed by
Dr. Matt Liebman at Iowa State University.

## Installation

You can install the development version of maRsden using devtools once
it is no longer private.

``` r
#devtools::install_github("vanichols/maRsden")
```

## Progress

The package currently includes the following data:

1.  Weather from [Iowa Mesonet Ames Coop
    Station](https://mesonet.agron.iastate.edu/)

<!-- end list -->

  - Historical weather (1987-2018)
  - Data-weather years (1987-2018, will be updated once 2019 ends)

<!-- end list -->

2.  General experiment data

<!-- end list -->

  - mrs\_plotkey (each year’s plot treatments, 2012-2019)

<!-- end list -->

3.  Will Osterholtz (WO) data

<!-- end list -->

  - mrs\_cornbio\_wo (WO corn biomass data 2013-2014)
  - mrs\_soilh2o\_wo (WO soil water data 2013-2014)

<!-- end list -->

4.  My data from 2018-2019

<!-- end list -->

  - mrs\_residue18 (VN residue at planting 2018)
  - mrs\_nutrients18 (VN soil nutrients at planting in 2018)
  - mrs\_penetrom (VN penetrometer readings 2018-2019)
  - mrs\_rootdepth (VN max root depths 2018-2019)
  - mrs\_cornbio\_vn (VN corn biomass 2018-2019)
  - mrs\_cornlai\_vn (VN corn leaf area index 2018-2019)

## Example

If you want to load the corn yield data this will make it much easier.

``` r
#library(maRsden)

# data(mrs_cornyield)
```

Or maybe you want to look at Will Osterholtz’s corn biomass data. That’s
easy\!

``` r
#data(mrs_cornbio_wo)
```
