
<!-- README.md is generated from README.Rmd. Please edit that file -->

# subpat

![https://img.shields.io/badge/lifecycle-experimental-orange.svg](https://img.shields.io/badge/lifecycle-experimental-orange.svg)

> This package is NOT VALIDATED, and should be reserved for exploratory
> analysis only.

subpat is a collection of shiny modules to create subpopulations and
subgroups of clinical trial data. It was designed with [CDISC
ADaM](https://www.cdisc.org/standards/foundational/adam) data format in
mind but supports any data format. It features two main applications,
the Patient Listing Generator (PLG), and a TTE (time-to-event) analysis
app.

## Install

subpat is not available on CRAN. A development version can be installed
by running

``` r
devtools::install_github("Novartis/subpat")
```

## Patient Listing Generator (PLG)

PLG is designed for non-techincal users such as medical writers or
clinicians to find specific patients and generate ad-hoc listings. It
features an intuitive, graphical patient querying interface to find
specific subjects from a clinical trial. The user can then use the
created subpopulation to create ad-hoc (non-validated) listing to
export. All of the components of the application can easily be reused in
other Shiny applications due to the modular design.

## TTE (time-to-event) analysis

The time-to-event (TTE) analysis application is a graphical and
exploratory way for a statistician to analyze censored event data (such
as survival times). This app uses the same subpopulation module as the
PLG as well as a similarly designed subgroup creation interface. The
subpopulations and subgroups are then used in the Kaplan Meier plots and
Cox Proportional model modules. The variables are easily mapped to allow
for data in any format.

## Modules

It features modules for:

  - Subpopulation creation and editing
  - Subgroup creation and editing
  - Ad-hoc patient listings
  - Kaplan Meier survival plots and event table summary
  - Cox Proportional Hazard models

All of these modules are implemented with the
[`{tidymodules}`](https://opensource.nibr.com/tidymodules) R package.

## Usage

### Patient Listing Generator (PLG)

``` r
subpat::runPlg()
```

### TTE Analysis

``` r
subpat::runTTE()
```

### Modules

subpat features many example Shiny applications to get you started using
the modules. To see a list of examples, run

``` r
subpat::subpatExamples()
#> Example applications:
#> Call with index subpatExamples(2) or name subpatExamples('06_tte_simple')
#>   01_add-filter-example
#>   02_edit-population-example
#>   03_subpopulation-manager-example
#>   04_table-listing-example
#>   05_variable-selection-example
#>   06_tte_simple
#>   07_tte_mapping
#>   08_subgroup
```

All of the examples are found in
[inst/shiny-examples](inst/shiny-examples).

## Vignettes

A more detailed introduction is found in the vignettes

### PLG

``` r
vignette("plg", package = "subpat")
```

### Modules Introduction

``` r
vignette("modules", package = "subpat")
```

### TTE Module Integration

``` r
vignette("tte_integration.Rmd", package = "subpat")
```

## Thanks

Special thanks to the SCC team at Novartis

  - Mustapha Larbaoui [@m-l-1](https://github.com/m-l-1)
  - Xiao Ni [@xni7](https://github.com/xni7)
  - David Granjon [@DivadNojnarg](https://github.com/DivadNojnarg)
  - Douglas Robinson
  - Renan Sauteraud [@SRenan](https://github.com/SRenan)
  - Marzie Rasekh [@marzie-rasekh](https://github.com/marzie-rasekh)

Also thanks to Novartis for allowing this work to become an open-source
project. It was started during my summer 2019 internship in Basel.

## License

    Copyright 2020 Novartis AG
    
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
      http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
