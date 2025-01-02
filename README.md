
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DHSCconsultations

<!-- badges: start -->
<!-- badges: end -->

The goal of DHSCconsultations is to provide a framework for performing
consultation response analysis within the Department of Health & Social
Care.

## Installation

You can install the development version of DHSCconsultations like so:

``` r
if (!requireNamespace("librarian")) install.packages("librarian")
librarian::stock(DataS-DHSC/DHSCdatatools)
```

To reinstall DHSCdatatools use:

``` r
librarian::stock(DataS-DHSC/DHSCdatatools, update_all = TRUE)```
```

## Usage

### Random Seeds

For reproducibility we give the lda functions a random seed. We
recommend setting this to the long-form of a date relevant to the
project, e.g. 20250102. This ensures that different projects are using
different seeds, which has become an issue in random simulations.
e.g. some projects had statistical quirks because almost every project
was using one of “42”, “1234”, … etc.

For best practise we would recommend using a second seed as a
sensitivity analysis - asking whether the results are similar-enough if
the seed is changed.

### Statistical Disclosure

*If in doubt on this section, consult a member of the Government
Statistical Service (GSS)*

The outputs of this library should not be immediately published, but
will likely go into the consultation response publication. It is
important that individuals are not identifiable from the outputs.

Before publication, counts should be rounded to the nearest 5 and counts
below 10 should be suppressed.

This should be considered within the context that a consultation is not
a survey, and the outputs are not intended to be representative of the
population.

## Code of Conduct

Please note that the DHSCconsultations project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## Licence

Unless stated otherwise, the codebase is released under the MIT License.
This covers both the codebase and any sample code in the documentation.

All other content is [© Crown
copyright](http://www.nationalarchives.gov.uk/information-management/re-using-public-sector-information/uk-government-licensing-framework/crown-copyright/)
and available under the terms of the [Open Government 3.0
licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/),
except where otherwise stated.
