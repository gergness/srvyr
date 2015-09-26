<!-- README.md is generated from README.Rmd. Please edit that file -->
srvyr
=====

srvyr is a [dplyr-like](https://github.com/hadley/dplyr/) wrapper around the [survey](https://cran.r-project.org/web/packages/survey/index.html) package.

It attempts to bring the convenience of pipe-able functions to analysis of surveys, though currently it is very limited and slow.

You can try it out (only available on github at the moment):

``` r
devtools::install_github("gergness/srvyr")
```

To create a `svy_tbl` object (the core concept behind the srvyr package), use the function `design_survey()` with the bare column names of the names you would use in `survey::svydesign()` object.

``` r
library(survey)
data(api)

dstrata <- apistrat %>%
   design_survey(strata = stype, weights = pw)
```

Now many of the dplyr-verbs are available.

-   Use `mutate()` if you want to add or modify a variable.

    ``` r
    dstrata <- dstrata %>%
      mutate(api_diff = api00 - api99)
    ```

-   `summarise()` calculates summary statistics (currently only survey mean is implemented).

    ``` r
    dstrata %>% 
      summarise(api_diff = survey_mean(api_diff, vartype = "ci")))
    ```

-   Use `group_by()` if you want to summarise by groups.

    ``` r
    dstrata %>% 
      group_by(stype) %>%
      summarise(api_diff = survey_mean(api_diff, vartype = "ci")))
    ```
