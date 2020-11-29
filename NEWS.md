# srvyr (development version)
* `summarize` has been rearchitected, 
  * main user facing improvements are:
    * `dplyr::across()` now works within it
    * dplyr functions like `dplyr::cur_group()`, `dplyr::cur_group_id()`, `dplyr::cur_data()`
    work in it (as well as new anlagous functions srvyr-specific `cur_svy()` and 
    `cur_svy_full()`)
  * The only known breaking change is:
    * objects in the `summarize` will refer to the output of `summarize` before the input.
      Meaning code that looks like this:
      ```r
      dstrata %>% summarize(api99 = survey_mean(api99), api_diff = survey_mean(api00 - api99)) 
      ```
      will now error because it calculates the mean of `api99` before using it inside of the
      calculation for `api_diff`. This behavior better matches `dplyr`'s so will likely be
      kept.
* Support for `group_split()`, `group_nest()` and `nest_by()` were added for `tbl_svy` objects.

# srvyr 0.4.0
* Fix to ensure that ordered factors can be used as grouping variables or as inputs to `survey_count` and `survey_tally` (#92, thanks for reporting @szimmer & @walkerke & for fixing @bschneidr).

* Fix to ensure that numeric values can be used in grouping variables (#78 & #74, thanks for reporting @tzoltak & fix @bschneidr)

* Some improvements for dplyr 1.0 (#79) `transmute()` now works (thanks for reporting @caayala), `summarise()`'s `.groups` argument is respected, and multi-row returns to `summarise()` work. (Unfortunately the new `across()` function isn't quite supported in `summarise()` yet, it will hopefully come soon)

# srvyr 0.3.10
* Another fix for upcoming dplyr
* Fix in vignette for changes to vardpoor package

# srvyr 0.3.9
* Fix for upcoming version of dplyr (thanks @romainfrancois)

# srvyr 0.3.8
* `unweighted` now evaluates in the right context and so will provide correct error
  when an incorrectly interpolated function is used (#70, thanks for reporting @tlmcmurry)
  
* `filter_at` works now, (#57, thanks for reporting @dcaseykc & helping @bschneidr).

* Fix for upcoming version of tibble (#72).

# srvyr 0.3.7
* `filter`ing on grouped survey designs now works correctly (#54, thanks for reporting @dcaseykc)

* Added function `pull` (#63, thanks @dcaseykc)

* `df` parameter now set to be degrees of freedom of survey for quantiles and variance to match other 
   functions.
   
* Updated tests to work with upcoming version of survey (#66).

# srvyr 0.3.6
* Small update to quasiquotation syntax inside `unweighted` to improve consistency with recent rlang updates (#54).

* Added functions `survey_tally()` and `survey_count()` (#53)

# srvyr 0.3.5
* New functions survey_var and survey_sd to calculate population variance and
  standard deviaton.

* Computation of standard errors in all survey_ functions can be suppressed
  by setting vartype=NULL (#45, thanks @tzoltak).

* Fixed an issue where you'd get an error when summarize components returned 
  different lengths of data - usually when factor levels were not present
  in the data (#49).
  
* Removed references to MonetDBLite since it has been removed from CRAN.

* Small updates to replace soft-deprecated dplyr functions with their tibble
  and tidyselect equivalents (#52, thanks @bschneidr).

# srvyr 0.3.4
* survey_mean/survey_total allow `deff="replace"` like their survey package
  forbearers. (#46, thanks @mandes95)
  
* Fixes for new release of dplyr

# srvyr 0.3.3
* Add warning to explain that design effects cannot be calculated on
  proportions. (#39, thanks @mlaviolet)
  
* Remove dependency on stringr in tests and add DBI to suggests
  so that test dependencies are correctly specified (#40, thanks CRAN!)

# srvyr 0.3.2
* Bug fix for calculating multiple quantiles on grouped data
  (#38, thanks @iantperry)

# srvyr 0.3.1
* When converting from a survey db-backed survey to a srvyr one
  srvyr now tries to capture the updates you've already sent.
  If dbplyr can convert the function, then it will bring the
  update. If it can't it will warn you (#35).

* Small bug fixes, mostly having to do with CRAN checks, 
  running on CI services, or for upstream rev dep checks.

# srvyr 0.3.0
* srvyr now uses tidy evaluation from rlang. The "underscore" functions 
have been soft deprecated in favor of quosure splicing. See dplyr's
vignette "programming" for more details. In almost all cases, the old syntax
will still work, with one exception: the standard
evaluation function `as_survey_twophase_()` had to be changed slightly
so that the entire list is inside quotation. 

* Datbase support has been rewritten. It should be faster now and doesn't
require a unique identifier. You also can now convert survey db-backed surveys
to srvyr with as_survey.

* srvyr now has a pkgdown site, check it out at <http://gdfe.co/srvyr>

# srvyr 0.2.2
* Remove test blocking survey update

# srvyr 0.2.1
* Added support for dplyr mutate_at/_if/_all and summarize_at/_if/_all for 
srvyr surveys.

* Fixed a few bugs introduced with dplyr 0.6. This version of srvyr will work
with both old versions of dplyr and 0.6, but may be full of warnings if you
update dplyr. Full support for the new dplyr is coming soon.

# srvyr 0.2.0
* Added support for database backed surveys, using dplyr's handling of
DBI. Because of problems interacting with the survey package twophase designs 
do not work. 

# srvyr 0.1.2
* Fixed a problem with confidence levels not being passed into quantiles

* Added deff parameter to `survey_mean()`, `survey_total()` and `survey_median()`, and 
  a df parameter to those functions and `survey_quantile()` / `survey_median()`.

* `summarize` and `mutate` match dplyr's behavior when arguments aren't named 
  (uses `dplyr::auto_name()`)

# srvyr 0.1.1

* New function `cascade` summarizes groups, and cascades to create
  summary statistics of groups of groups.

* Fixed a bug for confidence intervals for `survey_total()` on groups.

* Fixed some issues with the upcoming version of dplyr.
