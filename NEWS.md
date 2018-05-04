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
