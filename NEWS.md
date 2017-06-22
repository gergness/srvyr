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
