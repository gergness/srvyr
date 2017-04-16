# srvyr 0.2.0.9000
* Add support for mutate/summarize _at/_all/_if functions

* ids are now a required argument for as_survey_design to match survey's behavior.
  Use 0/1 to indicate that each row is a cluster.

# srvyr 0.2
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
