#' svrvyr: A package for 'dplyr'-Like Syntax for Summary Statistics of Survey Data.
#'
#' The srvyr package provides a new way of calculating summary statistics
#' on survey data, based on the dplyr package. There are three stages to using
#' srvyr functions, creating a survey object, manipulating the data, and
#' calculating survey statistics.
#'
#' @section Functions to create a survey object:
#' \code{\link{as_survey_design}}, \code{\link{as_survey_rep}},
#' and \code{\link{as_survey_twophase}} are used to create surveys based on
#' a data.frame and design variables, replicate weights or two phase design
#' respectively. Each is based on a function in the survey package
#' (\code{\link[survey]{svydesign}}, \code{\link[survey]{svrepdesign}},
#' \code{\link[survey]{twophase}}), and it is easy to modify code that uses
#' the survey package so that it works with the srvyr package. See
#' \code{vignette("srvyr_vs_survey")} for more details.
#'
#' The function \code{\link{as_survey}} will choose between the other three
#' functions based on the arguments given to save some typing.
#'
#' @section Functions to manipulate data in a survey object:
#' Once you've created a survey object, you can manipulate the data as you would
#' using dplyr with a data.frame. \code{\link{mutate}} modifies or creates a variable,
#' \code{\link{select}} and \code{\link{rename}} select or rename variables, and
#' \code{\link{filter}} keeps certain observations.
#'
#' Note that \code{arrange} and two table verbs such as \code{bind_rows},
#' \code{bind_cosl}, or any of the joins are not usable on survey objects
#' because they might require modificaitons to the definition of your survey. If
#' you need to use these variables, you should do so before you convert the
#' data.frame to a survey object.
#'
#' @section Functions to summarize a survey object:
#' Now that you have your data set up correctly, you can calculate summary
#' statistics. To get the statistic over the whole population, use
#' \code{\link{summarise}}, or to calculate it over a set of groups, use
#' \code{\link{group_by}} first.
#'
#' You can calculate the mean, (with \code{\link{survey_mean}}), the total
#' (\code{\link{survey_total}}), the quantile (\code{\link{survey_quantile}}),
#' or a ratio (\code{\link{survey_ratio}}). By default, srvyr will return the
#' statistic and the standard error around it in a data.frame, but with the
#' \code{vartype} parameter, you can also get a confidence interval ("ci"),
#' variance ("var"), or coefficient of variation ("cv").
#'
#' Within summarise, you can also use \code{\link{unweighted}}, which calculates
#' a function without taking into consideration the survey weighting.
#'
#' @docType package
#' @name srvyr
NULL
