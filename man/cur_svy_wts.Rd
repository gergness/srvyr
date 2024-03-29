% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_statistics_helpers.R
\name{cur_svy_wts}
\alias{cur_svy_wts}
\title{Get the full-sample weights for the current context}
\usage{
cur_svy_wts()
}
\value{
a numeric vector containing full-sample weights
}
\description{
This is a helper to allow srvyr's syntactic style. This function allows quick access
to the full-sample weights for the current group, using \code{cur_svy_wts()},
See \code{vignette("extending-srvyr")} for more details.
}
\examples{

data(api, package = 'survey')

dstrata <- apistrat \%>\%
  as_survey_design(strata = stype, weights = pw)

dstrata \%>\%
  summarize(sum_of_weights = sum(cur_svy_wts()),
            kish_deff = var(cur_svy_wts())/(mean(cur_svy_wts())^2))

}
