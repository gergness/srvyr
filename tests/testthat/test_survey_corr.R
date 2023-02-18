context("Quick tests for survey correlations")

library(srvyr)
library(survey)
source("utilities.R")

data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

out_def_srvyr <-
  dstrata %>%
  summarize(api_corr = survey_corr(x = api00, y = api99))

svycor <- function(formula, des){
  out_survey_var <- svyvar(~api00+api99, design=des)
  point_estimates <- as.vector(coef(out_survey_var)[c(1,2,4)])
  names(point_estimates) <- c('var_x', 'cov_xy', 'var_y')
  vcov_mat <- vcov(out_survey_var)[c(1,2,4), c(1,2,4)]
  rownames(vcov_mat) <- names(point_estimates)
  colnames(vcov_mat) <- names(point_estimates)
  class(point_estimates) <- "svystat"
  attr(point_estimates, 'var') <- vcov_mat
  attr(point_estimates, 'statistic') <- "covariances"

  survey::svycontrast(
    stat = point_estimates, contrasts = list(
      'corr' = quote(
        cov_xy / sqrt(var_x * var_y)
      )
    )) %>%
    as_tibble() %>%
    set_names(names(out_def_srvyr))
}

out_def_survey <- svycor(~api00+api99, dstrata)

test_that("ungrouped correlations works correctly",
          expect_df_equal(out_def_srvyr, out_def_survey))


## Testing grouped

out_by_srvyr <-
  dstrata %>%
  group_by(stype) %>%
  summarize(api_corr = survey_corr(x = api00, y = api99))

out_by_survey <- NULL
for (i in levels(apistrat$stype)){
  out_by_survey <- rbind(out_by_survey, tibble::tibble(stype=i, svycor(~api00+api99, filter(dstrata, stype==i))))
}
out_by_survey$stype <- factor(out_by_survey$stype, levels=levels(apistrat$stype))

test_that("grouped correlations works correctly",
          expect_df_equal(out_by_srvyr, out_by_survey))
