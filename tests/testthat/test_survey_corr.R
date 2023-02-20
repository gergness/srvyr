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

# --- No built-in equivalent in survey, so just hard code expectation:
expected <- data.frame(
  api_corr = 0.975904664064127,
  api_corr_se = 0.00400650195314815
)

test_that("ungrouped correlations works correctly",
          expect_df_equal(out_def_srvyr, expected))


## Testing grouped

out_by_srvyr <-
  dstrata %>%
  group_by(stype) %>%
  summarize(api_corr = survey_corr(x = api00, y = api99))

# --- No built-in equivalent in survey, so just hard code expectation:
expected <- data.frame(
  stype = factor(c("E", "H", "M")),
  api_corr = c(0.979106531819024, 0.976503255665411, 0.983299653441321),
  api_corr_se = c(0.00450564184375547, 0.00682594587202227, 0.00392151702187407)
)

test_that("grouped correlations works correctly",
          expect_df_equal(out_by_srvyr, expected))
