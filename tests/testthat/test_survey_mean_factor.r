context("Quick tests for survey factors")

library(srvyr)
library(survey)

data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

# One group
out_survey <- svymean(~awards, dstrata)

out_srvyr <- dstrata %>%
  group_by(awards) %>%
  summarize(pct = survey_mean())

test_that(
  "survey_mean gets correct values for factors with single grouped surveys",
  expect_equal(c(out_survey[[1]], sqrt(diag(attr(out_survey, "var")))[[1]]),
               c(out_srvyr[[2]][[1]], out_srvyr[[3]][[1]])))

test_that("survey_mean preserves factor levels",
          expect_equal(levels(apistrat$awards), levels(out_srvyr$awards)))


out_srvyr <- dstrata %>%
  group_by(awards = as.character(awards)) %>%
  summarize(pct = survey_mean())


test_that("survey_mean preserves factor levels",
          expect_equal("character", class(out_srvyr$awards)))

# More than 2 groups
out_srvyr <- dstrata %>%
  group_by(stype, awards) %>%
  summarize(tot = survey_total())

out_survey <- svyby(~awards, ~stype, dstrata, svytotal)

test_that("survey_total is correct when doing props with multiple groups",
          expect_equal(out_survey[["awardsNo"]],
                       out_srvyr %>% filter(awards == "No") %>% .$tot))

test_that("survey_total is correct when doing props with multiple groups (se)",
          expect_equal(out_survey[["se.awardsNo"]],
                       out_srvyr %>%
                         filter(awards == "No") %>%
                         .$tot_se))

# Preserves factor orderings and character
out_srvyr <- dstrata %>%
  mutate(stype2 = relevel(stype, "H")) %>%
  group_by(stype2) %>%
  summarize(tot = survey_total())

test_that("survey_* preserves factor levels when calculating a statistic (1 grp)",
          expect_true(class(out_srvyr$stype2) == "factor" &
                        all(levels(out_srvyr$stype2) == c("H", "E", "M")))
          )

out_srvyr <- dstrata %>%
  mutate(stype2 = as.character(stype)) %>%
  group_by(stype2) %>%
  summarize(tot = survey_total())

test_that("survey_* preserves character when calculating a statistic (1 grp)",
          expect_true(class(out_srvyr$stype2) == "character")
)

out_srvyr <- dstrata %>%
  mutate(stype2 = relevel(stype, "H")) %>%
  group_by(awards, stype2) %>%
  summarize(tot = survey_total())

test_that("survey_* preserves factor levels when calculating a statistic (multi grps)",
          expect_true(class(out_srvyr$stype2) == "factor" &
                        all(levels(out_srvyr$stype2) == c("H", "E", "M")))
)

out_srvyr <- dstrata %>%
  mutate(stype2 = as.character(stype)) %>%
  group_by(awards, stype2) %>%
  summarize(tot = survey_total())

test_that("survey_* preserves character when calculating a statistic (multi grps)",
          expect_true(class(out_srvyr$stype2) == "character")
)


# confidence intervals
out_survey_mn <- svymean(~awards, dstrata)
out_survey_tot <- svytotal(~awards, dstrata)
out_survey <- dplyr::data_frame(
  awards = factor(c("No", "Yes")),
  pct = as.numeric(out_survey_mn),
  pct_low = as.numeric(confint(out_survey_mn)[, 1]),
  pct_upp = as.numeric(confint(out_survey_mn)[, 2]),
  tot = as.numeric(out_survey_tot),
  tot_low = as.numeric(confint(out_survey_tot)[, 1]),
  tot_upp = as.numeric(confint(out_survey_tot)[, 2])
)

out_srvyr <- dstrata %>%
  group_by(awards) %>%
  summarize(pct = survey_mean(vartype = "ci"),
            tot = survey_total(vartype = "ci"))

test_that(
  "survey_mean and survey_total work with cis",
  expect_equal(out_srvyr, out_survey))
