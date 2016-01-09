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
               c(out_srvyr[[1, 2]], out_srvyr[[1, 3]])))

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
          expect_true(class(out_srvyr$stype) == "factor" &
                        all(levels(out_srvyr$stype) == c("H", "E", "M")))
          )

out_srvyr <- dstrata %>%
  mutate(stype2 = as.character(stype)) %>%
  group_by(stype2) %>%
  summarize(tot = survey_total())

test_that("survey_* preserves character when calculating a statistic (1 grp)",
          expect_true(class(out_srvyr$stype) == "character")
)

out_srvyr <- dstrata %>%
  mutate(stype2 = relevel(stype, "H")) %>%
  group_by(awards, stype2) %>%
  summarize(tot = survey_total())

test_that("survey_* preserves factor levels when calculating a statistic (multi grps)",
          expect_true(class(out_srvyr$stype) == "factor" &
                        all(levels(out_srvyr$stype) == c("H", "E", "M")))
)

out_srvyr <- dstrata %>%
  mutate(stype2 = as.character(stype)) %>%
  group_by(awards, stype2) %>%
  summarize(tot = survey_total())

test_that("survey_* preserves character when calculating a statistic (multi grps)",
          expect_true(class(out_srvyr$stype) == "character")
)
