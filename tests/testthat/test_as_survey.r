context("as_survey works.")
data <- data.frame(id1 = 1:2, id2 = 3:4, repwt1 = 1:0, repwt2 = 0:1,
                   wt = c(1 / 4, 3 / 4), x = 10:11)
pps <- "brewer" # test that it sees things outside of data.frame
test_that("as_survey correctly uses survey design's arguments",
          expect_equal(as_survey(data, ids = c(id1, id2), weights = wt,
                                 strata = repwt1, variables = x, pps = pps),
                       as_survey_design(data, ids = c(id1, id2), weights = wt,
                                     strata = repwt1, variables = x, pps = pps))
          )

test_that("as_survey_ correctly uses survey design's arguments",
          expect_equal(
            as_survey_(data, ids = c("id1", "id2"), weight = "wt",
                       strata = "repwt1", variables = "x", pps = pps),
            as_survey_design_(data, ids = c("id1", "id2"), weight = "wt",
                              strata = "repwt1", variables = "x", pps = pps)))


# Replicates
test_that("as_survey correctly uses replicate's arguments",
          expect_equal(
            as_survey(data, repweights = repwt1:repwt2, weight = wt,
                      variables = x),
            as_survey_rep(data, repweights = repwt1:repwt2, weight = wt,
                          variables = x)))

test_that("as_survey_ correctly uses replicate's arguments",
          expect_equal(
            as_survey_(data, repweights = c("repwt1", "repwt2"), weight = "wt",
                       variables = "x"),
            as_survey_rep_(data, repweights = c("repwt1", "repwt2"),
                           weight = "wt", variables = "x")))


# Twophase (hard to set up fake example - use from examples)
data(mu284, package = "survey")
mu284_1 <- mu284 %>%
  dplyr::slice(c(1:15, rep(1:5, n2[1:5] - 3))) %>%
  mutate(id = row_number(),
         sub = rep(c(TRUE, FALSE), c(15, 34 - 15)))

test_that("as_survey correctly uses twophase's arguments",
          expect_equal(
            mu284_1 %>%
              as_survey(id = list(id1, id), strata = list(NULL, id1),
                        fpc = list(n1, NULL), subset = sub),
            mu284_1 %>%
              as_survey_twophase(id = list(id1, id), strata = list(NULL, id1),
                                 fpc = list(n1, NULL), subset = sub)))

test_that("as_survey correctly uses twophase's arguments",
          expect_equal(
            mu284_1 %>%
              as_survey_(id = list("id1", "id"), strata = list(NULL, "id1"),
                         fpc = list("n1", NULL), subset = "sub"),
            mu284_1 %>%
              as_survey_twophase_(id = list("id1", "id"),
                                  strata = list(NULL, "id1"),
                                  fpc = list("n1", NULL), subset = "sub")))
