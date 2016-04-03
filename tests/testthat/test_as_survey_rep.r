context("as_survey_rep arguments work as expected.")
library(srvyr)
suppressPackageStartupMessages(library(dplyr))

# From survey::as.svrepdesign examples
suppressPackageStartupMessages(library(survey))
data(scd)
scddes <- svydesign(data = scd, prob = ~1, id = ~ambulance, strata = ~ESA,
                  nest = TRUE, fpc = rep(5, 6))
scdnofpc <- svydesign(data = scd, prob = ~1, id = ~ambulance, strata = ~ESA,
                    nest = TRUE)

# ------------------------------------------------------------------
# Test BRR replicate weights
# ------------------------------------------------------------------
scd2brr <- as.svrepdesign(scdnofpc, type="BRR", compress = FALSE)

scd2brr_srvyr <- scd2brr$repweights %>%
  unclass() %>% as.data.frame() %>%
  setNames(paste0("rep", 1:4)) %>%
  dplyr::bind_cols(scd) %>%
  mutate(weights = 1) %>%
  as_survey_rep(repweights = starts_with("rep"),
                    type = "BRR", rho = scd2brr$rho,
                    scale = scd2brr$scale, rscales = scd2brr$rscales,
                    mse = scd2brr$mse, weights = weights)

out_survey <- svymean(~alive, scd2brr)
out_srvyr <- scd2brr_srvyr %>%
  summarize(alive = survey_mean(alive))

test_that("as_survey_rep works when using BRR method of replicate weights",
          expect_equal(c(out_survey[[1]], sqrt(attr(out_survey, "var"))),
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))


# ------------------------------------------------------------------
# Test Fay replicate weights
# ------------------------------------------------------------------
scd2fay <- as.svrepdesign(scdnofpc, type="Fay", fay.rho = 0.3, compress = FALSE)

scd2fay_srvyr <- scd2fay$repweights %>%
  unclass() %>% as.data.frame() %>%
  setNames(paste0("rep", 1:4)) %>%
  dplyr::bind_cols(scd) %>%
  mutate(weights = 1) %>%
  as_survey_rep(repweights = starts_with("rep"),
                    type = "Fay", rho = 0.3,
                    scale = scd2fay$scale, rscales = scd2fay$rscales,
                    mse = scd2fay$mse, weights = weights)

out_survey <- svymean(~alive, scd2fay)
out_srvyr <- scd2fay_srvyr %>%
  summarize(alive = survey_mean(alive))


test_that("as_survey_rep works when using Fay method of replicate weights",
          expect_equal(c(out_survey[[1]], sqrt(attr(out_survey, "var"))),
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))

# ------------------------------------------------------------------
# Test JKn replicate weights
# ------------------------------------------------------------------
scd2jkn <- as.svrepdesign(scdnofpc, type = "JKn", compress = FALSE)

scd2jkn_srvyr <- scd2jkn$repweights %>%
  unclass() %>% as.data.frame() %>%
  setNames(paste0("rep", 1:6)) %>%
  dplyr::bind_cols(scd) %>%
  mutate(weights = 1) %>%
  as_survey_rep(repweights = starts_with("rep"),
                    type = "JKn",
                    scale = scd2jkn$scale, rscales = scd2jkn$rscales,
                    weights = weights)

out_survey <- svymean(~alive, scd2jkn)
out_srvyr <- scd2jkn_srvyr %>%
  summarize(alive = survey_mean(alive))


test_that("as_survey_rep works when using JKn method of replicate weights",
          expect_equal(c(out_survey[[1]], sqrt(attr(out_survey, "var"))),
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))


# ------------------------------------------------------------------
# Test JKn replicate weights with fpc
# ------------------------------------------------------------------
scd2jknf <- as.svrepdesign(scddes, type = "JKn", compress = FALSE)

scd2jknf_srvyr <- scd2jknf$repweights %>%
  unclass() %>% as.data.frame() %>%
  setNames(paste0("rep", 1:6)) %>%
  dplyr::bind_cols(scd) %>%
  mutate(weights = 1) %>%
  as_survey_rep(repweights = starts_with("rep"),
                    type = "JKn",
                    scale = scd2jknf$scale, rscales = scd2jknf$rscales,
                    weights = weights)

out_survey <- svymean(~alive, scd2jknf)
out_srvyr <- scd2jknf_srvyr %>%
  summarize(alive = survey_mean(alive))


test_that("as_survey_rep works when using JKn method of replicate weights",
          expect_equal(c(out_survey[[1]], sqrt(attr(out_survey, "var"))),
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))


# ------------------------------------------------------------------
# Test BRR with user-supplied hadamard matrix
# ------------------------------------------------------------------
scd2brr1 <- as.svrepdesign(scdnofpc, type="BRR", hadamard.matrix = paley(11),
                           compress = FALSE)

scd2brr1_srvyr <- scd2brr1$repweights %>%
  unclass() %>% as.data.frame() %>%
  setNames(paste0("rep", 1:12)) %>%
  dplyr::bind_cols(scd) %>%
  mutate(weights = 1) %>%
  as_survey_rep(repweights = starts_with("rep"),
                    type = "JKn",
                    scale = scd2brr1$scale, rscales = scd2brr1$rscales,
                    weights = weights)

out_survey <- svymean(~alive, scd2brr1)
out_srvyr <- scd2brr1_srvyr %>%
  summarize(alive = survey_mean(alive))


test_that("as_survey_rep works when using JKn method of replicate weights",
          expect_equal(c(out_survey[[1]], sqrt(attr(out_survey, "var"))),
                       c(out_srvyr[[1]][[1]], out_srvyr[[2]][[1]])))
