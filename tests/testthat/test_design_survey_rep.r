context("design_survey_rep arguments work as expected.")
library(srvyr)

# From survey::as.svrepdesign examples
library(survey)
data(scd)
scddes<-svydesign(data=scd, prob=~1, id=~ambulance, strata=~ESA,
                  nest=TRUE, fpc=rep(5,6))
scdnofpc<-svydesign(data=scd, prob=~1, id=~ambulance, strata=~ESA,
                    nest=TRUE)

# convert to BRR replicate weights
scd2brr <- as.svrepdesign(scdnofpc, type="BRR", compress = FALSE)
scd2fay <- as.svrepdesign(scdnofpc, type="Fay",fay.rho=0.3)
# convert to JKn weights
scd2jkn <- as.svrepdesign(scdnofpc, type="JKn")

# convert to JKn weights with finite population correction
scd2jknf <- as.svrepdesign(scddes, type="JKn")

## with user-supplied hadamard matrix
scd2brr1 <- as.svrepdesign(scdnofpc, type="BRR", hadamard.matrix=paley(11))


scd2brr_srvyr <- scd2brr$repweights %>%
  unclass() %>% as.data.frame() %>%
  setNames(paste0("rep", 1:4)) %>%
  dplyr::bind_cols(scd) %>%
  mutate(weights = 1) %>%
  design_survey_rep(repweights = starts_with("rep"),
                    type = "BRR", rho = scd2brr$rho,
                    scale = scd2brr$scale, rscales = scd2brr$rscales,
                    mse = scd2brr$mse, weights = weights)

test_that("design_survey_rep works when using BRR method of replicate weights",
          expect_equal(svymean(~alive, scd2brr_srvyr),
                       svymean(~alive, scd2brr)))

