context('dplyr verbs behave with tbl_svy objects')
library('srvyr')
library('survey')
library('dplyr')

#set up data
data(api)
dstrata <- apistrat %>%
  as_survey_design(strata = stype, weights = pw)

test_that('srvyr::pull works like dplyr::pull',{

  expect_equal(pull(dstrata, 'api00'), pull(apistrat, 'api00'))
  expect_equal(pull(dstrata, api99), pull(apistrat, api99))
  expect_equal(pull(dstrata, 1), pull(apistrat, 1))
  expect_equal(pull(dstrata, -4), pull(apistrat, -4))

})
