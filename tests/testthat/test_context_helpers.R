context('Able to use survey-context helpers inside dplyr verbs')
library('srvyr')
library('survey')
library('dplyr')

# Load test dataset
data(api)
dstrata <- apistrat %>%
  mutate(is_respondent = row_number() %% 2) %>%
  mutate(nr_adjusted_wt = pw * is_respondent * (sum(pw)/sum(pw * is_respondent))) %>%
  as_survey_design(strata = stype, weights = nr_adjusted_wt)
rstrata <- as_survey_rep(dstrata)

# Access survey weights
  test_that('summarize can access cur_svy_wts',{
    ## Linearization design
    result_df <- dstrata %>%
      summarize(result = sum(cur_svy_wts()),
                expected = unweighted(sum(nr_adjusted_wt)))
    expect_equal(result_df[['result']],
                 result_df[['expected']])

    ## Replicate-weights design
    result_df <- rstrata %>%
      summarize(result = sum(cur_svy_wts()),
                expected = unweighted(sum(nr_adjusted_wt)))
    expect_equal(result_df[['result']],
                 result_df[['expected']])
  })

  test_that('mutate can access cur_svy_wts',{
    ## Linearization design
    result_df <- dstrata %>%
      mutate(wts = cur_svy_wts())
    expect_equal(result_df[['wts']],
                 result_df[['nr_adjusted_wt']])

    ## Replicate-weights design
    result_df <- rstrata %>%
      mutate(wts = cur_svy_wts())
    expect_equal(result_df[['wts']],
                 result_df[['nr_adjusted_wt']])
  })

  test_that('transmute can access cur_svy_wts',{

    ## Linearization design
    result_df <- dstrata %>%
      transmute(wts = cur_svy_wts(),
                wt_var_copy = nr_adjusted_wt)
    expect_equal(unname(result_df[['variables']][['wts']]),
                 result_df[['variables']][['wt_var_copy']])

    ## Replicate-weights design
    result_df <- rstrata %>%
      transmute(wts = cur_svy_wts(),
                wt_var_copy = nr_adjusted_wt)
    expect_equal(unname(result_df[['variables']][['wts']]),
                 result_df[['variables']][['wt_var_copy']])
  })

# Check whether weights are accessed separately by group

  test_that('cur_svy_wts works for groups',{
    ## summarize
    result_df <- dstrata %>%
      group_by(stype) %>%
      summarize(result = sum(cur_svy_wts()),
                expected = unweighted(sum(nr_adjusted_wt)))

    expect_equal(result_df[['result']],
                 result_df[['expected']])

    ## mutate
    result_df <- dstrata %>%
      group_by(stype) %>%
      mutate(result = 2*cur_svy_wts(),
             expected = 2*nr_adjusted_wt) %>%
      ungroup()
    expect_equal(result_df[['result']],
                 result_df[['expected']])

    ## transmute
    result_df <- dstrata %>%
      group_by(stype) %>%
      transmute(result = 2*cur_svy_wts(),
                expected = 2*nr_adjusted_wt) %>%
      ungroup()
    expect_equal(result_df[['result']],
                 result_df[['expected']])
  })


# filter has access to svy context vars
  test_that('Can use filter with svy context vars', {
    using_context_vars <- dstrata %>%
      filter(cur_svy_wts() >  50)

    using_var <- dstrata %>%
      filter(nr_adjusted_wt > 50)

    expect_equal(using_context_vars, using_var)
  })


# Can access cur_svy_wts even in
  test_that('Can nest functions that use context helpers', {
    nested <- dstrata %>%
      mutate(
        cur_svy() %>%
          filter(cur_svy_wts() > 50) %>%
          summarize(
            count_below_50 = sum(cur_svy_wts() <= 50),
            count_total = length(cur_svy_wts())
          )
      )
    expect_equal(nested$variables$count_below_50, rep(0, nrow(dstrata)))
    expect_equal(nested$variables$count_total, rep(53, nrow(dstrata)))
  })
