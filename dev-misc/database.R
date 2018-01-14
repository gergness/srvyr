library(dplyr)
library(survey)
library(srvyr)
library(RSQLite)



data(api)
my_db <- src_sqlite("vignettes/ignore/my_db.sqlite3", create = T)

# Designed surveys ---------
# api_sqlite <- copy_to(my_db, apistrat, temporary = FALSE)

# Works with designed survey
api_sqlite <- tbl(my_db, sql("SELECT * FROM apistrat"))
mysvy <- api_sqlite %>%
  as_survey_design(strata = stype, weights = pw)


# Works
mysvy <- mysvy %>%
  mutate(apidiff = api00 - api99)

# Works
mysvy %>%
  summarize(x = survey_mean(api99),
            y = survey_median(api99),
            z = survey_ratio(api99, api00))


# Also Works
mysvy %>%
  summarize(x = survey_mean(apidiff))

# Also Works
mysvy %>%
  mutate(x = api99 + 10) %>%
  summarize(x = survey_mean(x))

# Works
mysvy %>% select(api99)

# Works
mysvy %>% rename(test = api99)

# Doesn't work
mysvy %>%
  group_by(stype) %>%
  summarize(x = survey_mean(api99),
            y = survey_median(api99),
            z = survey_ratio(api99, api00))


# Replicate Survey --------
# data(scd, package = "survey")
#
# scd <- scd %>%
#  mutate(rep1 = 2 * c(1, 0, 1, 0, 1, 0),
#         rep2 = 2 * c(1, 0, 0, 1, 0, 1),
#         rep3 = 2 * c(0, 1, 1, 0, 0, 1),
#         rep4 = 2 * c(0, 1, 0, 1, 1, 0))
#
# scd_sqlite <- copy_to(my_db, scd, temporary = FALSE)

# Works
scd_sqlite <- tbl(my_db, sql("SELECT * FROM scd"))
mysvy <- scd_sqlite %>%
  as_survey_rep(type = "BRR", repweights = starts_with("rep"),
                combined_weights = FALSE)

# Works
mysvy <- mysvy %>%
  mutate(diff = arrests - alive)

# Works
mysvy %>%
  summarize(x = survey_mean(arrests),
            y = survey_median(arrests),
            z = survey_ratio(arrests, alive))


# Also Works
mysvy %>%
  summarize(x = survey_mean(diff))

# Also Works
mysvy %>%
  mutate(x = arrests + 10) %>%
  summarize(x = survey_mean(x))

# Works
mysvy %>% select(arrests)

# Works
mysvy %>% rename(test = arrests)


# Doesn't work
mysvy %>%
  group_by(stype) %>%
  summarize(x = survey_mean(arrests),
            # y = survey_median(arrests),
            z = survey_ratio(arrests, alive))


# Twophase Survey --------
# data(mu284, package = "survey")
#
# mu284_1 <- mu284 %>%
#   dplyr::slice(c(1:15, rep(1:5, n2[1:5] - 3))) %>%
#   mutate(id = row_number(),
#          sub = rep(c(TRUE, FALSE), c(15, 34-15)))
#
# mu284_1_sqlite <- copy_to(my_db, mu284_1, temporary = FALSE)

# Doesn't Work - twophase expects to be able to subset with data[subset, ]
# data.frame. Twophase in original survey package doesn't support SQL
# so this isn't a huge problem.
mu284_1_sqlite <- tbl(my_db, sql("SELECT * FROM mu284_1"))
mysvy <- mu284_1_sqlite %>%
  as_survey_twophase(id = list(id1, id), strata = list(NULL, id1),
                     fpc = list(n1, NULL), subset = sub)

# Doesn't works
mysvy <- mysvy %>%
  mutate(diff = arrests - alive)

# Doesn't work
mysvy %>%
  summarize(x = survey_mean(arrests),
            y = survey_median(arrests),
            z = survey_ratio(arrests, alive))


# Doesn't work
mysvy %>%
  summarize(x = survey_mean(diff))

# Doesn't work
mysvy %>%
  mutate(x = arrests + 10) %>%
  summarize(x = survey_mean(x))

# Doesn't work
mysvy %>% select(arrests)

# Doesn't work
mysvy %>%
  group_by(stype) %>%
  summarize(x = survey_mean(arrests),
            # y = survey_median(arrests),
            z = survey_ratio(arrests, alive))


