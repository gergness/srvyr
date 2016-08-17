library(dplyr)
library(survey)
library(srvyr)
library(RSQLite)

data(api)
my_db <- src_sqlite("my_db.sqlite3", create = T)
api_sqlite <- copy_to(my_db, apistrat, temporary = FALSE)
api_sqlite <- tbl(my_db, sql("SELECT * FROM apistrat"))


# Works
dstrata <- api_sqlite %>%
  design_survey(strata = stype, weights = pw)

# Works
dstrata <- dstrata %>%
  mutate(apidiff = api00 - api99)

# Works
dstrata %>%
  summarize(x = survey_mean(api99),
            y = survey_median(api99),
            z = survey_ratio(api99, api00))

# Doesn't work
dstrata %>% select(api99)

# Also Works
dstrata %>%
  summarize(x = survey_mean(apidiff))

# Also Works
dstrata %>%
  mutate(x = api99 + 10) %>%
  summarize(x = survey_mean(x))
