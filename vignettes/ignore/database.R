library(dplyr)
library(survey)
library(srvyr)
library(RSQLite)

data(api)
my_db <- src_sqlite("my_db.sqlite3", create = T)
api_sqlite <- tbl(my_db, sql("SELECT * FROM apistrat"))


# Works
dstrata <- api_sqlite %>%
  design_survey(strata = stype, weights = pw)

# Works
dstrata <- dstrata %>%
  mutate(apidiff = api00 - api99)

# Doesn't work
dstrata %>%
  summarize(x = survey_mean(api99))

# Doesn't work
dstrata %>% select(api99)
