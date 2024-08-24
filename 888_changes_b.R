library(tidyverse)
library(rvest)
library(kableExtra) # this makes html output, so all the output needs to be html
library(stringdist)
source("888_changes_b_functions.R")


cutoff <- read_rds("last_get.rds") - seconds(30)
print(glue::glue("<br>Cutoff: {cutoff}</br>"))
no <- read_csv("no-longer.csv")
d <- make_d(cutoff)
d %>% pull(value) -> file_list
d %>% pull(mtime) -> mtimes
d %>% summarize(max = max(mtime)) %>% pull(max) -> last_get
my_url <- "~/Documents/r-projects/world-football/predictions.rds"
nowtime <- now()
predictions <- read_rds(my_url)
predictions2 <- make_current_predictions(predictions, nowtime)
latest_scores <- make_latest_scores(file_list, mtimes)
if (nrow(predictions2) == 0) {
  d1a <- make_d1a(latest_scores)
} else {
  d1 <- make_d1()
}




if (exists("d1a")) {
  cat("<H1>d1a:</H1>")

  d1a %>%
    select(-key) %>%
    kbl()
} else {
  cat("<H1>Matched games:</H1>")

  d1 %>%
    filter(mtch) %>%
    mutate(like = make_like(score, `2`, `1`, `0`)) %>%
    # filter(score0 == ppd_score) %>%
    select(fname,
           ko = ko,
           time = time,
           t1 = t1.y,
           r1,
           r2,
           t2 = t2.y,
           st = status,
           score,
           like,
           league,
           `2`, `1`, `0`,
           mtch) %>%
    left_join(no, join_by(league)) %>%
    mutate(t1 = ifelse(mtch, t1, NA)) %>%
    mutate(t2 = ifelse(mtch, t2, NA)) %>%
    select(-info) %>%
    # select(-squawk) %>%
    kbl() %>%
    collapse_rows(columns = c(4, 7))

  # leagues I want, but games not matched

  cat("<H1>Matched leagues (but not games):</H1>")

  d1 %>%
    # filter(score0 == ppd_score) %>%
    filter(!mtch) %>%
    select(league,
           time = time,
           t1 = t1.x,
           t2 = t2.x,
           st = status,
           score,
           mtch) %>%
           kbl() %>%
           collapse_rows(columns = c(3, 4))


  # non-matched leagues


  cat("<H1>Non-matched leagues:</H1>")

  d1 %>%
    # filter(score0 == ppd_score) %>%
    filter(is.na(mtch)) %>%
    select(league,
           time = time,
           t1 = t1.x,
           t2 = t2.x,
           st = status,
           score,
           mtch) %>%
    kbl() %>%
    collapse_rows(columns = c(3, 4))

}


# cat("\n")
# print(glue::glue("Last get: {last_get} (saved)"))

# write_rds(last_get, "last_get.rds")



