#| message: false
library(tidyverse)
library(rvest)
library(stringdist)
cutoff <- read_rds("last_get.rds") - seconds(30)
print(glue::glue("Cutoff: {cutoff}"))


# no <- read_csv("no.csv")
# # no
# no %>% mutate(squawk = "no") -> no

no <- read_csv("no-longer.csv")

html_tab <- function(url, mtime) {
  time_str <- sprintf("%02d:%02d:%02.0f", hour(mtime), minute(mtime), second(mtime))
  # time_str <- glue::glue("{hour(mtime)}:{minute(mtime)}:{round(second(mtime))}")
  my_html <- read_html(url)
  my_html %>% html_table() %>% pluck(1) %>%
    filter(!str_detect(X1, "/")) %>%
    filter(str_detect(X2, ":")) %>%
    rename(league = X1, ko = X2, status = X3, t1 = X4, score = X5, t2 = X6) %>%
    mutate(time = time_str) %>%
    select(time, league:t2)
}

best_match <- function(x, lookup) {
  stringsimmatrix(x, lookup, method = "lcs") %>%
    apply(1, which.max) -> perm
  lookup[perm]
}



enframe(list.files(path = "888", pattern = "888_.*.html", full.names = TRUE)) %>%
  mutate(mtime = file.mtime(value)) %>%
  filter(mtime >= cutoff) %>%
  arrange(mtime) -> d
d %>%
  pull(value) -> file_list
d %>%
  pull(mtime) -> mtimes
d %>% summarize(max = max(mtime)) %>% pull(max) -> last_get

# get predictions

my_url <- "~/Documents/r-projects/world-football/predictions.rds"
predictions <- read_rds(my_url)
predictions %>%
  filter(ko <= now() + hours(3)) %>%
  mutate(key = str_c(t1, " - ", t2)) -> predictions2


nowtime <- now()

map2(file_list, mtimes, \(x, y) html_tab(x, y)) %>%
  bind_rows() %>%
  arrange(league, t1) %>%
  group_by(league, t1) %>%
  mutate(next_score = lead(score),
         last_score = lag(score),
         next_status = lead(status),
         last_status = lag(status)) %>%
  filter( score != next_score |
            score != last_score |
            ((status == "FT") & (last_status != "FT"))) %>%
  select(league, time, status, t1, score, t2) %>%
  left_join(no, join_by(league)) %>%
  filter(is.na(info) | info != "no") %>%
  arrange(info) %>%
  mutate(key = str_c(t1, " - ", t2)) -> latest_scores


latest_scores %>%
  rowwise() %>%
  mutate(bm = best_match(key, predictions2$key)) %>%
  left_join(predictions2, join_by(bm == key)) %>%
  ungroup() %>%
  select(fname,
         ko = ko,
         time = time,
         t1 = t1.y,
         r1,
         r2,
         t2 = t2.y,
         status,
         score,
         `2`, `1`, `0`, s1, s2,
         league) %>%
  left_join(no, join_by(league)) %>%
  mutate(mtch = str_detect(fname, info)) %>%
  select(-info) %>%
# select(-squawk) %>%
  knitr::kable()

cat("\n")
print(glue::glue("Last get: {last_get} (saved)"))

write_rds(last_get, "last_get.rds")

