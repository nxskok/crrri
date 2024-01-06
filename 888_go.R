#| message: false
library(tidyverse)
library(rvest)

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



enframe(list.files(path = "888", pattern = "888_.*.html", full.names = TRUE)) %>%
  mutate(mtime = file.mtime(value)) %>%
  slice_max(mtime, n = 1) -> d
d %>%
  pull(value) -> file_list
d %>%
  pull(mtime) -> mtimes



nowtime <- now()

map2(file_list, mtimes, \(x, y) html_tab(x, y)) %>%
  bind_rows() %>%
  arrange(league, t1) %>%
  group_by(league, t1) %>%
  filter(status != "FT") %>%
  filter(score != "-") %>%
  filter(score != "") %>%
  left_join(no, join_by(league)) %>%
  filter(is.na(info) | info != "no") %>%
  arrange(info) %>%
  # select(-squawk) %>%
  knitr::kable()


