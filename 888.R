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
  if (length(lookup) == 0) return(NULL)
  stringsimmatrix(x, lookup, method = "lcs") %>%
    apply(1, which.max) -> perm
  # as.character(length(perm))
  lookup[perm]
}

make_like <- function(score, x2, x1, x0) {
  tibble(score = score, x2, x1, x0) %>%
    mutate(score = ifelse(score %in% c("-", "Lineup"), "0 - 0", score)) %>%
    separate_wider_delim(score, names = c("s1", "s2"), delim = "-",
                         too_few = "align_start") %>%
    mutate(across(starts_with("s"), as.numeric)) %>%
    mutate(ans = case_when(
      s1 > s2   ~ x2,
      s1 == s2  ~ x1,
      s1 < s2   ~ x0,
      .default = -1
    )) %>%
    pull(ans)
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

# what <- c(1:6, 8:11, 13:15)
# file_list[-what]
# file_list
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


if (nrow(predictions2) == 0) {
  latest_scores %>%
    mutate(mtch = NA) -> d1a
} else {
  latest_scores %>%
    rowwise() %>%
    mutate(bm = best_match(key, predictions2$key)) %>%
    left_join(predictions2, join_by(bm == key)) %>%
    ungroup() %>%
    mutate(score0 = str_replace(score, " - ", "-")) %>%
    # unnest(ppd, names_sep = "_") %>%
    mutate(fname_no_year = str_remove(fname, "_20[0-9].(-20..)?")) %>%
    mutate(mtch = str_detect(fname_no_year, info)) -> d1
}

# if (!("ppd_score" %in% names(d1))) stop("no games")

# d1

# doesn't match because of the part after the year, so I should extract
# the year and match on that


# matched games

if (exists("d1a")) {
  cat("\nd1a:\n")

  d1a %>%
    select(-key) %>%
    knitr::kable() %>% print()
} else {
  cat("\nMatched games:\n")

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
    knitr::kable() %>% print()

  # leagues I want, but games not matched

  cat("\nMatched leagues (but not games):\n")

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
    knitr::kable() %>% print()


  # non-matched leagues


  cat("\nNon-matched leagues:\n")

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
    knitr::kable() %>% print()

}



cat("\n")
print(glue::glue("Last get: {last_get} (saved)"))

write_rds(last_get, "last_get.rds")

