library(tidyverse)
library(kableExtra)
# functions
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


# code
no <- read_csv("no-longer.csv")
d1 <- read_rds("test_d1.rds")
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
  select(-info) %>% kbl() %>%
  collapse_rows(columns = c(4, 7))
