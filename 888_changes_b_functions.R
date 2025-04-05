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

save_html_as_rds <- function(fname) {
  m <- file.mtime(fname)
  savename <- str_c("888/", m, ".rds")
  x <- html_tab(fname, m)
  write_rds(x, savename)
  savename
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
    mutate(mx = pmax(x2, x1, x0)) %>%
    mutate(ans = case_when(
      s1 > s2   ~ log(mx/x2),
      s1 == s2  ~ log(mx/x1),
      s1 < s2   ~ log(mx/x0),
      .default = -1
    )) %>%
    pull(ans) -> ax
  round(ax*100)
}


make_d <- function(cutoff) {
  enframe(list.files(path = "888", pattern = "888_.*.html", full.names = TRUE)) %>%
    mutate(mtime = file.mtime(value)) %>%
    filter(mtime >= cutoff) %>%
    arrange(mtime)
}

make_d_c <- function(cutoff) {
  enframe(list.files(path = "888", pattern = "*.rds", full.names = FALSE)) %>%
    mutate(mtime = ymd_hms(value, tz = "America/Toronto")) %>%
    filter(mtime >= cutoff) %>%
    mutate(value = str_c("888/", value)) %>%
    arrange(mtime)
}

make_current_predictions <- function(predictions, nowtime) {
  predictions %>%
    filter(ko <= nowtime + hours(3)) %>%
    mutate(key = str_c(t1, " - ", t2))
}

how_many_current_scores <- function(file_list, mtimes) {
  map2(file_list, mtimes, \(x, y) html_tab(x, y)) %>%
    bind_rows() %>%
    left_join(no, join_by(league)) %>%
    filter(is.na(info) | info != "no") %>%
    filter(str_detect(score, " - ")) %>%
    filter(status != "FT")
}

make_latest_scores <- function(file_list, mtimes) {
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
    mutate(key = str_c(t1, " - ", t2))
}

make_latest_scores_c <- function(file_list, mtimes) {
  map(file_list, \(x) read_rds(x)) %>%
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
    mutate(key = str_c(t1, " - ", t2))
}


make_d1a <- function(latest_scores) {
  latest_scores %>%
    mutate(mtch = NA)
}


make_d1 <- function(latest_scores, predictions2) {
  latest_scores %>%
    rowwise() %>%
    mutate(bm = best_match(key, predictions2$key)) %>%
    left_join(predictions2, join_by(bm == key)) %>%
    ungroup() %>%
    mutate(score0 = str_replace(score, " - ", "-")) %>%
    # unnest(ppd, names_sep = "_") %>%
    mutate(fname_no_year = str_remove(fname, "_20[0-9].(-20..)?")) %>%
    mutate(mtch = str_detect(fname_no_year, info))
}

display_d1a <- function(d1a) {
  if (nrow(d1a) == 0) return("No rows to display")

  d1a %>%
    select(league, time, status, t1, score, t2) %>%
    kbl() %>%
    collapse_rows(columns = c(4, 6), valign = "top")
}

display_matched_games <- function(d1, no) {

  if (nrow(d1) ==0) return ("No games to report")

  d1 %>%
    filter(mtch) -> d11
  if (nrow(d11) == 0) return("No matched games")
  d11 %>%
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
    select(-info, -mtch) %>%
    select(-league) %>%
    # select(-squawk) %>%
    select(fname:`0`) %>%  # I don't know where the extra columns are coming from
    kbl() %>%
    column_spec(10, bold = TRUE) %>%
  collapse_rows(columns = c(1, 2, 4, 7), valign = "top")

}

display_non_matched_games <- function(d1) {
  if (nrow(d1) ==0) return (NULL)

  d1 %>%
    filter(!mtch) -> d11

  if (nrow(d11) == 0) return("No non-matched games")

  d11 %>%
    select(league,
           time = time,
           t1 = t1.x,
           t2 = t2.x,
           st = status,
           score) %>%
    kbl() %>%
    collapse_rows(columns = c(1, 3, 4), valign = "top")


}


display_non_matched_leagues <- function(d1) {

  if (nrow(d1) ==0) return (NULL)

  d1 %>%
    filter(is.na(mtch)) -> d11

  if (nrow(d11) == 0) return("No games from non-matched leagues")

  d11 %>%
    select(league,
           time = time,
           t1 = t1.x,
           t2 = t2.x,
           st = status,
           score) %>%
    kbl() %>%
    collapse_rows(columns = c(1, 3, 4), valign = "top")


}



