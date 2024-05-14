library(tidyverse)
library(crrri)
brave <- "/usr/bin/brave-browser"
Sys.setenv(HEADLESS_CHROME = brave)

system2("fuser -k 9222/tcp")

dump_DOM <- function(url, file = "") {
  perform_with_chrome(function(client) {
    Network <- client$Network
    Page <- client$Page
    Runtime <- client$Runtime
    Network$enable() %...>% {
      Page$enable()
    } %...>% {
      Network$setCacheDisabled(cacheDisabled = TRUE)
    } %...>% {
      Page$navigate(url = url)
    } %...>% {
      Page$loadEventFired()
    } %>% wait(delay = 1) %...>% {
      Runtime$evaluate(
        expression = 'document.documentElement.outerHTML'
      )
    } %...>% (function(result) {
      html <- result$result$value
      cat(html, "\n", file = file)
    })
  })
}

while (TRUE) {
  fname <- tempfile(pattern = "888_", tmpdir = "./888/", fileext = ".html")
  safe_dump <- safely(dump_DOM)
  ans <- safe_dump(url = "https://www.888scoreonline.net/", file = fname)
  print(now())
  if (!is.null(ans$error)) {
    print(ans$error)
    Sys.sleep(1 * 60) # this many minutes
  } else {
    print("successful")
    Sys.sleep(3 * 60) # this many minutes
  }
}


