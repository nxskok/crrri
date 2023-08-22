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
    } %...>% {
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
  dump_DOM(url = "https://www.888scoreonline.net/", file = fname)
  print(now())
  Sys.sleep(3 * 60) # this many minutes
}


