library(tidyverse)
library(kableExtra)
mtcars %>%
  rownames_to_column("car") %>%
  arrange(cyl) %>%
  filter(cyl < 4) %>%
  kbl() %>%
  collapse_rows(columns = 3)
