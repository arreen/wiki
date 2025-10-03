# library(rvest)
# library(dplyr)
# library(stringr)
# library(jsonlite)

# test <- read_html(x = "https://en.wikipedia.org/wiki/List_of_women_in_statistics")
# test2 <- test %>% html_element("div.mw-content-ltr.mw-parser-output") %>% html_nodes("ul") %>%
#   html_nodes("a") %>% html_attr("href")

# test2 <- str_subset(test2, "^/wiki/")
# test2 <- str_replace(test2, pattern = "/wiki/", replacement = "")

# women_in_statistics <- test2

# saveRDS(women_in_statistics, file = "women_in_statistics.rda")
