libs <- c("tidyverse",
          "rvest",
          "polite",
          "httr",
          "stringr",
          "lubridate",
          "xml2",
          "dplyr",
          "readr")
install.packages(setdiff(libs, rownames(installed.packages())))
lapply(libs, library, character.only = TRUE)


setwd("/Users/zahrita/Documents/DissData/corpusp1/")

part1 <- read_csv("ol_text_p1.csv")
head(part1)

titles <- str_extract(part1$text,"(?<=\\.</p><p dir=\\\"auto\\\">).*?(?=</p><p dir=\\\"auto\\\">)")
part1$title <- titles

articles <- part1 %>%
  mutate(
    article = str_split(
      text,
      '</p><p dir=\"auto\">' 
    )
  ) %>%
  unnest(article) %>%
  select(-text)

articles <- articles %>%
  mutate(
    article = str_remove_all(
      articles$article,
      "(?i)VOL\\.\\s*\\d+\\s*[—–-]\\s*NO\\.\\s*\\d+"
      )
  ) %>%
  filter(str_trim(article) != "")


articles <- articles %>%
  mutate(
    article = str_remove_all(
      articles$article,
      "<p dir=\"auto\">R"
    )
  ) %>%
  filter(str_trim(article) != "")

articles <- articles %>%
  mutate(
    article = str_remove_all(
      articles$article,
      "PUBLISHED SINCE JANURARY 1900"
    )
  ) %>%
  filter(str_trim(article) != "")

articles <- articles %>%
  mutate(
    article = str_remove_all(
      articles$article,
      "JANUAkY 7900"
    )
  ) %>%
  filter(str_trim(article) != "")

articles <- articles %>%
  filter(article != "ORGANIZED LABOR")

articles <- articles %>%
  mutate(
    article = str_remove_all(
      articles$article,
      "^WHOLE\\s+NO\\.?\\s*\\d{3,5}$"
    )
  ) %>%
  filter(str_trim(article) != "")

articles <- articles %>%
  mutate(
    article = str_remove_all(
      articles$article,
      '<p dir="auto">'
    )
  ) %>%
  filter(str_trim(article) != "")
articles <- articles %>%
  mutate(
    article = str_remove_all(
      articles$article,
      '<p dir= auto >IÅ omen ö jf^aae'
    )
  ) %>%
  filter(str_trim(article) != "")

articles <- articles %>%
  article = artices$articcle
  mutate(
    str_remove_all("The Union is You . Take an Active Part in It!"),
    str_remove_all("^WHOLE\\s+NO\\.?\\s*\\d{3,5}$"),
    str_remove_all("(?i)VOL\\.\\s*\\d+\\s*[—–-]\\s*NO\\.\\s*\\d+"),
    str_remove_all("PUBLISHED SINCE JANUARY 1900"),
    str_remove_all("(?i)VOL\\.\\s*\\d+\\s*[—–-]\\s*NO\\.\\s*\\d+"),
    
  ) %>%
  filter(article != "ORGANIZED LABOR") %>%
  filter(str_trim(article) != "")
  
articles <- articles %>%
filter(
  str_replace_all(article, '[[:punct:] ]+', ' ')
  ) 


