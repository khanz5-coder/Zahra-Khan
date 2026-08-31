#####setup#############
libs <- c("tidyverse",
          "rvest",
          "polite",
          "httr",
          "stringr",
          "lubridate",
          "xml2",
          "dplyr")
install.packages(setdiff(libs, rownames(installed.packages())))
lapply(libs, library, character.only = TRUE)

getwd()
setwd(dirname("/Users/zahrita/Documents/DissData/"))

df <- read_csv("HandcodedOLText.csv")


df <- df %>%
  mutate(
    articles = str_split(
      text,
      '.' 
    )
  ) %>%
  unnest(article)


#Removing Repeats 
df <- df %>%
  group_by(date) %>%
  filter(n() <= 8) %>%
  ungroup()

#Subsetting Smaller Group
set.seed(123)
df_s <-df %>%
  slice_sample(n = 200)

df_s$date <- as.Date(df_s$date, format = "%d %B %Y")
df_s <- df_s[order(df_s$date), ]
head(df_s)
df_s <- df_s %>%
  mutate(
    article = str_split(
      text,
      '</p><p dir=\"auto\">' 
    )
  ) %>%
  unnest(article)

df_s <- df_s %>%
  filter(article != "") %>%
  filter(str_detect(article, "\\S")) %>%
  mutate( article = str_replace_all(article,  '[[:punct:] ]+', ' ')) %>%
  str_squish() %>%
  filter(article != "", !is.na(article))

df_s <- df_s %>%
  mutate(article = str_replace_all(article, '[[:punct:] ]+', ' ')) 
df_s <- df_s %>%
  mutate(article = str_replace_all(article, '[^[:alnum:]]+', ' ')) 
df_s <- df_s %>%
  mutate(
    article = article %>%
  str_remove_all( "The Union is You \\. Take an Active Part in It!") %>%
  str_remove_all("^WHOLE\\s+NO\\.?\\s*\\d{3,5}$") %>%
  str_remove_all("(?i)VOL\\.?\\s*[\\di]+\\.?\\s*[—–-]?\\s*NO\\.?\\s*[\\di]+\\.?") %>%
  str_remove_all("PUBLISHED SINCE JANUARY 1900") %>%
  str_remove_all("(?i)VOL\\.?\\s*NO\\.?\\s*[\\di]+\\.?") %>%
  str_remove_all("ORGANIZED LABOR") %>%
  str_remove_all("$1.50 PER YEAR.") %>%
  str_remove_all("\\d+</p>") %>%
  str_remove_all("(?i)[—-]?\\s*(monday|tuesday|wednesday|thursday|friday|saturday|sunday),\\s+(january|february|march|april|may|june|july|august|september|october|november|december)\\s+\\d{1,2}") %>%
  str_remove_all('<p dir="auto">.') %>%
  str_remove_all("(?i)\\$\\s*\\d+(?:\\.\\d{1,2})?\\s*PER\\s+YEAR\\[.,]?") %>%
  str_remove_all("(?i)SAN\\s+FRANCISCO[.,]\\s+CALIFORNIA,*,*\\s*\\d{4}\\.?") %>%
  str_remove_all("(?i)\\(Continued\\s+on\\s+page\\s+\\d+\\.?\\)\\s*</p>?") %>%
  str_remove_all("^\\s*,?\\s*\\d+\\.?\\s*$") %>%
  str_squish()
) %>%
  filter(article != "", !is.na(article))

df_s <- df_s %>%
  mutate(count = str_count(article, "\\b\\w+\\b")) %>%
  filter(count >= 15) %>%
  filter(!str_detect(article, "^[^a-s]*$")) %>%
  filter(str_count(article, "\\d")/ nchar(article) < 0.6)

df_s <- df_s[, -which(names(df_s) == 'text')]
df_s <- df_s[, -which(names(df_s) == 'page')]

df_s <- df_s %>%
  mutate(id = sprintf("a%02d", row_number()))

write.csv(df_s, file = "HandcodedOLText.csv", row.names = FALSE)

