####Full Corpus Cleaning######
###setup#####
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

df <- read_csv("/Users/zahrita/Documents/DissData/Data/ol_texts.csv")

file.rename("/Users/zahrita/Documents/DissData/Data/ol_texts.csv", "/Users/zahrita/Documents/DissData/Data/Raw/ol_texts.csv")

head(df)
#Removing Repeats 
df <- df %>%
  group_by(date) %>%
  filter(n() <= 8) %>%
  ungroup()

#Ordering by date
df$date <- as.Date(df$date, format = "%d %B %Y")
df <- df[order(df$date), ]
head(df)

extract_articles <- function(page_text){
boundary_pattern <- '<p dir=\\"auto\\">([A-Za-z][^<]{0,200})</p>'

loc <- str_locate_all(page_text, boundary_pattern)[[1]]
if (nrow(loc) == 0) return(character(0))
    
start <- loc[, "start"]
ends <- c(start[-1] -1, nchar(page_text))

articles <- mapply(
  function(s, e) substr(page_text, s, e),
  start, ends,
  USE.NAMES = FALSE
)
 return(articles)
}

ol_articles <- df %>%
  rowwise() %>%
  mutate(articles = list(extract_articles(text))) %>%
  unnest(cols = c(articles))

write.csv(ol_articles, file = "article_olv2.csv", row.names = FALSE)

file.rename("/Users/zahrita/Documents/DissData/Code/article_olv2.csv", "/Users/zahrita/Documents/DissData/Data/Processed/article_olv2.csv")

dfa <- read_csv( "/Users/zahrita/Documents/DissData/Data/Processed/article_olv2.csv")

dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove_all(fixed("the union is you \\. take an active part in it")) %>%
      str_remove_all(fixed("^whole\\s+no\\.?\\s*\\d{3,5}$")) %>%
      str_remove_all(fixed("(?i)vol\\.?\\s*[\\di]+\\.?\\s*[—–-]?\\s*no\\.?\\s*[\\di]+\\.?")) %>%
      str_remove_all(fixed("(?i)PUBLISHED SINCE JANUARY 1900")) %>%
      str_remove_all(fixed("(?i)vol\\.?\\s*no\\.?\\s*[\\di]+\\.?")) %>%
      str_remove_all(fixed("(?i)ORGANIZED LABOR")) %>%
      str_remove_all(fixed("(?i)$1.50 PER YEAR.")) %>%
      str_remove_all(fixed("\\d+</p>")) %>%
      str_remove_all(fixed("(?i)[—-]?\\s*(monday|tuesday|wednesday|thursday|friday|saturday|sunday),\\s+(january|february|march|april|may|june|july|august|september|october|november|december)\\s+\\d{1,2}")) %>%
      str_remove_all(fixed('<p dir="auto">.')) %>%
      str_remove_all(fixed("(?i)\\$\\s*\\d+(?:\\.\\d{1,2})?\\s*per\\s+year\\[.,]?")) %>%
      str_remove_all(fixed("(?i)san\\s+francisco[.,]\\s+california,*,*\\s*\\d{4}\\.?")) %>%
      str_remove_all(fixed("^\\s*,?\\s*\\d+\\.?\\s*$")) %>%
      str_squish()
  ) %>%
  filter(articles != "", !is.na(articles))

# df_articles <- df_articles[, -which(names(df_articles) == 'text')]
dfa <- dfa[, -which(names(dfa) == 'page')]

dfa <- dfa %>%
  mutate(id = sprintf("a%02d", row_number()))

dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )

dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove("<p dir= auto >") %>%
      str_remove(" < p><p dir= auto >") %>%
      str_remove("vol 1 no 1 < p>") %>%
      str_remove("< p>")
  )
dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove("(?i)[—-]?\\s*(monday|tuesday|wednesday|thursday|friday|saturday|sunday),\\s+(january|february|march|april|may|june|july|august|september|october|november|december)\\s+\\d{1,2}") %>%
      str_remove(" < p><p dir= auto >") %>%
      str_remove("vol 1 no 1 < p>") %>%
      str_remove("< p >") %>%
      str_remove_all("(?i)\\b(?:san\\s+francisco\\s+)?california\\s+(monday|tuesday|wednesday|thursday|friday|saturday|sunday)\\s+(january|february|march|april|may|june|july|august|september|october|november|december)\\s+\\d{1,2}\\s+\\d{4}")
  )

dfa <- dfa %>%
  filter(nchar(articles) >= 50)

dfa <- dfa[!apply(is.na(dfa) | trimws(dfa)== "", 1, all), ]

dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove_all("organized labor") %>%
      str_remove_all("(?i)[—-]?\\s*(monday|tuesday|wednesday|thursday|friday|saturday|sunday),\\s+(january|february|march|april|may|june|july|august|september|october|november|december)\\s+\\d{1,2}")
  )


write.csv(dfa, file = "article_olv3.csv", row.names = FALSE)

file.rename("/Users/zahrita/Documents/DissData/Code/article_olv3.csv", "/Users/zahrita/Documents/DissData/Data/Processed/article_olv3.csv")

dfa <- read_csv( "/Users/zahrita/Documents/DissData/Data/Processed/article_olv3.csv")

dfa <- na.omit(dfa)
dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove("organized labor") %>%
      str_remove("^saturday february 3 1900$") %>%
      str_remove("(?i)vol\\.?\\s*no\\.?\\s*[\\di]+\\.?") %>%
    str_squish()
  ) %>%
  filter(articles != "", !is.na(articles))

dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove("vol\\s*\\d+\\s*no\\s*\\d+") %>%
      str_remove("per\\s+year\\s+\\d+\\s+\\d+") %>%
      str_remove("(?i)^.{0,30}(jan(uary)?|febr?(uary)?|mar(ch)?|apr(il)?|may|jun(e)?|jul(y)?|aug(ust)?|sep(t(ember)?)?|oct(ober)?|nov(ember)?|dec(ember)?)\\s*\\d{1,2}\\s+\\d{4,5}")
  ) %>%
  filter(articles != "", !is.na(articles))

dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove("published weekly by the publishing company incorporated") %>%
      str_remove_all("[■▪□▫●◆◇▲△▼▽|]") %>%
      str_remove_all("\\b[a-z](?:\\s+[a-z0-9]){1,6}\\b") %>%
      str_remove("(?i)p[e3r]{2,3}\\s*y[e3a]{2,3}r?\\s*[\\$Ss]?\\s*\\d+\\s+\\d+") %>%
      str_squish() 
  )%>%
  filter(articles != "", !is.na(articles))

dfa <- dfa %>%
  mutate(
    articles = articles %>%
      str_remove("per year si so") %>%
      str_squish()
  ) %>%
  filter(articles != "", !is.na(articles))

write.csv(dfa, file = "article_olv4.csv", row.names = FALSE)


file.rename("/Users/zahrita/Documents/DissData/Code/Full Corp Clean.R", "/Users/zahrita/Documents/DissData/Code/Process/Full Corp Clean.R")

dfa <- read_csv( "/Users/zahrita/Documents/DissData/Code/article_olv4.csv")

file.rename("/Users/zahrita/Documents/DissData/Code/article_olv4.csv", "/Users/zahrita/Documents/DissData/data/Processed/article_olv4.csv")

dfa <- dfa %>%
  filter(!(nchar(articles) <= 200 & grepl("continued|cont\\.|from page|from first page|from last page", articles, ignore.case = TRUE)))

sample <- dfa %>%
  filter(nchar(articles) <= 200) %>%
  select(id, articles) %>%
  sample_n(100)

write.csv(dfa, file = "article_olv5.csv", row.names = FALSE)
#dropping everything under 200 characters:

dfa <- dfa %>%
  filter(nchar(articles) > 200)


write.csv(dfa, file = "article_olv6.csv", row.names = FALSE)

dfa <- dfa %>%
  mutate(articles = gsub("<[^>]+>", "", articles))

write.csv(dfa, file = "article_olv.csv", row.names = FALSE)
dfa <- dfa[, -which(names(dfa) == 'text')]
dfa <- dfa[, -which(names(dfa) == 'page')]

write.csv(dfa, file = "article_olv7.csv", row.names = FALSE)
