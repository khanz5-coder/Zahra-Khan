#####setup#############
libs <- c("tidyverse",
          "rvest",
          "polite",
          "httr",
          "stringr",
          "lubridate",
          "xml2",
          "dplyr",
          "tokenizers")
install.packages(setdiff(libs, rownames(installed.packages())))
lapply(libs, library, character.only = TRUE)

getwd()
setwd("/Users/zahrita/Documents/DissData/corpusp1/")

df <- read_csv("/Users/zahrita/Documents/DissData/Data/Raw/ol_texts.csv")

df <- df %>%
  group_by(date) %>%
  filter(n() <= 8) %>%
  ungroup()

set.seed(129)
df_s <-df %>%
  slice_sample(n = 200)

df_s$date <- as.Date(df_s$date, format = "%d %B %Y")
df_s <- df_s[order(df_s$date), ]
head(df_s)

df_s <- df_s %>%
  mutate(sentences = map(text, ~ tokenize_sentences(html_text(read_html(.x)))[[1]])) %>%
  unnest(sentences)  
df_s <- df_s %>%
  mutate(sentences = tolower(sentences))

df_s <- df_s %>%
  mutate(count = str_count(sentences, "\\b\\w+\\b")) %>%
  filter(count >= 15) %>%
  filter(!str_detect(sentences, "^[^a-s]*$")) %>%
  filter(str_count(sentences, "\\d")/ nchar(sentences) < 0.6)

df_s <- df_s %>%
  mutate(id = sprintf("s%02d", row_number()))

keywords <- c("japanese", "chinese", "asiatic", "mongolian", "exclude", "bracero",
              "exlucsion", "native", "deport", "coolie", "deportation", "criminals",
              "filipino", "unamerican", "white labor", "japs", "invasion", "undesirable",
              "illegal", "illegals", "alien", "American labor")
df_codes <- df_s %>%
  mutate(
    likely_pos = if_else(str_detect(sentences, str_c(keywords, collapse = "|")), TRUE, FALSE)
  )

set.seed(129)

sen_total <- 150
n_pos <- 200
n_else <- sen_total - n_pos

positive <- df_codes %>%
  filter(likely_pos) %>%
  sample_n(size = n_pos)

neg <- df_codes %>%
  filter(!likely_pos) %>%
  sample_n(size = n_else)


coding_final <- bind_rows(positive, neg) %>%
  arrange(id) %>%
  mutate(sample_type = if_else(likely_pos, "keyword", "random"))

head(coding_final)

coding_final <- coding_final %>%
  select(-page,)
coding_final <- coding_final %>%
  select(-text)

positive <- positive %>%
  select(-page,)
positive <- positive %>%
  select(-text)

set.seed(126)
HCdfs <-df_s %>%
  slice_sample(n = 700)

HCdfs <- HCdfs %>%
  select(-page,)
HCdfs <- HCdfs %>%
  select(-text)

HCdfs$date <- as.Date(HCdfs$date, format = "%d %B %Y")
HCdfs <- HCdfs[order(HCdfs$date), ]
head(HCdfs)

write.csv(coding_final, file = "HandcodOLT.csv", row.names = FALSE)
write.csv(positive, file = "QTAhand.csv", row.names = FALSE)


df_s <- df_s %>%
  mutate(
    article = str_split(
      text,
      '</p><p dir=\"auto\">' 
    )
  ) %>%
  unnest(article)

write.csv(df_s, file = "HandcodedOLText.csv", row.names = FALSE)


set.seed(127)
df_s2 <-df %>%
  slice_sample(n = 200)
#formatting and organzing by date
df_s2$date <- as.Date(df_s2$date, format = "%d %B %Y")
df_s2 <- df_s2[order(df_s2$date), ]
head(df_s2)

#preprocessing and cleaning the smaller set a bit
df_s2 <- df_s2 %>%
  mutate(sentences = map(text, ~ tokenize_sentences(html_text(read_html(.x)))[[1]])) %>%
  unnest(sentences)  
df_s2 <- df_s2 %>%
  mutate(sentences = tolower(sentences))


df_s2 <- df_s2 %>%
  mutate(count = str_count(sentences, "\\b\\w+\\b")) %>%
  filter(count >= 15) %>%
  filter(!str_detect(sentences, "^[^a-s]*$")) %>%
  filter(str_count(sentences, "\\d")/ nchar(sentences) < 0.6)

#adding a row id for the sentences
df_s2 <- df_s2 %>%
  mutate(id = sprintf("s%02d", row_number()))
#setting up keyword to get balanced code in the handocded set

keywords <- c("japanese", "chinese", "asiatic", "mongolian", "exclude", "bracero",
              "exlucsion", "native", "deport", "coolie", "deportation", "criminals",
              "filipino", "unamerican", "white labor", "japs", "invasion", "undesirable",
              "illegal", "illegals", "alien", "wetback", "bracero", "mexican", "white native",
              "braceros", "border", "operation wetback", "mccarran-walter act",
              "guatemalan", "america first", "national purity", "native jobs")
df_codes2 <- df_s2 %>%
  mutate(
    likely_pos = if_else(str_detect(sentences, str_c(keywords, collapse = "|")), TRUE, FALSE)
  )

set.seed(127)

sen_total <- 600
n_pos <- 200
n_else <- sen_total - n_pos

positive <- df_codes2 %>%
  filter(likely_pos) %>%
  sample_n(size = n_pos)

neg <- df_codes2 %>%
  filter(!likely_pos) %>%
  sample_n(size = n_else)


coding_final2 <- bind_rows(positive, neg) %>%
  arrange(id) %>%
  mutate(sample_type = if_else(likely_pos, "keyword", "random"))

head(coding_final2)

coding_final2 <- coding_final2 %>%
  select(-page,)
coding_final2 <- coding_final2 %>%
  select(-text)

set.seed(128)
HCdfs2 <-df_s2 %>%
  slice_sample(n = 450)

HCdfs2 <- HCdfs2 %>%
  select(-page,)
HCdfs2 <- HCdfs2 %>%
  select(-text)

HCdfs2$date <- as.Date(HCdfs2$date, format = "%d %B %Y")
HCdfs2 <- HCdfs2[order(HCdfs2$date), ]
head(HCdfs2)

write.csv(coding_final2, file = "HandcodOLT2Original1.csv", row.names = FALSE)


set.seed(129)
df_s3 <-df %>%
  slice_sample(n = 200)
#formatting and organzing by date
df_s3$date <- as.Date(df_s3$date, format = "%d %B %Y")
df_s3 <- df_s3[order(df_s3$date), ]
head(df_s3)

#preprocessing and cleaning the smaller set a bit
df_s3 <- df_s3 %>%
  mutate(sentences = map(text, ~ tokenize_sentences(html_text(read_html(.x)))[[1]])) %>%
  unnest(sentences)  
df_s3 <- df_s3 %>%
  mutate(sentences = tolower(sentences))


df_s3 <- df_s3 %>%
  mutate(count = str_count(sentences, "\\b\\w+\\b")) %>%
  filter(count >= 15) %>%
  filter(!str_detect(sentences, "^[^a-s]*$")) %>%
  filter(str_count(sentences, "\\d")/ nchar(sentences) < 0.6)

#adding a row id for the sentences
df_s3 <- df_s3 %>%
  mutate(id = sprintf("s%02d", row_number()))
#setting up keyword to get balanced code in the handocded set

keywords <- c("bracero", "white natives", "deportation", "deport them all", "deportations", "criminals",
              "unamerican", "white labor", "invasion", "undesirable",
              "illegal", "illegals", "alien", "wetback", "braceros", "mexicans", "white native",
              "bracero", "border", "operation wetback", "mccarran-walter act",
              "guatemalan", "america first", "national purity", "native jobs")

df_s3 <- df_s3 %>%
  mutate(
    article = str_split(
      text,
      '</p><p dir=\"auto\">' 
    )
  ) %>%
  unnest(article)

df_codes <- df_s3 %>%
  mutate(
    likely_pos = if_else(str_detect(sentences, str_c(keywords, collapse = "|")), TRUE, FALSE)
  )

set.seed(120)

sen_total <- 200
n_pos <- 120
n_else <- sen_total - n_pos

positive <- df_codes %>%
  filter(likely_pos) %>%
  sample_n(size = n_pos, replace = TRUE)

neg <- df_codes %>%
  filter(!likely_pos) %>%
  sample_n(size = n_else)


coding_final3 <- bind_rows(positive, neg) %>%
  arrange(id) %>%
  mutate(sample_type = if_else(likely_pos, "keyword", "random"))

head(coding_final3)

coding_final3 <- coding_final3 %>%
  select(-page,)
coding_final3 <- coding_final3 %>%
  select(-text)

set.seed(120)
HCdfs3 <-df_s3 %>%
  slice_sample(n = 200)

HCdfs3 <- HCdfs3 %>%
  select(-page,)
HCdfs3 <- HCdfs3 %>%
  select(-text)

HCdfs3$date <- as.Date(HCdfs3$date, format = "%d %B %Y")
HCdfs3 <- HCdfs3[order(HCdfs3$date), ]
head(HCdfs3)

write.csv(coding_final3, file = "HandcodOLT3CA1.csv", row.names = FALSE)


