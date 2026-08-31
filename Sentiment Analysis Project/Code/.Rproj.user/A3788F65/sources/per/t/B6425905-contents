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



df1 <- read_csv("~/Documents/DissData/Output/corpusp1/HandcodOLT3CA1.csv")
df2 <- read_csv("~/Documents/DissData/Output/corpusp1/HandcodOLTUPDATED.csv")

master_code <- rbind(df1, df2)

head(master_code)
parsed <- as.Date(master_code$date, format = "%m/%d/%y")
parsed <- as.Date(format(parsed, "19%y-%m-%d"))
master_code$date <- format(parsed, "%Y %m %d")
head(master_code)
master_code <- master_code[order(master_code$date), ]
head(master_code)

master_code <- master_code %>%
  mutate(code = if_else(code >= 1 & code <=5,1, code))

write.csv(master_code, file = "master_codev1.csv", row.names = FALSE)

file.rename("/Users/zahrita/Documents/DissData/Code/master_codev1.csv", "/Users/zahrita/Documents/DissData/Data/Processed/master_codev1.csv")

dfc <- read_csv("/Users/zahrita/Documents/DissData/Data/Processed/master_codev1.csv")

dfc <- dfc %>%
  distinct(sentences, .keep_all = TRUE)
dfc <- na.omit(dfc)

dfc <- dfc %>%
  mutate(sentences = iconv(sentences, to = "UTF-8", sub = "byte")) %>%
  mutate(sentences = gsub("<[^>]+>", "", sentences))

dfc <- dfc %>%
  mutate(
    sentences = sentences %>%
      str_remove_all("[■▪□▫●◆◇▲△▼▽|]") %>%
      str_remove("(?i)p[e3r]{2,3}\\s*y[e3a]{2,3}r?\\s*[\\$Ss]?\\s*\\d+\\s+\\d+") %>%
      str_squish() 
  )

dfc <- dfc %>%
  mutate(
    sentences = sentences %>%
      str_to_lower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_squish()
  )
dfc <- dfc[, -which(names(dfc) == 'count')]
dfc <- dfc[, -which(names(dfc) == 'likely_pos')]
dfc <- dfc[, -which(names(dfc) == 'sample_type')]

write.csv(dfc, file = "master_codev2.csv", row.names = FALSE)
