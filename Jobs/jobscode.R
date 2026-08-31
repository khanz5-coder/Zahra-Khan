getwd()

setwd('/Users/zahrita/Documents/Zahra-Khan')
dir.create("/Users/zahrita/Documents/Zahra-Khan/Jobs")
setwd("/Users/zahrita/Documents/Zahra-Khan/Jobs")

libraries <- c("tidyverse",
               "rvest",
               "polite",
               "httr",
               "stringr",
               "lubridate",
               "xml2",
               "dplyr",
               "ggplot2",
               "scales",
               "gridExtra",
               "ggthemes",
               "TSstudio",
               "xts",
               "tseries",
               "forecast",
               "stargazer",
               "TTR",
               "plotly",
               "zoo",
               "readxl",
               'jsonlite',
               'topicmodels',
               'tidytext',
               'tm',
               'datefixR',
               'rsample',
               'pROC',
               'chromote',
               'purrr',
              'httr2' )
install.packages(setdiff(libraries, rownames(installed.packages())))
lapply(libraries, library, character.only = TRUE)

url <- "https://stripe.com/careers/search"

page <- read_html(url) #extract

hyperlinks <- html_nodes(
  page,
  xpath = "//a[contains(@class, 'hds-link careers-role-result__title')]/@href"
) %>% as.character()
print(hyperlinks[1])

base <- "https://stripe.com/careers/search"

# Getting the first page
first_page <- read_html(base)

# Extracting pagination numbers
max_page <- first_page %>%
  html_nodes(".hds-button.careers-pagination__page-count") %>% #criteria
  html_text() %>% 
  str_extract("\\d+") %>%
  as.numeric() %>% 
  max(na.rm = TRUE)

max_page

#go onto pagination, look for d+ tag,
# Constructing the URLs for all the pages
urls <- c(
  base,
  paste0(base, "?page=", 2:max_page)
)

# Getting links to each press release from each page
extract_links <- function(page_url) {
  read_html(page_url) %>%
    html_nodes("a.hds-link.careers-role-result__title") %>%
    html_attr("href")
}

all_links <- urls %>% map(extract_links) %>% unlist()

length(all_links)
head(all_links)

all_links_f <- sub("^/careers/", "https://stripe.com/careers/", all_links)
head(all_links_f)

#extract all the urols from the broader pages around it.
# Extracting html from all links 

pages <- vector("list", length(all_links))

for (i in seq_along(all_links_f)) {
  message("Reading: ", all_links_f[i])
  pages[[i]] <- read_html(all_links_f[i])
}

titles <- vector("character", length(pages))

for (i in seq_along(pages)) { #do this across all the html across al the pages 
  
  titles[i] <- pages[[i]] %>%
    html_node("h1") %>%         # find the H1 title
    html_text(trim = TRUE)      # extract clean text
}

titles[1] # Seems right 

company <- vector("character", length(pages))

for (i in seq_along(pages)) {
  
  company[i] <- pages[[i]] %>%
    html_nodes("dd") %>%
    html_text(trim = TRUE)
}

company[1:100]

#### Combining into a table ####

df <- data.frame(
  title = titles,
  company = company,
  url = all_links_f,
  stringsAsFactors = FALSE
)

bodies <- vector("character", length = nrow(df))

for (i in seq_len(nrow(df))) {
  
  this_url <- df$url[i]
  message("Scraping: ", this_url)
  
  # load article page
  page <- read_html(this_url)
  
  # extract all strong + p tags as text
  nodes <- page %>%
    html_nodes("li, h3, h2, p, ul, strong") %>%
    html_text(trim = TRUE)
  start_patt <- c("Minimum requirements", "What you’ll do")
  end_patt <- c("In-office expectations", "Working remotely at Stripe")
  
  start_idx <- which(str_detect(nodes, paste(start_patt, collapse = "|")))[1]
  end_idx <- which(str_detect(nodes,  paste(end_patt, collapse = "|")))[1]
  
  # fallback if either is missing
  if (is.na(start_idx) || is.na(end_idx) || end_idx <= start_idx) {
    bodies[i] <- NA_character_
    next
  }

  # content between date line and "Ends."
  body <- nodes[(start_idx + 1):(end_idx - 1)]
  
  # collapse into one text block
  body <- paste(body, collapse = "\n\n")
  
  bodies[i] <- body
}
bodies[1]

df$body <- bodies

type <- print(replicate(1060, "Fintech"))

df$type <- type

write.csv(df, "stripjobs.csv", row.names = FALSE)


glimpse(df)
df$body[1]


########################################################################
fetch_page <- function(offset) {
  req <- request("https://api.lifeattiktok.com/api/v1/public/supplier/search/job/posts") |>
    req_method("POST") |>
    req_headers(`Content-Type` = "application/json") |>
    req_body_json(list(
      keyword = "dublin",
      limit = 12,
      offset = offset
      # add other fields seen in the real payload
    ))
  
  resp <- req_perform(req)
  resp_body_json(resp)
}

offsets <- seq(0, 144, by = 12)
results <- map(offsets, fetch_page, .progress = TRUE)
