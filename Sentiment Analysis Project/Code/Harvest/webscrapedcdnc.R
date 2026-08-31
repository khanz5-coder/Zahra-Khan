### SET
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

setwd("~")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

######################################################
## 2. Generating Saturday Dates
######################################################
today <- as.Date('1949-01-01')

days_since_sat <- (as.POSIXlt(today)$wday - 6) %% 7
most_recent_sat <- today - days_since_sat

cutoff <- today - 49 * 365.25
all_sat <- seq(from = most_recent_sat, to = cutoff, by = "-7 days")
head(all_sat) #finding all saturdays between 1988 and 1900

sat_str <- format(all_sat, "%Y%m%d") #reformatting
sat_str <- as.character(sat_str) #transforming into character vector
head(sat_str)
sat_str[1]
sat_str[2]
sat_str[2553]
sat_str <- sat_str[1:2553]
#########################################
## 3. Building Urls
#########################################

urls <- map(
  sat_str,
  ~paste0(
    "https://cdnc.ucr.edu/?a=d&d=OLSF",
    .x,
    ".1.", 
    1:8,
    "&f=XML&cloudflare=93N102TQ3B"
  )
) |> unlist() 

length(urls)

batch_size <- 100  
## creating smaller batches eve with safe helpers to ensure no data is lost
url_batches <- split(urls, ceiling(seq_along(urls) / batch_size))
length(url_batches)

######################################
## 5. Safe Helpers
######################################
safe_read <- function(url) {
  tryCatch(
    {
      Sys.sleep(runif(1, 1, 2))
      x <- read_xml(url)
      gc()
      x
    },
    error = function(e){
      cat(url, "\n", "failed_links.txt", append = TRUE)
      gc()
      NULL
    }
  )
}

safe_text <- function(xml, xpath) {
  if (is.na(xml)) return (NA)
  node <- xml_find_first(xml, xpath)
  if (length(node) == 0) NA else xml_text(node)
}


################################
## 6. Process One Page
################################

process_page <- function(url){
  xml <- safe_read(url)
  if (is.null(xml)) return(NULL)
  
  tibble(
    date = safe_text(xml, ".//DocumentDate"),
    page = safe_text(xml, ".//DocumentNumber"),
    text = safe_text(xml, ".//PageTextHTML"),
    volume = safe_text(xml, ".//DocumentVolume"),
    number = safe_text(xml, ".//DocumentNumber")
  )
}


output_file <- "ol_texts.csv"
if(!file.exists(output_file)) {
  write_csv(
    tibble(date = character(),
      page = character(),
      text = character(),
      volume = character(),
      number = character()),
    output_file
    )
}



for (b in seq_along(url_batches)) {
  cat("Starting Batch", b, "of", length(url_batches), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batches[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batches[[b]]), "\n")
      process_page(url_batches[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 5, 10))
}

######################################################
## 2. Generating Friday Dates
#####################################################

ftoday <- as.Date('1957-03-29')

days_since_fri <- (as.POSIXlt(ftoday)$wday - 5) %% 7
most_recent_fri <- ftoday - days_since_fri

cutoff <- ftoday - 9 * 365.25
all_fri <- seq(from = most_recent_fri, to = cutoff, by = "-7 days")
head(all_fri) #finding all saturdays between 1988 and 1900

fri_str <- format(all_fri, "%Y%m%d") #reformatting
fri_str <- as.character(fri_str) #transforming into character vector
head(fri_str)
fri_str[1]
fri_str[430]
fri_str <- fri_str[1:430]

urls2 <- map(
  fri_str,
  ~paste0(
    "https://cdnc.ucr.edu/?a=d&d=OLSF",
    .x,
    ".1.", 
    1:8,
    "&f=XML&cloudflare=93N102TQ3B"
  )
) |> unlist() 

length(urls2)
urls2[1]
batch_size <- 100  
## creating smaller batches eve with safe helpers to ensure no data is lost
url_batch2 <- split(urls2, ceiling(seq_along(urls2) / batch_size))
length(url_batch2)

url_batch29 <- split(urls29, ceiling(seq_along(urls29) / batch_size))
length(url_batch29)

for (b in seq_along(url_batch29)) {
  cat("Starting Batch", b, "of", length(url_batch29), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batch29[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batch29[[b]]), "\n")
      process_page(url_batch29[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 20, 40))
}

for (b in seq_along(url_batch2)) {
  cat("Starting Batch", b, "of", length(url_batch2), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batch2[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batch2[[b]]), "\n")
      process_page(url_batch2[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 20, 40))
}


length(urls2)
urls2[1]

urls25 <- urls2[1:200]
urls26 <- urls2[201:400]
urls27 <- urls2[401:600]
urls28 <- urls2[601:1000]
urls288 <- urls28[200:400]
urls29 <- urls2[2000:3440]
batch_size <- 100  
## creating smaller batches eve with safe helpers to ensure no data is lost
url_batch25 <- split(urls25, ceiling(seq_along(urls25) / batch_size))
length(url_batch25)

for (b in seq_along(url_batch25)) {
  cat("Starting Batch", b, "of", length(url_batch25), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batch25[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batch25[[b]]), "\n")
      process_page(url_batch25[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 20, 40))
}



url_batch27 <- split(urls27, ceiling(seq_along(urls27) / batch_size))
length(url_batch27)

for (b in seq_along(url_batch27)) {
  cat("Starting Batch", b, "of", length(url_batch27), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batch27[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batch27[[b]]), "\n")
      process_page(url_batch27[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 20, 40))
}



url_batch288 <- split(urls288, ceiling(seq_along(urls288) / batch_size))
length(url_batch288)

for (b in seq_along(url_batch288)) {
  cat("Starting Batch", b, "of", length(url_batch28), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batch288[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batch288[[b]]), "\n")
      process_page(url_batch288[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 20, 40))
}

url_batch29 <- split(urls29, ceiling(seq_along(urls29) / batch_size))
length(url_batch29)

for (b in seq_along(url_batch29)) {
  cat("Starting Batch", b, "of", length(url_batch29), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batch29[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batch29[[b]]), "\n")
      process_page(url_batch29[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 20, 40))
}
#####################################
## Mondays
####################################
mtoday <- as.Date('1988-12-26')

days_since_mon <- (as.POSIXlt(mtoday)$wday - 1) %% 7
most_recent_mon <- mtoday - days_since_mon

cutoff <- mtoday - 32 * 365.25
all_mon <- seq(from = most_recent_mon, to = cutoff, by = "-7 days")
head(all_mon) #finding all saturdays between 1988 and 1900

m_str <- format(all_mon, "%Y%m%d") #reformatting
m_str <- as.character(m_str) #transforming into character vector
head(m_str)
m_str[1]
m_str[1655]
m_str <- m_str[1:1655]

urls3 <- paste0("https://cdnc.ucr.edu/?a=d&d=OLSF",m_str, ".1.", 1:8, "&f=XML&cloudflare=93N102TQ3B"
) |> unlist() 

length(urls3)
urls3[1]
batch_size <- 50  
## creating smaller batches eve with safe helpers to ensure no data is lost
url_batch3 <- split(urls3, ceiling(seq_along(urls3) / batch_size))
length(url_batch3)

########################################
## 4. Polite Session
#######################################

session <- scrape(
  bow("https://cdnc.ucr.edu") %>%
    user_agent = "Academic research (contact: zahrajnkhan@gmail.com)" %>%
    scrape(content = "xml")
)
session
######################################
## 5. Safe Helpers
######################################
safe_read <- function(url) {
  tryCatch(
    {
      Sys.sleep(runif(1, 1, 3))
      scrape(session, url) %>% read_xml()
    },
    error = function(e){
      write(url, "failed_links.txt", append = TRUE)
      NA
    }
  )
}

safe_text <- function(xml, xpath) {
  if (is.na(xml)) return (NA)
  node <- xml_find_first(xml, xpath)
  if (length(node) == 0) NA else xml_text(node)
}


################################
## 6. Process One Page
################################

process_page <- function(url){
  xml <- safe_read(url)
  if (is.na(xml)) return(NULL)
  
  tibble(
    date = safe_text(xml, ".//DocumentDate"),
    page = safe_text(xml, ".//DocumentNumber"),
    text = safe_text(xml, ".//PageTextHTML"),
    volume = safe_text(xml, ".//DocumentVolume"),
    number = safe_text(xml, ".//DocumentNumber")
  )
}


output_file <- "ol_texts.csv"
if(!file.exists(output_file)) {
  write_csv(
    tibble(date = character(),
           page = character(),
           text = character(),
           volume = character(),
           number = character()),
    output_file
  )
}



for (b in seq_along(url_batch3)) {
  cat("Starting Batch", b, "of", length(url_batch3), "\n")
  
  batch_results <- map_dfr(
    seq_along(url_batch3[[b]]),
    function(i) {
      cat("Page", i, "of", length(url_batch3[[b]]), "\n")
      process_page(url_batch3[[b]][i])
    }
  )
  
  if (nrow(batch_results) > 0) {
    write_csv(batch_results, output_file, append = TRUE)
  }
  
  Sys.sleep(runif(1, 20, 40))
}

