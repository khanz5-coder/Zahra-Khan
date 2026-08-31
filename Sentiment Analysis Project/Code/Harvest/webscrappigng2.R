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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#########################################
## 3. Building Urls
#########################################

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
urls2 <- urls2[1]
urls2 <- urls2[2200:3440]
batch_size <- 100  
## creating smaller batches eve with safe helpers to ensure no data is lost
url_batch2 <- split(urls2, ceiling(seq_along(urls2) / batch_size))
length(url_batch2)

######################################
## 5. Safe Helpers
######################################
reading_page <- function(url) {
  tryCatch(
    {
      Sys.sleep(runif(1, 1, 2))
      
      connections <- url(url, open = 'rb')
      x <- read_xml(connections)
      
      close(connections)
      
      rm(connections)
      gc()
      x
    },
    error = function(e){
      cat(url, "\n", file = "failed_links.txt", append = TRUE)
      gc()
      NULL
    }
  )
}

text_saftey <- function(xml, xpath) {
  if (is.na(xml)) return (NA)
  node <- xml_find_first(xml, xpath)
  if (length(node) == 0) NA else xml_text(node)
}


################################
## 6. Process One Page
################################

process_page <- function(url){
  xml <- reading_page(url)
  if (is.null(xml)) return(NULL)
  
  tibble(
    date = text_saftey(xml, ".//DocumentDate"),
    page = text_saftey(xml, ".//PageTextHTML"),
    text = text_saftey(xml, ".//PageTextHTML"),
    volume = text_saftey(xml, ".//DocumentVolume"),
    number = text_saftey(xml, ".//DocumentNumber")
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
  closeAllConnections()
  gc()
  Sys.sleep(runif(1, 20, 40))
}



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

urls3 <- map(
  m_str,
  ~paste0(
    "https://cdnc.ucr.edu/?a=d&d=OLSF",
    .x,
    ".1.", 
    1:8,
    "&f=XML&cloudflare=93N102TQ3B"
  )
) |> unlist() 

length(urls3)
urls3[1]
urls3 <- urls3[393:13240]
batch_size <- 100
url_batch3 <- split(urls3, ceiling(seq_along(urls3) / batch_size))
length(url_batch3)

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
  closeAllConnections()
  gc()
  Sys.sleep(runif(1, 20, 40))
}
