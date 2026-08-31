######################################
#General Webscraping Fucntion
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

