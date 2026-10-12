install.packages("ipumsr")   # only needed once

library(DBI)
library(ipumsr)

con <- dbConnect(RPostgres::Postgres(),
                 dbname = "irish_census", host = "localhost",
                 port = 5432, user = Sys.info()[["user"]])

dat <- read_ipums_micro("~/Downloads/ipumsi_00001.xml")

# strip IPUMS labels so Postgres can store plain values
dat <- dplyr::mutate(dat, dplyr::across(dplyr::everything(), as.vector))

# lower-case column names so you don't have to quote them in SQL
names(dat) <- tolower(names(dat))

dbWriteTable(con, "persons_raw", dat, overwrite = TRUE)
dbDisconnect(con)
