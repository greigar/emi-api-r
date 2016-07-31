library(httr)
library(jsonlite)
library(dplyr)

# Get API KEY from EA EMI site and export in environment - https://emi.portal.azure-api.net/developer: 
#   e.g. export APIKEY=longstring
apikey   <- Sys.getenv("APIKEY")

if ( apikey == "" ) {
  stop("Set APIKEY env var : export APIKEY=longstring")
}

base_url <- "https://emi.azure-api.net/"

# Construct and invoke URL to get data
get_json_data <- function(rtx) {
  url  <- paste0(base_url, rtx)

  print(paste("Getting:", url))

  req <- httr::GET(
    url,
    httr::add_headers("Ocp-Apim-Subscription-Key" = apikey)
  )
  json <- httr::content(req, as = "text")
  fromJSON(json)
}

# Convert date-time
tidy_data <- function(data) {
  transmute(data,
            interval_datetime = as.POSIXct(interval, format="%d-%b-%Y %H:%M"),
            Node              = pnode,
            Load              = load,
            Generation        = generation,
            Price             = price) 
}

# Get real time prices
rtp <- get_json_data("rtp") %>% tidy_data()

# Get real time dispatch
rtd <- get_json_data("rtd") %>% tidy_data()

# split into load and generation if needed - or filter where gen/load != 0
rtp_load <- filter(rtp, !grepl(" ", Node))
rtp_gen  <- filter(rtp,  grepl(" ", Node))

# Five minute price reference nodes
nodes = c("OTA2201", "HLY2201", "WKM2201", "TUI1101", "SFD2201", "HAY2201", "STK2201", "ISL2201", "BEN2201", "HWB2201", "INV2201")


