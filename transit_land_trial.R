# Load required package
library(httr)
library(jsonlite)
library(dplyr)

# Define the base URL, feed key, and your API key
base_url <- "https://transit.land/api/v2/rest/feeds"
api_key <- "vo9IsjMlCcikqWTxTcVq7m0DRjmf831N"  

feed_key <- "f-r6-nswtrainlink~sydneytrains~buswayswesternsydney~interlinebus"  # NSW
feed_key <- "f-r7h-translink"  # Qld
feed_key <- "f-pz-otagoregionalcouncil~otagoregionalcouncil"  # Otago
feed_key <- "f-r1-ptv~3"  # Vic vline [why isn't this trams?]
feed_key <- "f-rck-gowest~sealinkgroup~atairporter~atmetro~thepartybuscompany"  # auckland

## Getting the latest feed

# Construct the endpoint URL: latest versions
url <- paste0(base_url, "/", feed_key, "/download_latest_feed_version?api_key=", api_key)

# Get the resonse
response <- GET(url)

# Print - zipfile for gtfs feed
if (status_code(response) == 200) {
  # Save the file
  output_file <- "gtfs-latest-feed.zip"
  writeBin(content(response, "raw"), output_file)
  print(paste("GTFS feed downloaded and saved as", output_file))
} else {
  # Print an error message if the request failed
  print(paste("Failed to download GTFS feed. Status code:", status_code(response)))
}


## Getting a list of feeds

# Construct the endpoint URL: query for feeds available for provider
url <- paste0(base_url, "/", feed_key, "?api_key=", api_key)

# Get the resonse
response <- GET(url)

# Print - feeds available for provider
if (status_code(response) == 200) {
  # Parse the JSON content into an R object
  json_data <- content(response, "text") %>% fromJSON(flatten = TRUE)
  
  # Save the JSON data to a file (optional)
  write_json(json_data, "feed_details.json")
  print("Feed details retrieved and saved as 'feed_details.json'")
  
  # Optionally, print or inspect the data
  print(json_data)
} else {
  # Print an error message if the request failed
  print(paste("Failed to retrieve feed details. Status code:", status_code(response)))
}
