
# URISA PEAR COMMITTEE GIS PRO BLACK OWNED BUSINESSES ---------------------


# PURPOSE -----------------------------------------------------------------

# webscrape black owned businesses from website:https://www.blackownedmaine.com/bom-listing/



# LIBRARIES ---------------------------------------------------------------


# Install and load required libraries
# install.packages(c("rvest", "tidyverse", "readr", "purrr"))
library(rvest)
library(tidyverse)
library(readr)
library(purrr)
library(jsonlite)




# SCRAPE DIRECTORY OF BUSINESSES ------------------------------------------
# Function to scrape a single page
scrape_page<-function(page_url) {
  webpage <- read_html(page_url)
  
  # Extract business names
  business_names <- webpage %>%
    html_nodes(".business-name a") %>%
    html_text(trim = TRUE)
  
  # Extract business addresses
  business_addresses <- webpage %>%
    html_nodes(".location") %>%
    html_text(trim = TRUE)
  
  # Extract business descriptions
  business_descriptions <- webpage %>%
    html_nodes(".description .flex-grow-1") %>%
    html_text(trim = TRUE)
  
  # Extract business categories
  business_categories <- webpage %>%
    html_nodes(".categories") %>%
    html_text(trim = TRUE)
  
  # Extract business URLs
  business_urls <- webpage %>%
    html_nodes(".business-name a") %>%
    html_attr("href")
  
  # Combine data into a data frame
  data <- data.frame(
    Name_of_Business = business_names,
    Business_Address = business_addresses,
    Description = business_descriptions,
    Category = business_categories,
    Business_URL = business_urls,
    stringsAsFactors = FALSE
  )
  
  return(data)
}

# Base URL and page range
"https://blackownedmaine.com/bom-listing/?sort_by=business_name&zip_code&within_miles&lpage="->base_url
1:100 ->page_numbers  # Adjust if the total number of pages changes

# Create full URLs for all directory pages
paste0(base_url, page_numbers)->page_urls

# Scrape all directory pages
map_df(page_urls, scrape_page)->all_data



# SCRAPE SUBDIRECTORY BUSINESS PAGES --------------------------------------

# > cleaning functions ----------------------------------------------------


# Function to scrape a single page
extract_business_info <-function(url) {
  page <- read_html(url)
  
  # Extract Title, Description, and Webpage
  title <- page %>% html_node("title") %>% html_text(trim = TRUE)
  description <- page %>% html_node("meta[name='description']") %>% html_attr("content")
  webpage <- page %>% html_node("link[rel='canonical']") %>% html_attr("href")
  
  # Extract Address
  address <- page %>% html_node(".location .address") %>% html_text(trim = TRUE)
  
  # Extract Contact Info
  email <- page %>% html_node(".contact-info .email a") %>% html_text(trim = TRUE)
  phone <- page %>% html_node(".contact-info .phone a") %>% html_text(trim = TRUE)
  
  # Extract Social Media Handles
  facebook <- page %>%  html_node(".social-media a[href*='facebook']") %>% html_attr("href")
  instagram <- page %>% html_node(".social-media a[href*='instagram']") %>% html_attr("href")
  twitter <- page %>% html_node(".social-media a[href*='twitter']") %>% html_attr("href")
  # Extract Description Details
  description_details <- page %>% html_node(".description p") %>% html_text(trim = TRUE)
  
  # Extract Business Website URL
  business_website <- page %>%
    html_element("a:contains('Visit Website')") %>%
    html_attr("href")
  
  # Extract Language Spoken
  language_spoken <- page %>%
    html_nodes(xpath = '//div[contains(text(), "Languages Spoken:")]') %>%
    html_text() %>%
    str_extract('(?<=Languages Spoken: ).*') %>%
    str_trim()
  
  tibble(
    Title = ifelse(length(title) == 0, NA, title),
    Description = ifelse(length(description) == 0, NA, description),
    Webpage = ifelse(length(webpage) == 0, NA, webpage),
    Address = ifelse(length(address) == 0, NA, address),
    Email = ifelse(length(email) == 0, NA, email),
    Phone = ifelse(length(phone) == 0, NA, phone),
    Facebook_Page = ifelse(length(facebook) == 0, NA, facebook),
    Twitter_Page = ifelse(length(twitter) == 0, NA, twitter),
    Instagram_Page = ifelse(length(instagram) == 0, NA, instagram),
    Description_Details = ifelse(length(description_details) == 0, NA, description_details),
    Business_Website = ifelse(length(business_website) == 0, NA, business_website),
    Language_Spoken = ifelse(length(language_spoken) == 0, NA, language_spoken)
  ) } 
# Function to clean and standardize phone numbers
standardize_phone<-function(phone) {
  phone %>%
    str_remove_all("\\s") %>%                          # Remove whitespace
    str_replace_all("[^0-9x]", "") %>%                 # Remove non-numeric and non-'x' characters
    str_replace_all("x", " x")                         # Add space before 'x' if present
} 
# Function to clean extra whitespace
clean_whitespace<-function(text) {
  text %>%
    str_squish() %>%                                   # Remove excessive whitespace
    str_trim()                                         # Trim leading/trailing whitespace
}
# Function to remove quotes from description
remove_quotes<- function(text) {
  text %>%
    str_replace_all("[“”]", "") %>%                    # Remove left and right quotes
    str_replace_all("\"", "") # %>%                         # Remove remaining quotes
    
}
clean_title<-function(title) {
  # Remove the part of the title after " - Black Owned Maine"
  cleaned_title <- gsub(" - Black Owned Maine$", "", title)
  return(cleaned_title)
} 


# > scrape all subdirectory pages of businesses ---------------------------

# Scrape all pages
map_df(all_data$Business_URL, extract_business_info)->result



# > clean data ------------------------------------------------------------

result %>% 
  mutate(
    Facebook_Page = case_when(
      Facebook_Page == "https://www.facebook.com/blackownedmaine" ~ "", 
      TRUE ~ Facebook_Page
    ),
    Instagram_Page = case_when(
      Instagram_Page == "https://www.instagram.com/blackownedmaine" ~ "", 
      TRUE ~ Instagram_Page
    ),
    Address = clean_whitespace(Address),
    Phone = standardize_phone(Phone),
    Description_Details = remove_quotes(Description_Details),
    Title = clean_title(Title),  # Apply the clean_title function here
    Source = "Black Owned Maine"
  )  %>% 
    merge(all_data, 
          by.y = "Business_URL",
          by.x = "Webpage", all.y=T) %>% 
  rename(Business_City = Business_Address,
         Description_Short = Description.x, 
         # Business_Website = Business_website,
         Description_Long = Description_Details,
         Description_Complete = Description.y,
         Business_Address = Address) %>% 
  mutate(Business_City= case_when(grepl(x = Business_City, "Portland") ~ "Portland, Maine", TRUE~ Business_City)) %>% 
  dplyr::select(-Title) %>% 
  mutate(across(where(is.character), ~ na_if(., ""))) %>% 
  dplyr::select(

Name_of_Business  ,
Webpage             ,
Business_Website    ,
Description_Short   ,
Description_Long    ,
Description_Complete ,
Category    ,
Language_Spoken  ,   
Business_Address    ,
Business_City       ,
Email               ,
Phone               ,
Facebook_Page       ,
Instagram_Page      ,
Twitter_Page,
Source              )->results_cleaned

# results_cleaned %>% names %>% print.default()
# # data dictionary
# # 
# tibble(
# Name_of_Business    | Name of Business
# Webpage             | Source Black Owned Maine business directory URL
# Business_Website    | Business website (if available)
# Description_Short   | Short Description (busines strategic plan/mission/description, missing for some businesses)
# Description_Long    | Long Description (more detailed)
# Description_Complete | Short Description available for all business 
# Category             | Business category type
# Language_Spoken      | Language associated with business
# Business_Address     | Physical business adress (if available)
# Business_City        | Associated city with Business (if available)
# Email                | Business email address (if available)
# Phone                | Business Phone number (if available)
# Facebook_Page        | Business Facebook website (if available)
# Instagram_Page       | Business Instagram page (if available)
# Twitter_Page         | Business Twitter page (if available)
# Source               | Credit and data source (Black Owned Maine))
# 
# 

# > save all in maine -----------------------------------------------------

 
write_csv(results_cleaned, "Black_Owned_Businesses_Maine.csv")

results_cleaned %>% 
  filter(Business_City %in% "Portland, Maine") 


# install.packages("tidygeocoder")
# install.packages("osmdata")
# install.packages("sf")
# install.packages('ggmap')
library('tidygeocoder')
library('osmdata')
library("sf")
library('ggmap')


# limit to businesses with in portland city limits ------------------------
# 
# The following are part of Portland city or its metropolitan area in Maine:
#   Portland, Maine
# South Portland, Maine (neighboring city and part of the Portland metropolitan area)
# Peaks Island, Maine (a part of Portland, located in Casco Bay)
# Cape Elizabeth, Maine (a town neighboring Portland)
# Westbrook, Maine (neighboring Portland)
# Falmouth, Maine (a suburb of Portland)
# Scarborough, Maine (a nearby town in the Portland metropolitan area)

results_cleaned %>% head %>% 
  geocode(address = Business_Address,
          method = 'osm', lat = latitude , long = longitude)->geocoded_data

# Install necessary packages if not already installed
# install.packages(c("osmdata", "sf", "ggplot2", "dplyr", "ggmap"))

library(osmdata)
library(sf)
library(ggplot2)
library(dplyr)
library(ggmap)

# Define the bounding box for Portland, Maine
bbox <- c(-70.3, 43.6, -70.1, 43.8)

# Retrieve Portland city boundary from OSM
portland_data <- opq(bbox = bbox) %>% 
  add_osm_feature(key = 'boundary', value = 'administrative') %>%
  add_osm_feature(key = 'name', value = 'Portland') %>%
  add_osm_feature(key = 'admin_level', value = '8') %>%  # Admin level for cities
  osmdata_sf()

# Convert to a data frame
portland_df <- portland_data$osm_multipolygons %>% st_drop_geometry() %>% as.data.frame()

# Filter to get Portland city boundary
portland_boundary <- portland_data$osm_multipolygons %>%
  filter(name == "Portland" & place == "city" & `tiger:STATEFP` == "23")

# Plot the Portland boundary to confirm it's correct
ggplot() +
  geom_sf(data = portland_boundary, fill = 'blue', color = 'black') +
  theme_minimal() +
  labs(title = "Portland City Boundary")


# Define a function to check if a point is within the Portland boundary
is_within_portland <- function(address, portland_boundary_sf) {
  
  # Set working directory and read API token
  working <- "/Users/rashaelnimeiry/Library/Mobile Documents/com~apple~CloudDocs/geotruth"
  
  # Read the Google Maps API key from the Excel file
  token <- readxl::read_xlsx(glue::glue('{working}/keys/tokens.xlsx')) %>% 
    filter(Program == 'googlegeocode') %>%
    pull(key)
  
  # Register Google Maps API key
  register_google(key = token)
  
  # Geocode the address with error handling
  coords <- tryCatch({
    geocode(address)
  }, error = function(e) {
    message(paste("Geocoding failed for address:", address, "Error:", e$message))
    return(data.frame(lon = NA, lat = NA))
  })
  
  # Check if geocoding was successful
  if (is.na(coords$lon) || is.na(coords$lat)) {
    return(FALSE)
  }
  
  # Create a spatial point object
  point_sf <- st_as_sf(data.frame(lon = coords$lon, lat = coords$lat), coords = c("lon", "lat"), crs = 4326)
  
  # Check if the point is within the boundary
  within_boundary <- st_intersects(point_sf, portland_boundary_sf, sparse = FALSE)
  
  # Return TRUE if point is within boundary, otherwise FALSE
  return(any(within_boundary))
}





# Filter businesses within Portland
businesses_within_portland <- results_cleaned %>% 
  filter(map_lgl(Business_Address, ~ is_within_portland(.x, portland_boundary)))

results_cleaned %>% 
  filter(Business_City %in% "Portland, Maine")-> inportland

anti_join(x =  businesses_within_portland, by = "Name_of_Business", y = inportland)  %>% 
  rbind(
anti_join(y =  businesses_within_portland, by = "Name_of_Business", x = inportland) )%>% 
  distinct() ->toadd

# Define the list of cities close/neighborhood to portland in (case-insensitive)
city_names <- c(
  "Portland, Maine", 
  "South Portland, Maine", 
  "Peaks Island, Maine", 
  "Cape Elizabeth, Maine", 
  "Westbrook, Maine", 
  "Falmouth, Maine", 
  "Scarborough, Maine"
)

# Filter the data using purrr::map_lgl for case-insensitive matching
results_cleaned %>%
  filter(map_lgl(Business_City, function(city) {
    any(map_lgl(city_names, ~ grepl(.x, city, ignore.case = TRUE)))
  })) %>% 
  distinct->allportlandcitylimit

write_csv(results_cleaned, "BOB_Maine_InAround_Portland_City_Limit.csv")
results_cleaned %>% 
  filter(Business_City %in% "Online Business")->onlinebusiness

write_csv(results_cleaned, "BOB_Maine_Online_Businesses.csv")


# businesses with out addressess  -----------------------------------------



library(httr)
library(jsonlite)

# Replace with your actual Google API key


# Function to get business address by name using Google Places API
get_business_address <- function(business_name) {
  # Construct API request URL
  working <- "/Users/rashaelnimeiry/Library/Mobile Documents/com~apple~CloudDocs/geotruth"
  
  # Read the Google Maps API key from the Excel file
  token <- readxl::read_xlsx(glue::glue('{working}/keys/tokens.xlsx')) %>% 
    filter(Program == 'googleplaces') %>%
    pull(key)

  url <- paste0(
    "https://maps.googleapis.com/maps/api/place/findplacefromtext/json?input=",
    URLencode(business_name), "&inputtype=textquery&fields=formatted_address,name&key=", token
  )
  
  # Send GET request to Google Places API
  res <- httr::GET(url)
  
  # Parse the result
  data <- jsonlite::fromJSON(content(res, as = "text"))
  
  # Extract the address if available
  if (length(data$candidates) > 0) {
    return(data$candidates$formatted_address)
  } else {
    return(NA)
  }
}

# Example business names
businesses <- c("Starbucks", "Whole Foods", "Portland City Hall")

results_cleaned$Business_Address
# Get addresses for each business
addresses <- sapply(glue::glue("{results_cleaned$Name_of_Business %>% head(10)} {results_cleaned$Business_City %>% head(10)}"), get_business_address)

# Combine business names and addresses into a data frame
business_addresses <- data.frame(Business = businesses, Address = addresses)

print(addresses) %>% view


