
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
 
write_csv(results_cleaned, "Black_Owned_Businesses_Maine.csv")



