# --------------- script.r -----------------------
#
#     PEARL - Digital Democracy 2019 Research 
#   
#    Stephanie Tran
# 
# --------------------------------------------------

# required packages
install.packages('car')
install.packages('rio')
install.packages('dplyr')   
install.packages('plyr')  
install.packages('openxlsx')
install.packages('readr')
install.packages('tidyverse')   
install.packages('data.table')   


# --------------------------------------------------
#    --- SETTING UP ---
# 
# --------------------------------------------------
library(car)    
library(rio)
library(dplyr)
library(plyr)
library(openxlsx)
library(readr)    
library(tidyverse)  
library(data.table)


# --------------------------------------------------
#    --- SETTING UP ---
# 
# --------------------------------------------------
# create data.table from csv file
md <- fread('all-visits.csv')

# set blank inputs as NA
md[md==""] <- NA


# --------------------------------------------------
#    --- Preliminary observations ---
# 

# add column startDate 
md$startDate <- as.Date(md$start_time)

# check number of unique instances of each variable
sapply(md, function(x) length(unique(x))) %>%  write.xlsx('unique_instances.xlsx')

#   member_id               visit_id           page_view_id 
#   754                     318051                2274384 
#   site_name              category1              category2 
#   22263                     42                     28 
#   category3              category4              category5 
#     27                     19                     12 
#   page_url            search_term             start_time 
#   1024444                  61246                1457879 
#   end_time               duration            duration_ms 
#    1459634                   6656                 386948 
#   device_id            day_of_week           device_class 
#     897                      7                      3 
#   source              device_os        unlocalized_start_time 
#     2                      6                2273199 

# check number of empty URL records
sum(is.na(md$page_url))
#  16620

# 1) How many websites a person visits in a day. 
# 2) How long they spend on average on any single page.
# 3) How much variance there is in the time they spend on a page.
# 
# --------------------------------------------------

# --------------------------------------------------
#
# 1) How many websites a person visits in a day. 
#
# --------------------------------------------------

# find number of users with at least one NA in site url
md[, is.na(md$page_url), by = member_id]


# find : total unique page url's per user (note: page URLs are not recorded for every page visit),
#         total unique site names per user,
#         average duration per site (ms),
#         average duration per site (minutes),        
#         average number of sites a day,
#         number of records without url's 
# 
# add results as columns in userVisits
totalUserVisits = md[, list( 
                       total_unique_site_names = uniqueN(site_name), 
                       total_unique_page_urls = uniqueN(page_url),
                     avg_num_sites_per_day = uniqueN(page_url) / uniqueN(startDate),
                     avg_duration_ms = mean(duration_ms),
                     avg_duration_minutes = mean(duration_ms / 6000),
                     blank_urls = sum(is.na(page_url))),
                     by = member_id] 


# find number of respondents without blank url records
count(totalUserVisits$blank_urls == 0)
#   676 users without blank url records
#   78 with blank url records


