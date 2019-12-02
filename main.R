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

# check number of empty URL records
# sum(is.na(md$page_url))


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

# find : total unique page url's per user (note: page URLs are not recorded for every page visit),
#         total unique site names per user,
#         average duration per site (ms),
#         average duration per site (minutes),        
#         average number of sites a day,
#         average number of sites by weekday,
#         site duration variance,                                                                
#         number of records without url's 
# 
# add results as columns in userVisits
totalUserVisits = md[, list( 
                       total_unique_page_urls = uniqueN(page_url),
                       total_unique_site_names = uniqueN(site_name), 
                     avg_duration_ms = mean(duration_ms),
                     avg_duration_minutes = mean(duration_ms / 6000),
                     avg_num_sites_per_day = uniqueN(page_url) / uniqueN(startDate),
                     avg_num_sites_sunday = (mean(uniqueN(site_name[day_of_week=="Sunday"]))),
                     avg_num_sites_monday = (mean(uniqueN(site_name[day_of_week=="Monday"]))),
                     avg_num_sites_tuesday = (mean(uniqueN(site_name[day_of_week=="Tuesday"]))),
                     avg_num_sites_wednesday = (mean(uniqueN(site_name[day_of_week=="Wednesday"]))),
                     avg_num_sites_thursday = (mean(uniqueN(site_name[day_of_week=="Thursday"]))),
                     avg_num_sites_friday = (mean(uniqueN(site_name[day_of_week=="Friday"]))),
                     avg_num_sites_saturday = (mean(uniqueN(site_name[day_of_week=="Saturday"]))),
                     duration_variance_min = var(duration_ms / 6000),
                     duration_sd_min = sd(duration_ms / 6000),
                     blank_urls = sum(is.na(page_url))),
                     by = member_id][order(blank_urls, member_id)] 

# find number of respondents without blank url records
count(totalUserVisits$blank_urls == 0)

write.xlsx(totalUserVisits, "preliminary-user-summary.xlsx")

