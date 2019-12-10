# --------------- main.R -----------------------
#
#     PEARL - Digital Democracy 2019-2020 Research 
#   
#             Stephanie Tran
# 
# --------------------------------------------------

# required packages
# install.packages('car')
# install.packages('rio')
# install.packages('dplyr')   
# install.packages('plyr')  
# install.packages('openxlsx')
# install.packages('readr')
# install.packages('tidyverse')   
# install.packages('data.table')   
# install.packages('lubridate')  
# install.packages('stringi')
# install.packages('ggplot2')
# install.packages('scales')

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
library(stringi)
library(ggplot2)
library(scales)

# create data.table from csv file
md <- fread('all-visits.csv')

# set blank inputs as NA
md[md==""] <- NA

# add column startDate, derived from start_time (since there is no date column)
md$startDate <- as.Date(md$start_time)

# --------------------------------------------------
#    --- Data Analysis ---
# 
# check number of unique instances of each variable
# sapply(md, function(x) length(unique(x))) %>%  write.xlsx('unique_instances.xlsx')
# check number of empty URL records
# sum(is.na(md$page_url))

# --------------------------------------------------

# (1) find : total unique page url's per user (note: page URLs are not recorded for every page visit),
#         total unique site names per user,
#         average duration per site (ms),
#         average duration per site (minutes),        
#         average number of sites a day,
#         average number of sites by weekday,
#         total news consumption
#         site duration variance,   
#         site duration standard deviation,
#         number of records without url's 
# 
# add results as columns in userVisits
totalUserVisits = md[, list( 
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
                     total_unique_site_names = uniqueN(site_name), 
                     total_page_urls = sum(!is.na(page_url)),
                     total_blank_urls = sum(is.na(page_url)),
                     percentage_of_urls_that_are_blank = ( sum(is.na(page_url)) / sum(!is.na(page_url)))),
                     by = member_id][order(total_blank_urls, member_id)] 


# find number of respondents without blank url records
# count(totalUserVisits$blank_urls == 0)

# create table of all records that are missing page url
blankRecords <- md[,
                   list(num_blank_urls=sum(is.na(page_url)),
                    total_recorded_urls = sum(!is.na(page_url))
                        ), 
                   by=startDate][order(startDate)]
# save missing url records data into Excel file
# write.xlsx(blankRecords, "Records Missing Urls.xlsx")


# save preliminary data into Excel file
# write.xlsx(totalUserVisits, "Site Visits Count and Duration by User.xlsx")

# create summary table
# totalSummary <- summary(totalUserVisits)
# write.xlsx(totalSummary, "preliminary-summaries.xlsx")


# --------------------------------------------------
#
# (2) find : total news exposure,
#           weekly average counts of news exposure
#

generalNewsExposure = md[, list( 
  # had to use operators for ranges. Tried %within% function in lubidate but would not work
  week1_news_visits= uniqueN(page_url[category1 == "News" & startDate >= '2019-09-16' & startDate <= '2019-09-22']),
  week2_news_visits= uniqueN(page_url[category1 == "News" & startDate >= '2019-09-23' & startDate <= '2019-09-29']),
  week3_news_visits= uniqueN(page_url[category1 == "News" & startDate >= '2019-09-30' & startDate <= '2019-10-06']),
  week4_news_visits= uniqueN(page_url[category1 == "News" & startDate >= '2019-10-07' & startDate <= '2019-10-13']),
  week5_news_visits= uniqueN(page_url[category1 == "News" & startDate >= '2019-10-14' & startDate <= '2019-10-20']),
  total_news_visits= uniqueN(page_url[category1 == "News" & startDate >= '2019-09-16' & startDate <= '2019-10-20']),
  blank_urls = sum(is.na(page_url))),
  by = member_id][order(blank_urls,member_id)] 

# save generalNewsExposure into Excel file
# write.xlsx(generalNewsExposure, "General News Exposure by Users.xlsx")



# --------------------------------------------------
#
# (3) find : weekly and total counts of exposure to specific sources
#
#



sources <- read.xlsx('sources.xlsx', colNames = FALSE)

# trim white space from source names
for(i in seq_along(sources)){
  sources$X1[i] <- str_trim(sources$X1[i])
}

sc<-sources$X1

for(i in seq_along(sc)){
  sc[i] <- str_trim(sc[i])
}

# duplicate md to new data table oe
oe <- copy(md)

# create columns for each source and indicate 1 if page_url comes from that source
for(j in seq_along(sc)){       
  oe[, paste0(sc[j]) := ifelse(grepl(sc[j], page_url), 1, 0)]
}

# create table of number of visits to specific outlets by respondent
outletVisits = oe[, 
          lapply(.SD, sum), 
          by = member_id, .SDcols = sc][order(member_id)]


# save outletVisits into Excel file
write.xlsx(outletVisits, "Number of Outlet-Specific Visits by User.xlsx")

mergedRespondentData <- merge(totalUserVisits, outletVisits)

# save outletVisits into Excel file
write.xlsx(mergedRespondentData, "Merged - duration, general visits, outlet-specific visits.xlsx")



