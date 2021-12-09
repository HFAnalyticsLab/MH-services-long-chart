#Load packages
library("tidyverse")
library("lubridate")
library("here")
library("data.table")
library("readxl")
library("plotly")
library("hrbrthemes")
library("DescTools")
library("eatTools")

#Clean up the global environment
rm(list = ls())

#Read
source(here::here("0. File locations.R"))

#Referrals

wales_camhs <- fread(paste0(rawdatadir_wales,
                                   "Wales-waiting-first-appointment.csv"),
                            header=TRUE, sep=",", check.names=F, skip=8) %>%
  filter(.,grepl("[[:digit:]]", V1)&V1!="Source 1") %>%
  rename(.,month_year=V1,All=".") %>%
  janitor::clean_names() %>%
  mutate(.,up_to_4_weeks=as.numeric(up_to_4_weeks),
         over_4_weeks=as.numeric(over_4_weeks),
         all=as.numeric(all),
         month_year=lubridate::dmy(paste(1,month_year,sep="-"))) %>%
  mutate(.,
         pct_up_to_4_weeks=up_to_4_weeks/all*100,
         pct_over_4_weeks=over_4_weeks/all*100) %>%
  pivot_longer(.,
               cols = up_to_4_weeks:pct_over_4_weeks,
               names_to = "Metric",
               values_to = "Count")

#Save data for dashboard
fwrite(wales_camhs, here::here("Clean data for dashboard","wales_camhs.csv"), row.names = F, sep = ",")