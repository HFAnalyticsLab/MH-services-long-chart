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

scotland_referrals <- fread(paste0(rawdatadir_scot,
                                   "Referrals/camhs-referrals.csv"),
                            header=TRUE, sep=",", check.names=T) %>%
  select(.,HB,Month,ReferralsAccepted,ReferralsAcceptedQF,ReferralsReceived,ReferralsReceivedQF) %>%
  mutate(.,pct_accepted=ReferralsAccepted/ReferralsReceived*100) %>%
  mutate_all(as.character) %>%
  pivot_longer(.,
               cols = ReferralsAccepted:pct_accepted,
               names_to = "Metric",
               values_to = "Count")

#Open cases

scotland_opencases <- fread(paste0(rawdatadir_scot,
                                   "Open cases/camhs-opencases.csv"),
                            header=TRUE, sep=",", check.names=T) %>%
  select(.,HB,Month,OpenCases,OpenCasesQF) %>%
  mutate_all(as.character) %>%
  pivot_longer(.,
               cols = OpenCases:OpenCasesQF,
               names_to = "Metric",
               values_to = "Count")

#Patients seen

scotland_patientsseen <- fread(paste0(rawdatadir_scot,
                                   "Adjusted patients seen/camhs-adjusted-patients-seen.csv"),
                            header=TRUE, sep=",", check.names=T) %>%
  mutate_all(as.character) %>%
  pivot_longer(.,
               cols = TotalPatientsSeen:X90thPercentileWeeksPatientsSeenQF,
               names_to = "Metric",
               values_to = "Count")

#Patients waiting

scotland_patientswait <- fread(paste0(rawdatadir_scot,
                                      "Adjusted patients waiting/camhs-adjusted-patients-waiting.csv"),
                               header=TRUE, sep=",", check.names=T) %>%
  mutate_all(as.character) %>%
  pivot_longer(.,
               cols = TotalPatientsWaiting:NumberOfPatientsWaitingOver52WeeksQF,
               names_to = "Metric",
               values_to = "Count")

#Append data

scotland_camhs <- plyr::rbind.fill(scotland_referrals,scotland_opencases,scotland_patientsseen,scotland_patientswait)
rm(scotland_referrals,scotland_opencases,scotland_patientsseen,scotland_patientswait)

#Add variables

scotland_camhs <- scotland_camhs %>%
  mutate(.,year=as.numeric(substr(Month, 1, 4)),
         month=as.numeric(substr(Month,5,6))) %>%
  mutate(.,year_month=lubridate::ymd(paste(year,month,"1",sep="-")))

#Save data for dashboard
fwrite(scotland_camhs, here::here("Clean data for dashboard","scotland_camhs_appended.csv"), row.names = F, sep = ",")

#See all fields
unique(scotland_camhs$Metric)

#Patients seen and referrals received

contacts_data <- scotland_camhs %>%
  filter(., Metric %in% c("ReferralsReceived","TotalPatientsSeen","ReferralsAccepted"),
         year_month>=ymd("2018-01-01")) %>%
  mutate(., Count=as.numeric(Count))

contacts_chart <- contacts_data %>%
  ggplot(., aes(x=year_month, y=Count, group=Metric)) +
  geom_line(aes(color=Metric, linetype=Metric),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "6 months") +
  scale_y_continuous(labels = scales::comma, limits = c(0,6000)) +
  theme_ipsum() +
  xlab("") +
  ylab("Number of people") +
  labs(col="",
       title="People in contact with CAMHS in Scotland") +
  scale_color_manual(values=c("ReferralsAccepted" = "chartreuse4",
                              "ReferralsReceived" = "maroon",
                              "TotalPatientsSeen" = "navyblue")) +
  scale_linetype_manual(values=c("ReferralsAccepted" = "dotted",
                                 "ReferralsReceived" = "dotted",
                                 "TotalPatientsSeen" = "solid")) +
  guides(linetype = FALSE)  +
  theme(legend.position="right",
        panel.border = element_blank(),
        strip.text = element_text(size=12),
        text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
        axis.title.y = element_text(size = 12))

ggplotly(contacts_chart)

#Patients seen and patients waiting

waiting_data <- scotland_camhs %>%
  filter(., Metric %in% c("TotalPatientsWaiting","TotalPatientsSeen"),
         year_month>=ymd("2018-01-01")) %>%
  mutate(., Count=as.numeric(Count))

waiting_chart <- waiting_data %>%
  ggplot(., aes(x=year_month, y=Count, group=Metric)) +
  geom_line(aes(color=Metric, linetype=Metric),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "6 months") +
  scale_y_continuous(labels = scales::comma, limits = c(0,15000)) +
  theme_ipsum() +
  xlab("") +
  ylab("Number of people") +
  labs(col="",
       title="People in contact with CAMHS in Scotland") +
  scale_color_manual(values=c("TotalPatientsWaiting" = "orange2",
                              "TotalPatientsSeen" = "navyblue")) +
  scale_linetype_manual(values=c("TotalPatientsWaiting" = "dotted",
                                 "TotalPatientsSeen" = "solid")) +
  guides(linetype = FALSE)  +
  theme(legend.position="right",
        panel.border = element_blank(),
        strip.text = element_text(size=12),
        text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
        axis.title.y = element_text(size = 12))

ggplotly(waiting_chart)

#Wait times

waiting_time_data <- scotland_camhs %>%
  filter(., Metric %in% c("MedianWeeksPatientsSeen",
                          "X90thPercentileWeeksPatientsSeen"),
         year_month>=ymd("2017-01-01")) %>%
  mutate(., Count=as.numeric(Count))

waiting_time_chart <- waiting_time_data %>%
  ggplot(., aes(x=year_month, y=Count, group=Metric)) +
  geom_line(aes(color=Metric, linetype=Metric),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "6 months") +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum() +
  xlab("") +
  ylab("Weeks") +
  labs(col="",
       title="Waiting times to access CAMHS in Scotland") +
  scale_color_manual(values=c("MedianWeeksPatientsSeen" = "lightsalmon",
                              "X90thPercentileWeeksPatientsSeen" = "lightsalmon4")) +
  scale_linetype_manual(values=c("MedianWeeksPatientsSeen" = "lightsalmon",
                                 "X90thPercentileWeeksPatientsSeen" = "lightsalmon4")) +
  guides(linetype = FALSE)  +
  theme(legend.position="right",
        panel.border = element_blank(),
        strip.text = element_text(size=12),
        text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
        axis.title.y = element_text(size = 12))

ggplotly(waiting_time_chart)