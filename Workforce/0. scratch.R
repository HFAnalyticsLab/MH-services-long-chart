#Load packages
library("tidyverse")
library("lubridate")
library("here")
library("data.table")
library("readxl")
library("plotly")
library("hrbrthemes")
library("DescTools")

#Clean up the global environment
rm(list = ls())

#Read
source(here::here("0. File locations.R"))

#Load workforce data
NHS_workforce_doctors <- read_excel(paste0(workforcedir,"/NHS Workforce Statistics, May 2021 Doctors by Grade and Specialty.xlsx"),
           sheet = "Data")

#Load main performance data
MHSDS_main_pooled_dashboard <- fread("Clean data for dashboard/MHSDS_main_pooled.csv",header=TRUE, sep=",", check.names=T)

############## CAMHS activity level

CAMHS_contacts <- MHSDS_main_pooled_dashboard %>%
  filter(.,PRIMARY_LEVEL_DESCRIPTION=="England",
         MEASURE_ID=="CYP01") %>%
  select(.,start_date,MEASURE_VALUE) %>%
  mutate(.,date_ymd=lubridate::ymd(start_date)) %>%
  mutate(.,date_ymd=floor_date(date_ymd, "month"),
         measure="People in contact CAMHS",
         MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
  select(.,-"start_date") %>%
  arrange(.,date_ymd)

############## Consultants chart

psych_doctors_data <- NHS_workforce_doctors %>%
  mutate(.,date_ymd=lubridate::ymd(Date)) %>%
  mutate(.,date_ymd=floor_date(date_ymd, "month"),
         measure="FTE doctors (child and adolescent psychiatry)") %>%
  rename(.,MEASURE_VALUE=FTE) %>%
  filter(Specialty %in% c("Child and adolescent psychiatry",
                          "General psychiatry")) %>%
  group_by(date_ymd,measure,`Specialty Group`,Specialty) %>%
  summarise(MEASURE_VALUE=sum(MEASURE_VALUE,na.rm=TRUE)) %>% 
  ungroup() %>%
  bind_rows(.,CAMHS_contacts) %>% 
  mutate(.,date_ymd_l1=date_ymd-years(1))

psych_doctors_data_l1 <- psych_doctors_data %>%
  select(.,date_ymd,measure,`Specialty Group`,Specialty,MEASURE_VALUE) %>%
  rename(.,MEASURE_VALUE_l1=MEASURE_VALUE,date_ymd_l1=date_ymd)

psych_doctors_data <- left_join(psych_doctors_data,
                                          psych_doctors_data_l1,
                                          by=c("date_ymd_l1","measure","Specialty Group","Specialty")) %>%
  arrange(.,Specialty,date_ymd) %>%
  mutate(pct_change_l1=(MEASURE_VALUE-MEASURE_VALUE_l1)/MEASURE_VALUE_l1*100) %>%
  mutate(.,fy_precovid=ifelse(date_ymd>=ymd("2019-04-01")&
                                date_ymd<=ymd("2020-03-01"),1,0)) %>%
  mutate(.,date_post_covid=ifelse(fy_precovid==0,date_ymd,"pre-COVID")) %>%
  mutate(.,group_for_base_chart=paste(measure,`Specialty Group`,`Specialty`,
                                      fy_precovid,date_post_covid))
rm(psych_doctors_data_l1)

############## Raw data chart

psych_doctors_chart <- psych_doctors_data %>%
  filter(., Specialty=="Child and adolescent psychiatry") %>% 
  ggplot(., aes(x=date_ymd, y=MEASURE_VALUE, group=Specialty)) +
  geom_line(aes(color=Specialty),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "6 months") +
  scale_y_continuous(labels = scales::comma, lim=c(700,1250)) +
  theme_ipsum() +
  xlab("") +
  ylab("FTE jobs") +
  labs(col="") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="bottom",
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        text = element_text(size = 8),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        axis.text = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 8),
        axis.title.y = element_text(size = 8))

ggplotly(psych_doctors_chart)

############## Pct chart

psych_doctors_pct_chart <- psych_doctors_data %>%
  filter(., (Specialty=="Child and adolescent psychiatry"&measure=="FTE doctors (child and adolescent psychiatry)")|
           (measure=="People in contact CAMHS")) %>% 
  ggplot(., aes(x=date_ymd, y=pct_change_l1, group=measure)) +
  geom_line(aes(color=measure),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "6 months") +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum() +
  xlab("") +
  ylab("% change") +
  labs(col="") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position="bottom",
        panel.border = element_blank(),
        strip.text = element_text(size=8),
        text = element_text(size = 8),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        axis.text = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 8),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 8),
        axis.title.y = element_text(size = 8))

ggplotly(psych_doctors_pct_chart)

############## Level 100 chart

# psych_doctors_data %>%
#   filter(.,Specialty=="Child and adolescent psychiatry",
#          date_ymd>ymd("2020-03-01")) %>%
#   select(.,Specialty,fy_precovid,date_ymd,group_for_base_chart,MEASURE_VALUE) %>%
#   pull(group_for_base_chart)

base100_data <- psych_doctors_data %>%
  filter(date_ymd>=ymd("2019-04-01")) %>% 
  group_by(group_for_base_chart) %>%
  mutate(.,MEASURE_VALUE_avg=mean(MEASURE_VALUE,na.rm=TRUE)) %>%
  mutate(.,pre_value=as.numeric(ifelse(fy_precovid==1,MEASURE_VALUE_avg,NA))) %>%
  ungroup() %>%
  group_by(measure,Specialty) %>%
  mutate(.,pre_value=LOCF(pre_value)) %>%
  mutate(value_base100=MEASURE_VALUE_avg/pre_value*100) %>% 
  ungroup() %>%
  filter(date_ymd>=ymd("2020-03-01"))

base100_chart <- base100_data %>%
  mutate(Specialty=ifelse(measure=="People in contact CAMHS","People in contact CAMHS",Specialty)) %>% 
  mutate(Specialty=ifelse(Specialty=="General psychiatry","General psychiatry (FTE doctors)",Specialty)) %>%
  mutate(Specialty=ifelse(Specialty=="Child and adolescent psychiatry","Child and adolescent psychiatry (FTE doctors)",Specialty)) %>% 
  ggplot(., aes(x=date_ymd, y=value_base100, group=Specialty)) +
  geom_line(aes(color=Specialty),size=1.5) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum() +
  xlab("") +
  ylab("Activity") +
  labs(col="",
       title = "Mental health services for children and young people", 
       subtitle = "Comparison to pre-COVID levels", 
       caption = "100 = average activity in year before March 2020") +
  scale_color_brewer(palette = "Set1") +
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

ggplotly(base100_chart)