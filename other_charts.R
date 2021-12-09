#Packages

library("plotly")
library("hrbrthemes")
library("tidyverse")
library("lubridate")
library("here")
library("data.table")

#Read

source(here::here("0. File locations.R"))

MHSDS_main_pooled <- fread(paste0(rawdatadir,main_name,"/Pooled/MHSDS_main_pooled.csv"),
                           header=TRUE, sep=",", check.names=T)

#Subset of metrics (to make size more manageable)

wait_time_measures <- MHSDS_main_pooled %>%
  filter(.,str_detect(MEASURE_NAME,"wait")&(!str_detect(MEASURE_NAME,"eating"))) %>% 
  pull(MEASURE_NAME) %>%
  unique(.)

under18_measures <- MHSDS_main_pooled %>%
  filter(.,str_detect(MEASURE_NAME,"0 to 18|under 18|Age 16|18th birthday|0-18|0 to 17")) %>% 
  pull(MEASURE_NAME) %>%
  unique(.)

CYP_measures <- MHSDS_main_pooled %>%
  filter(.,str_detect(MEASURE_ID,"CYP")) %>%
  pull(MEASURE_NAME) %>%
  unique(.)

#Correct dates

MHSDS_main_pooled <- MHSDS_main_pooled %>%
  filter(., MEASURE_NAME %in% c(under18_measures,CYP_measures)) %>% 
  mutate(.,format_date=ifelse(str_detect(REPORTING_PERIOD_START,"/"),"dmy","ymd")) %>% 
  mutate(.,
         start_ymd=ifelse(format_date=="ymd",REPORTING_PERIOD_START,NA),
         start_dmy=ifelse(format_date=="dmy",REPORTING_PERIOD_START,NA),
         end_ymd=ifelse(format_date=="ymd",REPORTING_PERIOD_END,NA),
         end_dmy=ifelse(format_date=="dmy",REPORTING_PERIOD_END,NA)) %>%
  mutate(.,
         start_ymd=lubridate::ymd(start_ymd),
         start_dmy=lubridate::dmy(start_dmy),
         end_ymd=lubridate::ymd(end_ymd),
         end_dmy=lubridate::dmy(end_dmy)) %>%
  mutate(.,start_date=ymd(ifelse(is.na(start_ymd),as.character(start_dmy),
                                 as.character(start_ymd))),
         end_date=ymd(ifelse(is.na(end_ymd),as.character(end_dmy),
                             as.character(end_ymd)))) %>% 
  select(.,-c("start_ymd","start_dmy","end_ymd","end_dmy")) %>%
  mutate(.,month_year=paste(lubridate::month(start_date,label = TRUE),lubridate::year(start_date),sep=" "))

#Percentage changes

CAMHS_people_in_contact_data <- MHSDS_main_pooled %>%
  filter(.,PRIMARY_LEVEL_DESCRIPTION=="England"&
           MEASURE_ID %in% c("CYP01","CYP23","MHS61a")) %>%
  select(.,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE,start_date,month_year) %>%
  mutate(.,MEASURE_VALUE=as.numeric(MEASURE_VALUE),
         start_date_l1=start_date-years(1),
         MEASURE_KEY=case_when(
           MEASURE_ID=="CYP01" ~ "People in contact",
           MEASURE_ID=="CYP23" ~ "Open referrals",
           MEASURE_ID=="MHS61a" ~ "First contacts (<18)",
           TRUE ~ "NA"
         ))

CAMHS_people_in_contact_l1 <- CAMHS_people_in_contact_data %>%
  select(.,start_date,MEASURE_VALUE,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_KEY) %>%
  rename(.,MEASURE_VALUE_l1=MEASURE_VALUE,start_date_l1=start_date)

CAMHS_people_in_contact_data <- left_join(CAMHS_people_in_contact_data,
          CAMHS_people_in_contact_l1,
          by=c("start_date_l1","PRIMARY_LEVEL_DESCRIPTION","MEASURE_ID","MEASURE_KEY")) %>%
  arrange(.,MEASURE_ID,start_date) %>%
  mutate(pct_change_l1=(MEASURE_VALUE-MEASURE_VALUE_l1)/MEASURE_VALUE_l1*100) %>%
  filter(.,!is.na(pct_change_l1))
rm(CAMHS_people_in_contact_l1)

CAMHS_people_in_contact_chart <- CAMHS_people_in_contact_data %>%
  ggplot(., aes(x=start_date, y=pct_change_l1, group= MEASURE_KEY)) +
  geom_line(aes(color= MEASURE_KEY),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "1 month") +
  ggtitle("Changes in activity compared to previous year") +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum() +
  xlab("Date") +
  ylab("% change") +
  labs(col="Metric") +
  scale_color_manual(values=c("Open referrals" = "aquamarine4",
                              "People in contact" = "tomato3",
                              "First contacts (<18)" = "olivedrab4")) +
  theme(panel.border = element_blank(),
        text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
        axis.title.y = element_text(size = 12))

ggplotly(CAMHS_people_in_contact_chart)

#Other charts

MHSDS_main_pooled %>%
   filter(., MEASURE_NAME %in% c("First attended contacts for referrals open in the RP, aged 0 to 18")) %>%
   pull(MEASURE_ID) %>%
   unique(.)

first_contact_data <- MHSDS_main_pooled %>%
  filter(.,PRIMARY_LEVEL_DESCRIPTION=="England"&
           MEASURE_ID %in% c("MHS61a")) %>%
  select(.,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE,start_date,month_year) %>%
  mutate(.,MEASURE_VALUE=as.numeric(MEASURE_VALUE))
  
first_contact_chart <- first_contact_data %>%
  ggplot(., aes(x=start_date, y=MEASURE_VALUE)) +
  geom_line(size=1, col="olivedrab4") +
  scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
  ggtitle("Number of people with first attendend contact (<18 yrs)") +
  scale_y_continuous(labels = scales::comma) +
  theme_ipsum() +
  xlab("Date") +
  ylab("People") +
  labs(col="Metric") +
  theme(panel.border = element_blank(),
        text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
        axis.title.y = element_text(size = 12))

ggplotly(first_contact_chart)

#Discharges from referral - is there a reason?

# "Proportion entering treatment waiting two weeks or less , Aged 0 to 17" 
# "Proportion waiting more than two weeks (still waiting) , Aged 0 to 17"
# "Referrals not on EIP pathway, Receiving a first contact and assigned a care co-ordinator with any team more than two weeks after referral aged 0 to 17"
# "Referrals not on EIP pathway, Receiving a first contact and assigned a care co-ordinator with any team two weeks or less after referral aged 0 to 17"

# MHSDS_main_pooled %>%
#    filter(., MEASURE_NAME %in% c("People discharged from a referral in the reporting period, aged 0 to 18")) %>%
#    pull(MEASURE_ID) %>%
#    unique(.)
# 
# discharge_data <- MHSDS_main_pooled %>%
#    filter(.,PRIMARY_LEVEL_DESCRIPTION=="England",
#           MEASURE_ID %in% c("MHS57a")) %>%
#    mutate(.,timing=ifelse(start_date<ymd("2020-04-01"),"Pre-COVID","Post-COVID"),
#           MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
#    mutate(.,timing=fct_relevel(timing, c("Pre-COVID","Post-COVID"))) %>%
#    select(.,PRIMARY_LEVEL_DESCRIPTION,start_date,end_date,month_year,timing,MEASURE_VALUE)
# 
# discharge_chart <- discharge_data %>%
#    ggplot(.) +
#    geom_bar(aes(x=start_date, y=MEASURE_VALUE), position="stack", stat="identity",fill="cornflowerblue") +
#    scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
#    scale_y_continuous(labels = scales::comma) +
#    facet_wrap(~timing, scales = "free_x") +
#    ggtitle("Number of discharges from referral (<18 yrs)") +
#    theme_ipsum() +
#    xlab("") +
#    ylab("") +
#    theme(panel.border = element_blank(),
#          text = element_text(size = 12),
#          legend.title=element_text(size=12),
#          legend.text=element_text(size=12),
#          axis.text = element_text(size = 12),
#          axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
#          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
#          axis.title.y = element_text(size = 12),
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank())
# 
#  ggplotly(discharge_chart)

#Far away ward stays
 
# MHSDS_main_pooled %>%
#    filter(., MEASURE_NAME %in% c("Open ward stays, distance >=50KM at end of the reporting period, aged 0 to 18")) %>%
#    pull(MEASURE_ID) %>%
#    unique(.)
#  
# far_ward_data <- MHSDS_main_pooled %>%
#    filter(.,PRIMARY_LEVEL_DESCRIPTION=="England",
#           MEASURE_ID %in% c("MHS22a")) %>%
#    mutate(.,timing=ifelse(start_date<ymd("2020-04-01"),"Pre-COVID","Post-COVID"),
#           MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
#    mutate(.,timing=fct_relevel(timing, c("Pre-COVID","Post-COVID"))) %>%
#    select(.,PRIMARY_LEVEL_DESCRIPTION,start_date,end_date,month_year,timing,MEASURE_VALUE)
#  
# far_ward_chart <- far_ward_data %>%
#    ggplot(.) +
#    geom_bar(aes(x=start_date, y=MEASURE_VALUE), position="stack", stat="identity",fill="darkslateblue") +
#    scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
#    scale_y_continuous(labels = scales::comma) +
#    facet_wrap(~timing, scales = "free_x") +
#    ggtitle("Number of ward stays >50km away (<18 yrs)") +
#    theme_ipsum() +
#    xlab("") +
#    ylab("") +
#    theme(panel.border = element_blank(),
#          text = element_text(size = 12),
#          legend.title=element_text(size=12),
#          legend.text=element_text(size=12),
#          axis.text = element_text(size = 12),
#          axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
#          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
#          axis.title.y = element_text(size = 12),
#          panel.grid.major = element_blank(),
#          panel.grid.minor = element_blank())
#  
#  ggplotly(far_ward_chart)

#New referrals AND people in contact

# number_contacts_data <- MHSDS_main_pooled %>%
#   filter(MEASURE_ID %in% c("CYP01","CYP32"),
#          PRIMARY_LEVEL_DESCRIPTION=="England") %>%
#   select(.,start_date,end_date,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE) %>% 
#   mutate(.,MEASURE_VALUE=as.numeric(MEASURE_VALUE),
#          timing=ifelse(start_date<ymd("2020-04-01"),"Pre-COVID","Post-COVID"),
#          type=ifelse(MEASURE_ID=="CYP01","In contact with CAMHS","New referrals"),
#          MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
#   mutate(.,timing=fct_relevel(timing, c("Pre-COVID","Post-COVID"))) %>% 
#   arrange(.,start_date) %>%
#   as_tibble()
# 
# number_contacts_chart <- number_contacts_data %>%
#   ggplot(., aes(x=start_date, y=MEASURE_VALUE, group=type)) +
#   geom_line(aes(color=type),size=1) +
#   scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
#   ggtitle("People in contact with CAMHS") +
#   scale_y_continuous(labels = scales::comma) +
#   theme_ipsum() +
#   xlab("Date") +
#   ylab("Number of people") +
#   labs(col="Type") +
#   scale_color_brewer(palette = "Set1") +
#   theme(panel.border = element_blank(),
#         text = element_text(size = 12),
#         legend.title=element_text(size=12),
#         legend.text=element_text(size=12),
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
#         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
#         axis.title.y = element_text(size = 12))
# 
# ggplotly(number_contacts_chart)

#Emergency and urgent referrals

# MHSDS_main_pooled %>%
#   filter(., MEASURE_NAME %in% c("New Emergency Referrals to Crisis Care teams in the Reporting Period, Aged under 18",
#                                 "New Urgent Referrals to Crisis Care teams in the Reporting Period, Aged under 18")) %>%
#   pull(MEASURE_ID) %>%
#   unique(.)

# urgent_crisis_referrals_data <- MHSDS_main_pooled %>%
#   filter(.,PRIMARY_LEVEL_DESCRIPTION=="England",
#          MEASURE_ID %in% c("CCR70b","CCR71b")) %>%
#   mutate(.,timing=ifelse(start_date<ymd("2020-04-01"),"Pre-COVID","Post-COVID"),
#          type=ifelse(MEASURE_ID=="CCR70b","emergency","urgent"),
#          MEASURE_VALUE=as.numeric(MEASURE_VALUE)) %>%
#   mutate(.,timing=fct_relevel(timing, c("Pre-COVID","Post-COVID"))) %>%
#   select(.,PRIMARY_LEVEL_DESCRIPTION,start_date,end_date,month_year,timing,type,MEASURE_VALUE)
# 
# urgent_crisis_referrals_chart <- urgent_crisis_referrals_data %>%
#   ggplot(.) +
#   geom_bar(aes(x=start_date, y=MEASURE_VALUE, fill=type), position="stack", stat="identity") +
#   scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
#   scale_y_continuous(labels = scales::comma) +
#   facet_wrap(~timing, scales = "free_x") +
#   ggtitle("Number of crisis referrals (<18 yrs)") +
#   theme_ipsum() +
#   xlab("") +
#   ylab("") +
#   labs(fill="Type") +
#   scale_fill_manual(values=c("urgent" = "brown", "emergency" = "firebrick2")) +
#   theme(panel.border = element_blank(),
#         text = element_text(size = 12),
#         legend.title=element_text(size=12),
#         legend.text=element_text(size=12),
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
#         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
#         axis.title.y = element_text(size = 12),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# ggplotly(urgent_crisis_referrals_chart)

#Eating disorders chart

MHSDS_ED_pooled <- fread(paste0(rawdatadir,ed_name,"/Pooled/MHSDS_ED_pooled.csv"),
                         header=TRUE, sep=",", check.names=T)

#Names of measures
ed_cyp_measures <- MHSDS_ED_pooled %>%
  select(MEASURE_NAME,MEASURE_ID) %>%
  distinct(.)

#Clean up dates
MHSDS_ED_pooled <- MHSDS_ED_pooled %>%
  filter(.,MEASURE_ID %in% c("ED88","ED89","ED86e","ED87e")) %>%
  mutate(.,start_date=lubridate::dmy(REPORTING_PERIOD_START),
         end_date=lubridate::dmy(REPORTING_PERIOD_END))

#Chart

#How many people waiting for treatment?

waiting_ED_data <- MHSDS_ED_pooled %>%
  filter(.,MEASURE_ID %in% c("ED88","ED89"),
         PRIMARY_LEVEL_DESCRIPTION=="England") %>%
  select(.,start_date,
         end_date,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE) %>%
  pivot_wider(names_from = MEASURE_ID,
              names_sep = "_",
              values_from = MEASURE_VALUE) %>%
  rename(.,urgent="ED89",
         all="ED88") %>%
  mutate(.,`non urgent`=as.numeric(all)-as.numeric(urgent),
         all=as.numeric(all),
         urgent=as.numeric(urgent)) %>%
  pivot_longer(
    cols = all:`non urgent`,
    names_to = c("type"),
    values_to = "count"
  ) %>%
  filter(.,type!="all") %>%
  mutate(.,timing=ifelse(start_date<ymd("2020-04-01"),"Pre-COVID","Post-COVID")) %>%
  mutate(.,timing=fct_relevel(timing, c("Pre-COVID","Post-COVID")))

waiting_ED_chart <- waiting_ED_data %>%
  ggplot(.) +
  geom_bar(aes(x=start_date, y=count, fill=type), position="stack", stat="identity") +
  scale_x_date(date_labels = "%b %Y",date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~timing, scales = "free_x") +
  ggtitle("Number of people waiting for treatment (<18 yrs)") +
  theme_ipsum() +
  xlab("") +
  ylab("") +
  labs(fill="Type") +
  scale_fill_manual(values=
                      c("urgent" = "brown", "non urgent" = "darkseagreen4")) +
  theme(panel.border = element_blank(),
        text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplotly(waiting_ED_chart)

#How many of those urgent cases will be entering treatment within one week?
#How many of the non-urgent cases will be seen within a month?

urgent_target_data <- MHSDS_ED_pooled %>%
  filter(.,MEASURE_ID %in% c("ED86e","ED87e"),
         PRIMARY_LEVEL_DESCRIPTION=="England") %>%
  select(.,start_date,
         end_date,PRIMARY_LEVEL_DESCRIPTION,MEASURE_ID,MEASURE_VALUE) %>%
  mutate(.,timing=ifelse(start_date<ymd("2020-04-01"),"Pre-COVID","Post-COVID")) %>%
  mutate(.,timing=fct_relevel(timing, c("Pre-COVID","Post-COVID"))) %>%
  mutate(.,MEASURE_VALUE=as.numeric(MEASURE_VALUE),
         Type=case_when(MEASURE_ID=="ED86e" ~ "urgent",
                        MEASURE_ID=="ED87e" ~ "non urgent",
                        TRUE ~ "NA"))

waiting_ED_chart <- urgent_target_data %>%
  ggplot(., aes(x=start_date, y=MEASURE_VALUE, group=Type)) +
  geom_line(aes(color=Type),size=1) +
  scale_x_date(date_labels = "%b %Y",date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~timing, scales = "free_x") +
  ggtitle("Proportion of people starting treatment within target time (<18 yrs)") +
  theme_ipsum() +
  xlab("") +
  ylab("") +
  labs(fill="Type") +
  scale_colour_manual(values=
                      c("urgent" = "brown", "non urgent" = "darkseagreen4")) +
  theme(panel.border = element_blank(),
        text = element_text(size = 12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
        axis.title.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplotly(waiting_ED_chart)

# consultation_mode_data <- MHSDS_main_pooled %>%
#   filter(.,PRIMARY_LEVEL_DESCRIPTION=="England",
#          MEASURE_ID=="MHS30e") %>%
#   filter(.,!(SECONDARY_LEVEL_DESCRIPTION %in% c("Invalid","Missing"))) %>%
#   mutate(.,mode_cat=fct_collapse(SECONDARY_LEVEL_DESCRIPTION,
#                                  F2F=c("Face to face communication"),
#                                  Virtual=c("Telephone","Telemedicine web camera"),
#                                  `E-mail/SMS`=c("Email","Short Message Service (SMS) - Text Messaging"),
#                                  Other=c("Talk type for a person unable to speak","Other"))) %>%
#   mutate(mode_cat=fct_relevel(mode_cat, c("E-mail/SMS","Other","Virtual","F2F"))) %>% 
#   group_by(PRIMARY_LEVEL_DESCRIPTION,start_date,end_date,month_year,mode_cat) %>%
#   summarise(MEASURE_VALUE=sum(as.numeric(MEASURE_VALUE),na.rm=TRUE)) %>% 
#   ungroup() %>%
#   mutate(.,timing=ifelse(start_date<ymd("2020-04-01"),"Pre-COVID","Post-COVID")) %>%
#   mutate(.,timing=fct_relevel(timing, c("Pre-COVID","Post-COVID")))
# 
# consultation_mode_chart <- consultation_mode_data %>%
#   ggplot(.) +
#   geom_bar(aes(x=start_date, y=MEASURE_VALUE, fill=mode_cat), position="stack", stat="identity") +
#   scale_x_date(date_labels = "%b %Y",date_breaks = "3 months") +
#   scale_y_continuous(labels = scales::comma) +
#   facet_wrap(~timing, scales = "free_x") +
#   ggtitle("Number of mental health contacts by mode (<18 yrs)") +
#   theme_ipsum() +
#   xlab("") +
#   ylab("") +
#   labs(fill="Mode") +
#   scale_fill_brewer(palette = "Set1") +
#   theme(panel.border = element_blank(),
#         text = element_text(size = 12),
#         legend.title=element_text(size=12),
#         legend.text=element_text(size=12),
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
#         axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size = 12),
#         axis.title.y = element_text(size = 12),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank())
# 
# ggplotly(consultation_mode_chart)