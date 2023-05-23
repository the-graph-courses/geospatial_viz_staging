# image_functions
## Ama Owusu-Darko
## 2022-05-23

#' <To create functions for creating figures in dates and times chapter>

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(pacman)) install.packages("pacman")
pacman::p_load(lubridate,
               knitr,
               tidyverse, 
               here)



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## create a bar chart of monthly totals from ebola data  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
total_mon_plot <-
  ebola_data%>%
  mutate(month_onset = lubridate::floor_date(date_of_onset, unit = "months"))%>%  
  # new column, 1st of month of onset
  count(month_onset) %>% # count cases by month
  complete(
    month_onset = seq.Date(
      min(month_onset, na.rm=T),     # include all months with no cases reported
      max(month_onset, na.rm=T),
      by="month"),
    fill = list(n = 0))%>%
  ggplot(aes(x=month_onset,y=n,fill= month_onset))+geom_col()+
  scale_x_date(date_breaks= "2 month",date_labels = "%B %Y")+
  guides(x = guide_axis(angle = 45))+ 
  labs(title= "A Bar chart showing the counts of Ebola cases per month")+
  ylab("Monthly Case count")+xlab("Month in which cases occurred")

png("ebola_data")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Labelled monthly plot  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lab_month<-
  total_mon_plot+ geom_text(aes(label=n),angle=30,check_overlap= T)
lab_month
ggsave("ch01_dates_times/images/lab_month.png")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## faceted daily plot  ---- May and June
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
daily_cases%>%
  mutate(month_onset=lubridate::month(date_of_onset, label = TRUE, abbr = FALSE),
         year_onset= lubridate::year(date_of_onset))%>%
  filter((month_onset=="May" | month_onset== "June") & year_onset== 2014)%>%
  ggplot(aes(x=date_of_onset,y=n,fill= date_of_onset))+geom_col()+
  scale_x_date(date_breaks= "1 day",date_labels = "%d %b %Y")+
  guides(x = guide_axis(angle = 45))+ facet_wrap(~month_onset, nrow=2, scales = "free")+
  labs(title= "A Bar chart showing the daily counts of Ebola cases in May 2014")+
  ylab("Daily Case count")+
  geom_text(aes(label=n),check_overlap= T, vjust= -0.2)
