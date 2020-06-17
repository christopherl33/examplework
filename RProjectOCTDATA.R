#Completion Data
library(tidyverse)
library(lubridate)
library(data.table)
library(tibbletime)


#defining df 


#insert dates of interest

df <- tmp_jason_data_completions_2019.11.01

#cleans duplicates of lesson and userid

#change variables to character format, then date format

df$lesson_started <- as.character(df$lesson_started)
df$lesson_started <- as.Date(df$lesson_started, format = "%Y-%m-%d") 

df$lesson_completed <- as.character(df$lesson_completed)
df$lesson_completed <- as.Date(df$lesson_completed, format = "%Y-%m-%d")

df <- df %>% filter(lesson_completed >= as.Date('2019-10-1'))

df <- df %>% distinct(lesson, user_id, .keep_all = TRUE)

#remove data not needed

df <- df[,-c(1)]
df <- df[,-c(4:7)]
df <- df[,-c(8)]
df <- df[,-c(9:13)]
df <- df[,-c(10:17)]

#adds conditions
df <- df %>% mutate(OCTSTATUS = case_when(month(lesson_started)== "monthx" & month(lesson_completed)==10 & year(lesson_completed)==2019 ~ 1,
                                          month(lesson_completed)==10 & year(lesson_completed)==2019 ~ 1,
                                          month(lesson_started)== 10 & year(lesson_started)==2019 & is.na(lesson_completed) ~ 0,
                                          month(lesson_started)== 10 & year(lesson_started)==2019 & month(lesson_completed)==11
                                          & year(lesson_completed)== 2019 ~ 0,
                                          TRUE ~ 2 ))


#output
table(df$OCTSTATUS)
#completions
table(df$partner, df$OCTSTATUS)
table(df$site, df$OCTSTATUS)
table(df$class, df$OCTSTATUS)
#logins
table(df$logins)

#export to csv
write.csv(df,file = file.choose(new = T))


#other (dont run)

#filters out completion dates
df <- df %>% filter(lesson_completed >= as.Date('2019-10-1'))
df <- df %>% filter(lesson_started <= as.Date('2019-10-1'))
