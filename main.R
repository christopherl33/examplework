#------ Sept 2019 Completions
library(tidyverse)
library(lubridate)

#load data
df <- read.csv("C:/workspaceR/Sept_2019_completions/Data.csv")

#------clean duplicate lesson+user_id, keep first
df <- df %>% distinct(lesson, user_id, .keep_all = TRUE)

#------remove class_end_date before Sep 1, 2019
df$class_end_date <- as.character(df$class_end_date)
df$class_end_date <- as.Date(df$class_end_date, format='%m/%d/%Y')

df <- df %>% filter(class_end_date >= as.Date('2019-09-01'))

#------completion status
df <- df %>%
  mutate_at(.vars = c("lesson_started","lesson_completed"),
            .funs = list(~ifelse(.=="", NA, as.character(.)))) #fill blank cells with NA in specified columns

df$lesson_started <- as.character(df$lesson_started)
df$lesson_started <- as.Date(df$lesson_started, format='%m/%d/%Y') #format day

df$lesson_completed <- as.character(df$lesson_completed)
df$lesson_completed <- as.Date(df$lesson_completed, format='%m/%d/%Y') #format day

df <- df %>% mutate(cs = case_when(!is.na(lesson_started) & month(lesson_completed)==9 & year(lesson_completed)==2019 ~ 1, # cond1
                           is.na(lesson_started) & is.na(lesson_completed) ~ NA_real_, # cond 2
                           is.na(lesson_completed) & month(lesson_started)==9 & year(lesson_started)==2019 ~ 0, # cond 3
                           month(lesson_started)==9 & year(lesson_started)==2019 & month(lesson_completed)==10 & year(lesson_completed)==2019 ~ 0, # cond 4
                           TRUE ~ 2 )) # all other cases 


#######
#export to csv
write.csv(df,file = file.choose(new = T))
