##############################
#### Class Cancellations  ####
##############################


library(tidyverse)
library(lubridate)


#---Set Dataframe

df <- Chris_Cancellation_File

#Distinct values and cleaning

df <- df %>% distinct(SecTerm, CRS_NAME, Section, .keep_all = TRUE)

df <- df %>% 
  filter(FacetoFaceVSOnline %in% c("Face to Face", "Online/Hybrid"))

#Average Enrollment at Cancellation by Division
DivisionAggregate <- aggregate(EnrollmentatCancellation ~ Division, df, mean)


#Weekdays

df <- df %>% 
  mutate(daynumber = case_when(WeekDays == "F" ~ 1,
                               WeekDays == "FSAT" ~ 2,
                               WeekDays == "M" ~ 1,
                               WeekDays == "MF" ~ 2, 
                               WeekDays == "MSAT" ~ 2, 
                               WeekDays == "MT" ~ 2, 
                               WeekDays == "MTH" ~ 2, 
                               WeekDays == "MTHF" ~ 3, 
                               WeekDays == "MTTH" ~ 3, 
                               WeekDays == "MTTHF" ~ 4,
                               WeekDays == "MTW" ~ 3, 
                               WeekDays == "MTWF" ~ 4, 
                               WeekDays == "MTWTH" ~ 4, 
                               WeekDays == "MTWTHF" ~ 5,
                               WeekDays == "MW" ~ 2,
                               WeekDays == "MWF" ~ 3,
                               WeekDays == "MWTH" ~ 3, 
                               WeekDays == "MWTHF" ~ 4,
                               WeekDays == "SAT" ~ 1, 
                               WeekDays == "SATSUN" ~ 2, 
                               WeekDays == "SUN" ~ 1, 
                               WeekDays == "T" ~ 1,
                               WeekDays == "TF" ~ 2,
                               WeekDays == "TH" ~ 1, 
                               WeekDays == "THF" ~ 2, 
                               WeekDays == "TTH" ~ 2, 
                               WeekDays == "TTHF" ~ 3,
                               WeekDays == "TW" ~ 2,
                               WeekDays == "TWTH" ~ 3,
                               WeekDays == "TWTHF" ~ 4,
                               WeekDays == "W" ~ 1, 
                               WeekDays == "WF" ~ 2, 
                               WeekDays == "WSUN" ~ 2,
                               WeekDays == "WTH" ~ 2, 
                               WeekDays == "WTHF" ~ 3, 
                               TRUE ~ 10
                               )
         )

df <- df %>% 
  mutate(daynumber = na_if(daynumber, "10"))

#Descriptives

  #Division Count

df %>% 
  filter(FacetoFaceVSOnline == "Face to Face") %>% 
  count(Division, sort = F) %>% 
  view()

df %>% 
  filter(FacetoFaceVSOnline == "Online/Hybrid") %>% 
  count(Division, sort = F) %>% 
  view()

  #Cancellation Time

df %>% 
  filter(CancellationTime == "Before Class Start") %>% 
  count(Division, sort = F) %>% 
  view()

df %>% 
  filter(CancellationTime == "After Start & Before Census") %>% 
  count(Division, sort = F) %>% 
  view()

df %>% 
  filter(CancellationTime == "After Census") %>% 
  count(Division, sort = F) %>% 
  view()



#Inferential
  
  #Division
    #ANOVA + means

df %>%
  group_by(Division) %>%
  summarise(mean = mean(EnrollmentatCancellation, na.rm = T), n = n(),)

aovenroll <- aov(EnrollmentatCancellation ~ Division, data = df)
summary(aovenroll)


df %>%
  group_by(Division) %>%
  summarise(mean = mean(EnrolledOtherSections, na.rm = T), n = n(),)

aovenrollothersec <- aov(EnrolledOtherSections ~ Division, data = df)
summary(aovenrollothersec)


  #Days/Week
    #ANOVA + means

df %>%
  group_by(daynumber) %>%
  summarise(mean = mean(EnrollmentatCancellation, na.rm = T), n = n(),)

aovdaynumber <- aov(EnrollmentatCancellation ~ daynumber, data = df)
summary(aovdaynumber)


df %>%
  group_by(daynumber) %>%
  summarise(mean = mean(EnrolledOtherSections, na.rm = T), n = n(),)

aovdayenroll <- aov(EnrolledOtherSections ~ daynumber, data = df)
summary(aovdayenroll)

