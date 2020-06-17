#NPS

library(tidyverse)

#---set dataframe

df <- Q3_2019_Partner_facilitator_NPS_Survey_Responses_

tenscore <- df$`On a scale of 0-10 how willing are you to recommend us to a friend or colleague?`

as.character(tenscore)

#recoding
tenscore <- recode(tenscore, "10 - Definitely would" = "10")

#vector needs to be numeric
tenscore <- as.numeric(tenscore)

df <- df %>% mutate(promoter = case_when(tenscore == 10 ~ 1,
                                         tenscore == 9 ~ 1,
                                         tenscore == 8 ~ 0,
                                         tenscore == 7 ~ 0,
                                         TRUE ~ 2))


promoters <- (sum(df$promoter == 1)) / (nrow(df))
detractors <- (sum(df$promoter == 2)) / (nrow(df))
  
NetPromoterScore <- ((promoters - detractors) * 100)
NetPromoterScore

table(df$promoter)


