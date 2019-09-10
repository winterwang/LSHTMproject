daysOfDiary <- CW3CB3_7regss_withCHOdetail %>% 
  left_join(NDAYS, by = "ID")

epiDisplay::tabpct(daysOfDiary$Ndays.y, daysOfDiary$SEX)
