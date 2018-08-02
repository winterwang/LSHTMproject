CW3CB2_CW3 <- CW3CB2 %>% 
  select(ID_DAY, CW, CB) %>% 
  rename(CW3_2 = CW, CB2_2 = CB)

CW3CB3 <- CW3CB3 %>% 
  left_join(CW3CB2_CW3, by = "ID_DAY")


with(CW3CB3, tabpct(CB, CW))
with(CW3CB3, tabpct(CB2_2, CW3_2))
with(CW3CB3, tabpct(CB, CW3_2))



