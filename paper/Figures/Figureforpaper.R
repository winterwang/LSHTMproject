library(readr)
library(tidyverse)

CW2CB2 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/50NDNS_CW2CB2.txt",
                      col_names = FALSE)

names(CW2CB2) <- c("H0",
                   "H1",
                   "H2",
                   "H3",
                   "H4",
                   "H5",
                   "H6",
                   "H7",
                   "H8",
                   "H9",
                   "H10",
                   "H11",
                   "H12",
                   "H13",
                   "H14",
                   "H15",
                   "H16",
                   "H17",
                   "H18",
                   "H19",
                   "H20",
                   "H21",
                   "H22",
                   "H23",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   # "CPROB5",
                   # "CPROB6",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")

pp <- CW2CB2 %>% 
  group_by(CW) %>% 
  summarise(H0_0 = sum(H0 == 0)/length(H0), 
            H0_1 = sum(H0 == 1)/length(H0), 
            H0_2 = sum(H0 == 2)/length(H0),
            H1_0 = sum(H1 == 0)/length(H1),
            H1_1 = sum(H1 == 1)/length(H1), 
            H1_2 = sum(H1 == 2)/length(H1),
            H2_0 = sum(H2 == 0)/length(H2),
            H2_1 = sum(H2 == 1)/length(H2), 
            H2_2 = sum(H2 == 2)/length(H2),
            H3_0 = sum(H3 == 0)/length(H3),
            H3_1 = sum(H3 == 1)/length(H3), 
            H3_2 = sum(H3 == 2)/length(H3),
            H4_0 = sum(H4 == 0)/length(H4),
            H4_1 = sum(H4 == 1)/length(H4), 
            H4_2 = sum(H4 == 2)/length(H4),
            H5_0 = sum(H5 == 0)/length(H5),
            H5_1 = sum(H5 == 1)/length(H5), 
            H5_2 = sum(H5 == 2)/length(H5),
            H6_0 = sum(H6 == 0)/length(H6),
            H6_1 = sum(H6 == 1)/length(H6), 
            H6_2 = sum(H6 == 2)/length(H6),
            H7_0 = sum(H7 == 0)/length(H7),
            H7_1 = sum(H7 == 1)/length(H7), 
            H7_2 = sum(H7 == 2)/length(H7),
            H8_0 = sum(H8 == 0)/length(H8),
            H8_1 = sum(H8 == 1)/length(H8), 
            H8_2 = sum(H8 == 2)/length(H8),
            H9_0 = sum(H9 == 0)/length(H9),
            H9_1 = sum(H9 == 1)/length(H9), 
            H9_2 = sum(H9 == 2)/length(H9),
            H10_0 = sum(H10 == 0)/length(H10),
            H10_1 = sum(H10 == 1)/length(H10), 
            H10_2 = sum(H10 == 2)/length(H10),
            H11_0 = sum(H11 == 0)/length(H11),
            H11_1 = sum(H11 == 1)/length(H11), 
            H11_2 = sum(H11 == 2)/length(H11),
            H12_0 = sum(H12 == 0)/length(H12),
            H12_1 = sum(H12 == 1)/length(H12), 
            H12_2 = sum(H12 == 2)/length(H12),
            H13_0 = sum(H13 == 0)/length(H13),
            H13_1 = sum(H13 == 1)/length(H13), 
            H13_2 = sum(H13 == 2)/length(H13),
            H14_0 = sum(H14 == 0)/length(H14),
            H14_1 = sum(H14 == 1)/length(H14), 
            H14_2 = sum(H14 == 2)/length(H14),
            H15_0 = sum(H15 == 0)/length(H15),
            H15_1 = sum(H15 == 1)/length(H15), 
            H15_2 = sum(H15 == 2)/length(H15),
            H16_0 = sum(H16 == 0)/length(H16),
            H16_1 = sum(H16 == 1)/length(H16), 
            H16_2 = sum(H16 == 2)/length(H16),
            H17_0 = sum(H17 == 0)/length(H17),
            H17_1 = sum(H17 == 1)/length(H17), 
            H17_2 = sum(H17 == 2)/length(H17),
            H18_0 = sum(H18 == 0)/length(H18),
            H18_1 = sum(H18 == 1)/length(H18), 
            H18_2 = sum(H18 == 2)/length(H18),
            H19_0 = sum(H19 == 0)/length(H19),
            H19_1 = sum(H19 == 1)/length(H19), 
            H19_2 = sum(H19 == 2)/length(H19),
            H20_0 = sum(H20 == 0)/length(H20),
            H20_1 = sum(H20 == 1)/length(H20), 
            H20_2 = sum(H20 == 2)/length(H20),
            H21_0 = sum(H21 == 0)/length(H21),
            H21_1 = sum(H21 == 1)/length(H21), 
            H21_2 = sum(H21 == 2)/length(H21),
            H22_0 = sum(H22 == 0)/length(H22),
            H22_1 = sum(H22 == 1)/length(H22), 
            H22_2 = sum(H22 == 2)/length(H22),
            H23_0 = sum(H23 == 0)/length(H23),
            H23_1 = sum(H23 == 1)/length(H23), 
            H23_2 = sum(H23 == 2)/length(H23))



pp_long <- pp %>% 
  gather(Hour, Prob, -CW) %>% 
  separate(Hour, into = c("HourN", "Carbo"), sep = "_") 




pp_long$HourN <- factor(pp_long$HourN, levels = c("H0","H1" ,"H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10","H11","H12","H13","H14","H15","H16","H17","H18","H19","H20","H21","H22","H23"))


library(ggthemr)
ggthemr("greyscale", layout = "scientific")
library(ggplot2)


cls1 <- ggplot(pp_long[pp_long$CW == 1, ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point(size = 3) + 
  geom_line(size = 1.5) + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        legend.position = "none",
        legend.direction = "horizontal"
  ) + 
  labs(title = "Class 1 days: Lunch at 1 pm high carb breakfast and at night.", x = "Hour of the day", y = "Probability",
       color = "Carbohydrate\nintake") + 
  scale_color_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1))



cls2 <- ggplot(pp_long[pp_long$CW == 2, ], aes(y = Prob, x=HourN, group = Carbo, color = Carbo)) + 
  geom_point(size = 3) + 
  geom_line(size = 1.5) + 
  theme(axis.title = element_text(size = 18), 
        axis.text = element_text(size = 18), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15), 
        legend.position = "bottom",
        legend.direction = "horizontal"
  ) + 
  labs(title = "Class 2 days: Lunch at noon lower probability of eating.", x = "Hour of the day", y = "Probability",
       color = "Carbohydrate contribution to energy intake") + 
  scale_color_discrete(labels = c("Not eating at all", "< 50%", ">= 50%")) + 
  ylim(c(0,1))


library(cowplot)
plot_grid(cls1, cls2, ncol = 1, labels = c('A', 'B'), rel_heights=c(1,1.2))




# diagram for LCGA --------------------------------------------------------
library(DiagrammeR)

grViz("
      digraph boxes_and_circles {
      
      # a 'graph' statement
      graph [overlap = true, fontsize = 8]
      
      # several 'node' statements
      node [fillcolor = grey, style = filled, 
      fontname = Helvetica, shape = circle]
      A [label = 'g'] 
      B [label = 'q']
      C [label = 'c']
      D [label = 'i']
      E [label = 'l']
      

      
      node [shape = box, 
      fillcolor = white, 
      fontname = Helvetica]
      F [label = 'u96']
      G [label = 'u1']
      H [label = 'u2']
      I [label = 'u3']
      J [label = '...']
      
      
      # edge statements
      A->B A->C A->D A->E   B->F C->F D->F E->F 
      B->G C->G D->G E->G
      B->H C->H D->H E->H
      B->I C->I D->I E->I
      B->J C->J D->J E->J
      }")

# 
# grViz("
# digraph {
#       
#       graph [ranksep = 1]
#       'i' [shape = 'circle']
#       's' [shape = 'circle']
#       't1' [shape = 'square']
#       't2' [shape = 'square']
#       't3' [shape = 'square']
#       't4' [shape = 'square']
#       'x1' [shape = 'square']
#       'x2' [shape = 'square']
#       'c1' [shape = 'square']
#       'c2' [shape = 'square']
#       'c3' [shape = 'square']
#       'c4' [shape = 'square']
#       'i'->'t1' [style = 'dashed', label = '1']
#       'i'->'t2' [style = 'dashed', label = '1']
#       'i'->'t3' [style = 'dashed', label = '1']
#       'i'->'t4' [style = 'dashed', label = '1']
#       's'->'t1' [style = 'dashed', label = '0']
#       's'->'t2' [style = 'dashed', label = '1']
#       's'->'t3' [style = 'dashed', label = '2']
#       's'->'t4' [style = 'dashed', label = '3']
#       'x1'->'i' [style = 'solid', label = '0.61']
#       'x2'->'i' [style = 'solid', label = '0.6']
#       'x1'->'s' [style = 'solid', label = '0.26']
#       'x2'->'s' [style = 'solid', label = '0.52']
#       'c1'->'t1' [style = 'solid', label = '0.14']
#       'c2'->'t2' [style = 'solid', label = '0.29']
#       'c3'->'t3' [style = 'solid', label = '0.33']
#       'c4'->'t4' [style = 'solid', label = '0.33']
#       
#       # Additional constraints on the graph
#       t1->t2->t3->t4 [style='invis']
#       {rank = 'max'; c1; c2; c3; c4;}
#       {rank = 'same'; t1; t2; t3; t4;}
#       }
#       ")