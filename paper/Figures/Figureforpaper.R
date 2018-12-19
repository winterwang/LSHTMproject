library(readr)
library(tidyverse)

# CW2CB2 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/50NDNS_CW2CB2.txt",
#                       col_names = FALSE)

CW2CB2 <- read_table2("results/50NDNS_CW2CB2.txt",
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

# Time slots level 1 classes ----------------------------------------------
library(readr)
library(tidyverse)


CW3CB2 <- read_table2("results/Timeslots/NDNSslot_CW3CB2.txt",
                      col_names = FALSE)

names(CW3CB2) <- c("Breakfast", 
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   "CPROB5",
                   "CPROB6",
                   # "CPROB7",
                   # "CPROB8",
                   # "CPROB9",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")

pp <- CW3CB2 %>% 
  group_by(CW) %>% 
  summarise(Breakfast_0 = sum(Breakfast == 0)/length(Breakfast), 
            Breakfast_1 = sum(Breakfast == 1)/length(Breakfast), 
            Breakfast_2 = sum(Breakfast == 2)/length(Breakfast),
            Morning.snack_0 = sum(Morning.snack == 0)/length(Morning.snack),
            Morning.snack_1 = sum(Morning.snack == 1)/length(Morning.snack), 
            Morning.snack_2 = sum(Morning.snack == 2)/length(Morning.snack),
            Lunch_0 = sum(Lunch == 0)/length(Lunch),
            Lunch_1 = sum(Lunch == 1)/length(Lunch), 
            Lunch_2 = sum(Lunch == 2)/length(Lunch),
            Afternoon.snack_0 = sum(Afternoon.snack == 0)/length(Afternoon.snack),
            Afternoon.snack_1 = sum(Afternoon.snack == 1)/length(Afternoon.snack), 
            Afternoon.snack_2 = sum(Afternoon.snack == 2)/length(Afternoon.snack),
            Dinner_0 = sum(Dinner == 0)/length(Dinner),
            Dinner_1 = sum(Dinner == 1)/length(Dinner), 
            Dinner_2 = sum(Dinner == 2)/length(Dinner),
            Before.bedtime.snack_0 = sum(Before.bedtime.snack == 0)/length(Before.bedtime.snack),
            Before.bedtime.snack_1 = sum(Before.bedtime.snack == 1)/length(Before.bedtime.snack), 
            Before.bedtime.snack_2 = sum(Before.bedtime.snack == 2)/length(Before.bedtime.snack),
            Midnight.food_0 = sum(Midnight.food == 0)/length(Midnight.food),
            Midnight.food_1 = sum(Midnight.food == 1)/length(Midnight.food), 
            Midnight.food_2 = sum(Midnight.food == 2)/length(Midnight.food))



pp_long <- pp %>% 
  gather(Hour, Prob, -CW) %>% 
  separate(Hour, into = c("Slots", "Carbo"), sep = "_") 




pp_long$Slots <- factor(pp_long$Slots, levels = c("Breakfast", 
                                                  "Morning.snack",
                                                  "Lunch",
                                                  "Afternoon.snack",
                                                  "Dinner",
                                                  "Before.bedtime.snack",
                                                  "Midnight.food"))


library(ggthemr)
ggthemr("greyscale", layout = "scientific")
library(ggplot2)


pp_long$Time <- as.character(pp_long$Slots)
pp_long$Time[pp_long$Slots == "Breakfast"] <- 7.5
pp_long$Time[pp_long$Slots == "Morning.snack"] <- 10.5
pp_long$Time[pp_long$Slots == "Lunch"] <- 13
pp_long$Time[pp_long$Slots == "Afternoon.snack"] <- 15.5
pp_long$Time[pp_long$Slots == "Dinner"] <- 18.5
pp_long$Time[pp_long$Slots == "Before.bedtime.snack"] <- 21
pp_long$Time[pp_long$Slots == "Midnight.food"] <- 26
pp_long$Time <- as.numeric(pp_long$Time)


cls1 <- ggplot(pp_long[pp_long$CW == 1, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo))  + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  scale_shape_manual(values=c(21,23,24)) + 
  theme(axis.title = element_text(size = 15, face = "bold"), 
        axis.text = element_text(size = 13, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        # legend.text = element_text(size = 10, face = "bold"), 
        # legend.title = element_text(size = 10, face = "bold"),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        legend.position = "none",
        plot.title=element_text(size = 17, 
                                face = "bold", hjust = 0.5),
        axis.text.x=element_blank()#,
        # legend.position = "bottom",
        # legend.direction = "horizontal"
  ) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 1 days - High percentage carbohydrate day (39.5%)", x = "", 
       y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))

cls2 <- ggplot(pp_long[pp_long$CW == 2, ], aes(y = Prob, x=Time, group = Carbo,
                                               linetype = Carbo)) + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  scale_shape_manual(values=c(21,23,24)) + 
  theme(axis.title = element_text(size = 15, face = "bold"), 
        axis.text = element_text(size = 13, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        # legend.text = element_text(size = 10, face = "bold"), 
        # legend.title = element_text(size = 10, face = "bold"),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        legend.position = "none",
        plot.title=element_text(size = 17, 
                                face = "bold", hjust = 0.5),
        axis.text.x=element_blank()#,
        # legend.position = "bottom",
        # legend.direction = "horizontal"
  ) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 2 days - Low percentage carbohydrate day (20.4%)", x = "", 
       y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))





cls3 <- ggplot(pp_long[pp_long$CW == 3, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo)) + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  theme(axis.title = element_text(size = 15, face = "bold"), 
        axis.text = element_text(size = 13, face = "bold"), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        # legend.position = "none",
        plot.title=element_text(size = 17, 
                                face = "bold", hjust = 0.5),
        # axis.text.x=element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal"
  ) +
  scale_shape_manual(values=c(21,23,24), 
                     labels =  c("No energy intake", "Carbohydrate < 50%", "Carbohydrate >= 50%")) + 
      scale_linetype_manual(values=c("dotted", "dashed", "solid"),
                            labels =  c("No energy intake", "Carbohydrate < 50%", "Carbohydrate >= 50%")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 3 days - Regular meals day (40.1%)", y = "Probability") + 
  labs(shape="Responses to\ncarbohydrate intake", 
       linetype = "Responses to\ncarbohydrate intake")+
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) +
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam")) +
theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(family = "Atlas Grotesk Medium", 
        size = 10, face = "bold", colour = "gray24", 
        hjust = 0, vjust = 1)) +
 labs(x = "Hours of the day", 
      caption = "Note:
Grey, and white shades indicate the 7 time slots;
Carbohydrate (CH) < 50% indicates CH contributed less than 50% total energy intake;
Carbohydrate >= 50% indicates CH contributed higher or equal to 50% total
      energy intake.")


library(cowplot)
cls1 <- cls1 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
cls2 <- cls2 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
# plot_grid(cls1, cls2, rel_heights = c(1,1), ncol = 1)

# dev.copy2pdf(file="../gemini/Fig/Fig01a.pdf",out.type="cairo", width=7.0, height=6.10)

cls3 <- cls3 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# cls3
plot_grid(cls1, cls2, cls3, ncol = 1, labels = c('A', 'B', 'C'), rel_heights=c(1,1,1.8))


# producing graph for the poster ------------------------------------------

cls1 <- cls1 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
cls2 <- cls2 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
# plot_grid(cls1, cls2, rel_heights = c(1,1), ncol = 1)

# dev.copy2pdf(file="../gemini/Fig/Fig01a.pdf",out.type="cairo", width=7.0, height=6.10)

cls3 <- cls3 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# cls3
plot_grid(cls1, cls2, cls3, ncol = 1, labels = c('I', 'II', 'III'), rel_heights=c(1,1,1.35))

dev.copy2pdf(file="../gemini/Fig/Fig01.pdf",out.type="cairo", width=7.25, height=8)

# Level 2 (CB=3) person classes distribution -------------------------------------
library(plyr)

library(scales)

CW3CB3 <- read_table2("results/Timeslots/NDNSslot_CW3CB3.txt",
                      col_names = FALSE)

names(CW3CB3) <- c("Breakfast", 
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   "CPROB5",
                   "CPROB6",
                   "CPROB7",
                   "CPROB8",
                   "CPROB9",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")




chart.data <- CW3CB3 %>% 
  group_by(CB, CW) %>% 
  tally %>% 
  group_by(CB) %>% 
  mutate(pct = n/sum(n))


chart.data$CW_new <- 0
chart.data$CW_new[chart.data$CW == 1] <- 3
chart.data$CW_new[chart.data$CW == 2] <- 1
chart.data$CW_new[chart.data$CW == 3] <- 2


chart.data <- chart.data[order(chart.data$CB, chart.data$CW_new),]
chart.data <- ddply(chart.data, .(CB), 
                    transform, pos = cumsum(pct) - (0.5 * pct)) 



chart.data$CW_new <- factor(chart.data$CW_new, levels = c("3", "2", "1"), 
                            labels = c("Regular\nmeals day", "Low % CH day", "High % CH day"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3"), 
                        labels = c("Low CH\n eaters\n(28.1%)",  
                                   "Moderate CH\n eaters\n(28.8%)", 
                                   "High CH\n eaters\n(43.1%)"))



library(ggthemr)
# ggthemr("dust", layout = "scientific")
# ggthemr("fresh", layout = "scientific")
ggthemr("greyscale", layout = "scientific")

ggplot() + 
  geom_bar(aes(y = pct, x = CB, fill = CW_new), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, 
            aes(x = CB, y = pos, fontface = "bold",
                label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 12, face = "bold"), 
        axis.title = element_text(size = 18, face = "bold"),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual Classes", y = "Percentage") +
  scale_y_continuous(labels=percent)


# producing graph for the poster ------------------------------------------



dev.copy2pdf(file="../gemini/Fig/level2.pdf",out.type="cairo", width=7.28, height=4.85)


# Level 2 (CB=2) classes solution  ----------------------------------------


CW3CB2 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW3CB2.txt",
                      col_names = FALSE)

names(CW3CB2) <- c("Breakfast", "Morning.snack", "Lunch",
                   "Afternoon.snack", "Dinner", "Before.bedtime.snack",
                   "Midnight.food", "ID_DAY", "AGE", "SEX",
                   "CPROB1", "CPROB2", "CPROB3", "CPROB4",
                   "CPROB5", "CPROB6",
                   # "CPROB7",
                   # "CPROB8",
                   # "CPROB9",
                   "CB", "CW", "MLCJOINT", "ID")


CW3CB3 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW3CB3.txt",
                      col_names = FALSE)

names(CW3CB3) <- c("Breakfast",  "Morning.snack", "Lunch",
                   "Afternoon.snack", "Dinner", "Before.bedtime.snack",
                   "Midnight.food", "ID_DAY", "AGE", "SEX",
                   "CPROB1", "CPROB2", "CPROB3", "CPROB4",
                   "CPROB5", "CPROB6", "CPROB7", "CPROB8",
                   "CPROB9", "CB", "CW", "MLCJOINT", "ID")

# tab1(CW3CB2$CW)
# tab1(CW3CB3$CW)

CW3CB3$CW_new <- 0
CW3CB3$CW_new[CW3CB3$CW == 1] <- 3
CW3CB3$CW_new[CW3CB3$CW == 2] <- 1
CW3CB3$CW_new[CW3CB3$CW == 3] <- 2

CB2_in_CW3CB2 <- CW3CB2 %>% 
  select(ID, ID_DAY, CW, CB) %>% 
  rename(CW3_in_2 = CW, CB2_in_2 = CB)
CW3CB3 <- CW3CB3 %>% 
  left_join(CB2_in_CW3CB2, by = "ID_DAY")


chart.data <- CW3CB3 %>%
  group_by(CB2_in_2, CW_new) %>%
  tally %>%
  group_by(CB2_in_2) %>%
  mutate(pct = n/sum(n))

chart.data <- chart.data[order(chart.data$CB2_in_2, chart.data$CW_new),]


chart.data <- ddply(chart.data, .(CB2_in_2),
                    transform, pos = cumsum(pct) - (0.5 * pct))

chart.data$CW_new <- factor(chart.data$CW_new, levels = c("3", "2", "1"),
                            labels = c("Regular\nmeals day", "Low carbo-\nhydrate day", "High carbo-\nhydrate day"))
chart.data$CB2_in_2 <- factor(chart.data$CB2_in_2, levels = c("1", "2"),
                              labels = c("Individual Class 1\n(35.3%)",  "Individual class 2\n(64.7%)"))



library(ggthemr)
ggthemr("greyscale", layout = "scientific")
ggplot() +
  geom_bar(aes(y = pct, x = CB2_in_2, fill = CW_new), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, aes(x = CB2_in_2, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)



# Level 2 (CB=4) classes solution  ----------------------------------------


CW3CB2 <- read_table2("results/Timeslots/NDNSslot_CW3CB2.txt",
                      col_names = FALSE)

names(CW3CB2) <- c("Breakfast", "Morning.snack", "Lunch",
                   "Afternoon.snack", "Dinner", "Before.bedtime.snack",
                   "Midnight.food", "ID_DAY", "AGE", "SEX",
                   "CPROB1", "CPROB2", "CPROB3", "CPROB4",
                   "CPROB5", "CPROB6",
                   # "CPROB7",
                   # "CPROB8",
                   # "CPROB9",
                   "CB", "CW", "MLCJOINT", "ID")


CW3CB4 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW3CB4.txt",
                      col_names = FALSE)

names(CW3CB4) <- c("Breakfast",  "Morning.snack", "Lunch",
                   "Afternoon.snack", "Dinner", "Before.bedtime.snack",
                   "Midnight.food", "ID_DAY", "AGE", "SEX",
                   "CPROB1", "CPROB2", "CPROB3", "CPROB4",
                   "CPROB5", "CPROB6", "CPROB7", "CPROB8",
                   "CPROB9", "CPROB10", "CPROB11", "CPROB12", 
                   "CB", "CW", "MLCJOINT", "ID")

# tab1(CW3CB2$CW)
# tab1(CW3CB3$CW)
tab1(CW3CB4$CW)


CW3CB4$CW_new <- 0
CW3CB4$CW_new[CW3CB4$CW == 1] <- 1
CW3CB4$CW_new[CW3CB4$CW == 2] <- 3
CW3CB4$CW_new[CW3CB4$CW == 3] <- 2
# 
# CB2_in_CW3CB2 <- CW3CB2 %>% 
#   select(ID, ID_DAY, CW, CB) %>% 
#   rename(CW3_in_2 = CW, CB2_in_2 = CB)
# CW3CB3 <- CW3CB3 %>% 
#   left_join(CB2_in_CW3CB2, by = "ID_DAY")


chart.data <- CW3CB4 %>%
  group_by(CB, CW_new) %>%
  tally %>%
  group_by(CB) %>%
  mutate(pct = n/sum(n))

chart.data <- chart.data[order(chart.data$CB, chart.data$CW_new),]


chart.data <- ddply(chart.data, .(CB),
                    transform, pos = cumsum(pct) - (0.5 * pct))

chart.data$CW_new <- factor(chart.data$CW_new, levels = c("3", "2", "1"),
                            labels = c("Regular\nmeals day", "Low carbo-\nhydrate day", "High carbo-\nhydrate day"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3", "4"),
                              labels = c("Individual\nClass 1\n(21.8%)",  
                                         "Individual\nclass 2\n(21.8%)",  
                                         "Individual\nclass 3\n(33.7%)",  
                                         "Individual\nclass 4\n(23.4%)"))



library(ggthemr)
ggthemr("greyscale", layout = "scientific")
ggplot() +
  geom_bar(aes(y = pct, x = CB, fill = CW_new), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, aes(x = CB, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)





# Sources energy by slot --------------------------------------------------
library(plyr)
library(readr)
CB1sources <- read_csv("Tablecsv/CB1sources.csv")
library(ggthemr)
ggthemr("dust", layout = "scientific")
library(scales)
CB1sources$Slot <- factor(CB1sources$Slot, levels = c("6 am – 9 am", 
                                                  "9 am – 12 am",
                                                  "12 noon – 2 pm",
                                                  "2 pm – 5 pm",
                                                  "5 pm – 8 pm",
                                                  "8 pm – 10 pm",
                                                  "10 pm – 6 am"))

CB1sources$Sources <- factor(CB1sources$Sources, levels = c("Alc", 
                                                            "Protein",
                                                            "Fat", 
                                                            "Carbohydrate"), 
                             labels = c( "Alcohol", "Protein",  "Fat", "Carbo"))



CB1sources <- CB1sources[order(CB1sources$Slot, CB1sources$Sources),]
CB1sources <- ddply(CB1sources, .(Slot), 
                    transform, pos = cumsum(pct) - (0.5 * pct)) 
CB1sources$pos[1] <- -0.03
CB1sources$pos[5] <- -0.03
CB1sources$pos[9] <- -0.03
# CB1sources$pos[13] <- -0.03

CB1sources$Sources <- factor(CB1sources$Sources,
                             levels = c("Carbo", "Fat", "Protein",  "Alcohol"))

Text <- data.frame(levels(CB1sources$Slot))
names(Text)[1] <- "Slot"
Text$TotalEner <-  c("753.1 kJ", "854.6 kJ", "1622.1 kJ", "950.8 kJ", 
                     "2328.6 kJ","1063.5 kJ", "422.4 kJ")
Text$pos <- rep(1.03, 7)
CB1 <- ggplot() +
  geom_bar(aes(y = pct, x = Slot, fill = Sources), data = CB1sources, width = 0.6,
           stat="identity") +
  geom_text(data=CB1sources, aes(x = Slot, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
  size=4, colour="black", family="Atlas Grotesk Medium") +
  geom_text(data= Text, aes(x = Slot, y = pos, label = TotalEner), 
            size = 4, colour = "black", family="Atlas Grotesk Medium") +
  theme(#legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light"),
        legend.position = "none", 
        axis.text.x=element_blank()) + 
        # legend.position = "bottom", 
        # legend.direction = "horizontal") +
  labs(title = "Low carbohydrate eaters (28.1%) [high fat and drinking at night]", x = " ", y = "Percentage") +
  # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels=percent)




CB2sources <- read_csv("Tablecsv/CB2sources.csv")


CB2sources$Slot <- factor(CB2sources$Slot, levels = c("6 am – 9 am", 
                                                      "9 am – 12 am",
                                                      "12 noon – 2 pm",
                                                      "2 pm – 5 pm",
                                                      "5 pm – 8 pm",
                                                      "8 pm – 10 pm",
                                                      "10 pm – 6 am"))

CB2sources$Sources <- factor(CB2sources$Sources, levels = c("Alc", 
                                                            "Protein",
                                                            "Fat", 
                                                            "Carbohydrate"), 
                             labels = c( "Alcohol", "Protein",  "Fat", "Carbo"))



CB2sources <- CB2sources[order(CB2sources$Slot, CB2sources$Sources),]
CB2sources <- ddply(CB2sources, .(Slot), 
                    transform, pos = cumsum(pct) - (0.5 * pct)) 

CB2sources$Sources <- factor(CB2sources$Sources,
                             levels = c("Carbo", "Fat", "Protein",  "Alcohol"))

Text <- data.frame(levels(CB2sources$Slot))
names(Text)[1] <- "Slot"
Text$TotalEner <-  c("296.9 kJ", "967.9 kJ", 
                     "1310.9 kJ", "1106.1 kJ", "1977.2 kJ", 
                     "1086.9 kJ",  "599.7 kJ")
Text$pos <- rep(1.03, 7)
CB2sources$pos[1] <- -0.03
CB2sources$pos[5] <- -0.03
CB2sources$pos[9] <- -0.03
CB2sources$pos[13] <- -0.03
CB2sources$pos[17] <- -0.03

CB2 <- ggplot() +
  geom_bar(aes(y = pct, x = Slot, fill = Sources), data = CB2sources, width = 0.6,
           stat="identity") +
  geom_text(data=CB2sources, aes(x = Slot, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="black", family="Atlas Grotesk Medium") +
  geom_text(data= Text, aes(x = Slot, y = pos, label = TotalEner), 
            size = 4, colour = "black", family="Atlas Grotesk Medium") +
  theme(#legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light"),
        legend.position = "none", 
        axis.text.x=element_blank()) +
# legend.position = "bottom", 
# legend.direction = "horizontal") +
labs(title = "Moderate carbohydrate eaters (28.8%) [eating late]", x = " ", y = "Percentage") +
  # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels=percent)




CB3sources <- read_csv("Tablecsv/CB3sources.csv")


CB3sources$Slot <- factor(CB3sources$Slot, levels = c("6 am – 9 am", 
                                                      "9 am – 12 am",
                                                      "12 noon – 2 pm",
                                                      "2 pm – 5 pm",
                                                      "5 pm – 8 pm",
                                                      "8 pm – 10 pm",
                                                      "10 pm – 6 am"))

CB3sources$Sources <- factor(CB3sources$Sources, levels = c("Alc", 
                                                            "Protein",
                                                            "Fat", 
                                                            "Carbohydrate"), 
                             labels = c( "Alcohol", "Protein",  "Fat", "Carbohydrate"))



CB3sources <- CB3sources[order(CB3sources$Slot, CB3sources$Sources),]
CB3sources <- ddply(CB3sources, .(Slot), 
                    transform, pos = cumsum(pct) - (0.5 * pct)) 

CB3sources$Sources <- factor(CB3sources$Sources,
                             levels = c("Carbohydrate", "Fat", "Protein",  "Alcohol"))

Text <- data.frame(levels(CB3sources$Slot))
names(Text)[1] <- "Slot"
Text$TotalEner <-  c("929.0 kJ", "746.4 kJ",  "1788.8 kJ",  "785.0 kJ", 
                     "2368.9 kJ", "860.2 kJ", "205.5 kJ")
Text$pos <- rep(1.03, 7)
CB3sources$pos[1] <- -0.03
CB3sources$pos[5] <- -0.03
CB3sources$pos[9] <- -0.03
CB3sources$pos[13] <- -0.03
CB3sources$pos[17] <- -0.03


CB3 <- ggplot() +
  geom_bar(aes(y = pct, x = Slot, fill = Sources), data = CB3sources, width = 0.6,
           stat="identity") +
  geom_text(data=CB3sources, aes(x = Slot, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="black", family="Atlas Grotesk Medium") +
  geom_text(data= Text, aes(x = Slot, y = pos, label = TotalEner), 
            size = 4, colour = "black", family="Atlas Grotesk Medium") +
  theme(legend.direction="horizontal",
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  scale_fill_manual(values = c("#db735c", "#EFA86E", "#9A8A76", "#F3C57B"),
    labels =  c("Carbo-\nhydrate","Fat", "Protein",  "Alcohol")) + 
  labs(title = "High carbohydrate eaters (43.1%) [eating early]", x = "Hours of the day", y = "Percentage") +
  theme(axis.text.x = element_text(angle = 14, hjust = 1)) +
  scale_y_continuous(labels=percent)


library(cowplot)


plot_grid(CB1, CB2, CB3, ncol = 1, labels = c('A', 'B', 'C'), rel_heights=c(1,1,1.38))





# prow <- plot_grid( CB1, CB2, 
#   CB3 + theme(legend.position="none"),
#   labels = c("A", "B", "C"),
#   hjust = -1,
#   nrow = 3, rel_heights=c(1,1,1.6))
# 
# legend <- get_legend(CB3 + theme(legend.position="bottom"))
# p <- plot_grid(prow, legend, ncol = 1)
# p
# prow


# Other solutions to MLCA -------------------------------------------------


# (CW = 2) Level 1 latent classes

## Visualisation of level 1 latent classes
library(readr)
library(tidyverse)
CW2CB2 <- read_table2("../LSHTMproject/results/Timeslots/NDNSslot_CW2CB2.txt",
                      col_names = FALSE)

names(CW2CB2) <- c("Breakfast",
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
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
  summarise(Breakfast_0 = sum(Breakfast == 0)/length(Breakfast),
            Breakfast_1 = sum(Breakfast == 1)/length(Breakfast),
            Breakfast_2 = sum(Breakfast == 2)/length(Breakfast),
            Morning.snack_0 = sum(Morning.snack == 0)/length(Morning.snack),
            Morning.snack_1 = sum(Morning.snack == 1)/length(Morning.snack),
            Morning.snack_2 = sum(Morning.snack == 2)/length(Morning.snack),
            Lunch_0 = sum(Lunch == 0)/length(Lunch),
            Lunch_1 = sum(Lunch == 1)/length(Lunch),
            Lunch_2 = sum(Lunch == 2)/length(Lunch),
            Afternoon.snack_0 = sum(Afternoon.snack == 0)/length(Afternoon.snack),
            Afternoon.snack_1 = sum(Afternoon.snack == 1)/length(Afternoon.snack),
            Afternoon.snack_2 = sum(Afternoon.snack == 2)/length(Afternoon.snack),
            Dinner_0 = sum(Dinner == 0)/length(Dinner),
            Dinner_1 = sum(Dinner == 1)/length(Dinner),
            Dinner_2 = sum(Dinner == 2)/length(Dinner),
            Before.bedtime.snack_0 = sum(Before.bedtime.snack == 0)/length(Before.bedtime.snack),
            Before.bedtime.snack_1 = sum(Before.bedtime.snack == 1)/length(Before.bedtime.snack),
            Before.bedtime.snack_2 = sum(Before.bedtime.snack == 2)/length(Before.bedtime.snack),
            Midnight.food_0 = sum(Midnight.food == 0)/length(Midnight.food),
            Midnight.food_1 = sum(Midnight.food == 1)/length(Midnight.food),
            Midnight.food_2 = sum(Midnight.food == 2)/length(Midnight.food))



pp_long <- pp %>%
  gather(Hour, Prob, -CW) %>%
  separate(Hour, into = c("Slots", "Carbo"), sep = "_")




pp_long$Slots <- factor(pp_long$Slots, levels = c("Breakfast",
                                                  "Morning.snack",
                                                  "Lunch",
                                                  "Afternoon.snack",
                                                  "Dinner",
                                                  "Before.bedtime.snack",
                                                  "Midnight.food"))



library(ggthemr)
ggthemr("fresh", layout = "scientific")
library(ggplot2)

pp_long$Time <- as.character(pp_long$Slots)
pp_long$Time[pp_long$Slots == "Breakfast"] <- 7.5
pp_long$Time[pp_long$Slots == "Morning.snack"] <- 10.5
pp_long$Time[pp_long$Slots == "Lunch"] <- 13
pp_long$Time[pp_long$Slots == "Afternoon.snack"] <- 15.5
pp_long$Time[pp_long$Slots == "Dinner"] <- 18.5
pp_long$Time[pp_long$Slots == "Before.bedtime.snack"] <- 21
pp_long$Time[pp_long$Slots == "Midnight.food"] <- 26
pp_long$Time <- as.numeric(pp_long$Time)


cls1 <- ggplot(pp_long[pp_long$CW == 1, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo))  + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  scale_shape_manual(values=c(21,23,24)) + 
  theme(axis.text = element_text(size = 15), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        legend.position = "none"
        # legend.position = "bottom", 
        # legend.direction = "horizontal"
  ) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 1 days  (37.1%)", x = "", y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))

cls2 <- ggplot(pp_long[pp_long$CW == 2, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo)) + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        # legend.position = "none"
        legend.position = "bottom",
        legend.direction = "horizontal"
  ) + 
  scale_shape_manual(values=c(21,23,24), 
                     labels =  c("Not eating\nany food", "Carbohydrate < 50%", "Carbohydrate >= 50%")) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid"),
                        labels =  c("Not eating\nany food", "Carbohydrate < 50%", "Carbohydrate >= 50%")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 2 days (62.9%)", y = "Probability") + 
  labs(shape="Responses to\ncarbohydrate intake", 
       linetype = "Responses to\ncarbohydrate intake")+
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) +
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam")) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(family = "Atlas Grotesk Medium", 
                                    size = 10, face = "bold", colour = "gray24", 
                                    hjust = 0, vjust = 1)) +
  labs(x = "Hours of the day", caption = "Note: 
Grey, and white shades indicate the 7 time slots;
Carbohydrate < 50% indicates that within the time slot, carbohydrate contributed less than 50% total energy intake; 
Carbohydrate >= 50% indicates that within the time slot, carbohydrate contributed higher or equal to 50% total 
      energy intake.")


library(cowplot)
plot_grid(cls1, cls2, ncol = 1, labels = c('A', 'B'), rel_heights=c(1,1.44))

## Visualisation of level 2 latent classes (CB = 2)
library(plyr)
library(readr)
library(tidyverse)
library(dplyr)

CW2CB2 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW2CB2.txt",
                      col_names = FALSE)

names(CW2CB2) <- c("Breakfast",
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
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

chart.data <- CW2CB2 %>%
  group_by(CB, CW) %>%
  tally %>%
  group_by(CB) %>%
  mutate(pct = n/sum(n))


chart.data <- ddply(chart.data, .(CB),
                    transform, pos = cumsum(pct) - (0.5 * pct))

chart.data$CW <- factor(chart.data$CW, levels = c("2", "1"),
                        labels = c("Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2"),
                        labels = c("Individual\nClass 1\n(56.5%)",  "Individual\nclass 2\n(43.5%)"))



library(ggthemr)
library(scales)
ggthemr("greyscale", layout = "scientific")
ggplot() +
  geom_bar(aes(y = pct, x = CB, fill = CW), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, aes(x = CB, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)


## Visualisation of level 2 latent classes (CB = 3)




CW2CB3 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW2CB3.txt",
                      col_names = FALSE)

names(CW2CB3) <- c("Breakfast",
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   "CPROB5",
                   "CPROB6",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")




chart.data <- CW2CB3 %>%
  group_by(CB, CW) %>%
  tally %>%
  group_by(CB) %>%
  mutate(pct = n/sum(n))


# chart.data$CW[chart.data$CW == 1] <- 0
# chart.data$CW[chart.data$CW == 3] <- 1
# chart.data$CW[chart.data$CW == 0] <- 3

chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
chart.data <- ddply(chart.data, .(CB),
                    transform, pos = cumsum(pct) - (0.5 * pct))



chart.data$CW <- factor(chart.data$CW, levels = c("2", "1"),
                        labels = c("Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3"),
                        labels = c("Individual\nclass 1\n(42.7%)",  "Individual\nclass 2\n(28.9%)",
                                   "Individual\nclass 3\n(28.4%)"))



library(ggthemr)
ggplot() +
  geom_bar(aes(y = pct, x = CB, fill = CW), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, aes(x = CB, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)


## Visualisation of level 2 latent classes (CB = 4)




CW2CB4 <- read_table2("../LSHTMproject/results/Timeslots/NDNSslot_CW2CB4.txt",
                      col_names = FALSE)

names(CW2CB4) <- c("Breakfast",
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   "CPROB5",
                   "CPROB6",
                   "CPROB7",
                   "CPROB8",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")

epiDisplay::tabpct(CW2CB4$CB, CW2CB4$CW)


chart.data <- CW2CB4 %>%
  group_by(CB, CW) %>%
  tally %>%
  group_by(CB) %>%
  mutate(pct = n/sum(n))


# chart.data$CW[chart.data$CW == 1] <- 0
# chart.data$CW[chart.data$CW == 3] <- 1
# chart.data$CW[chart.data$CW == 0] <- 3

chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
chart.data <- ddply(chart.data, .(CB),
                    transform, pos = cumsum(pct) - (0.5 * pct))



chart.data$CW <- factor(chart.data$CW, levels = c("2", "1"),
                        labels = c("Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3", "4"),
                        labels = c("Individual\nclass 1\n(23.3%)",  "Individual\nclass 2\n(23.3%)",
                                   "Individual\nclass 3\n(30.3%)", "Individual\nclass 4\n(23.1%)"))



library(ggthemr)
ggplot() +
  geom_bar(aes(y = pct, x = CB, fill = CW), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, aes(x = CB, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)


# CW=4 --------------------------------------------------------------------

# (CW = 2) Level 1 latent classes

## Visualisation of level 1 latent classes
library(readr)
library(tidyverse)
CW4CB2 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW4CB2.txt",
                      col_names = FALSE)

names(CW4CB2) <- c("Breakfast",
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   "CPROB5",
                   "CPROB6",
                   "CPROB7",
                   "CPROB8",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")

pp <- CW4CB2 %>%
  group_by(CW) %>%
  summarise(Breakfast_0 = sum(Breakfast == 0)/length(Breakfast),
            Breakfast_1 = sum(Breakfast == 1)/length(Breakfast),
            Breakfast_2 = sum(Breakfast == 2)/length(Breakfast),
            Morning.snack_0 = sum(Morning.snack == 0)/length(Morning.snack),
            Morning.snack_1 = sum(Morning.snack == 1)/length(Morning.snack),
            Morning.snack_2 = sum(Morning.snack == 2)/length(Morning.snack),
            Lunch_0 = sum(Lunch == 0)/length(Lunch),
            Lunch_1 = sum(Lunch == 1)/length(Lunch),
            Lunch_2 = sum(Lunch == 2)/length(Lunch),
            Afternoon.snack_0 = sum(Afternoon.snack == 0)/length(Afternoon.snack),
            Afternoon.snack_1 = sum(Afternoon.snack == 1)/length(Afternoon.snack),
            Afternoon.snack_2 = sum(Afternoon.snack == 2)/length(Afternoon.snack),
            Dinner_0 = sum(Dinner == 0)/length(Dinner),
            Dinner_1 = sum(Dinner == 1)/length(Dinner),
            Dinner_2 = sum(Dinner == 2)/length(Dinner),
            Before.bedtime.snack_0 = sum(Before.bedtime.snack == 0)/length(Before.bedtime.snack),
            Before.bedtime.snack_1 = sum(Before.bedtime.snack == 1)/length(Before.bedtime.snack),
            Before.bedtime.snack_2 = sum(Before.bedtime.snack == 2)/length(Before.bedtime.snack),
            Midnight.food_0 = sum(Midnight.food == 0)/length(Midnight.food),
            Midnight.food_1 = sum(Midnight.food == 1)/length(Midnight.food),
            Midnight.food_2 = sum(Midnight.food == 2)/length(Midnight.food))



pp_long <- pp %>%
  gather(Hour, Prob, -CW) %>%
  separate(Hour, into = c("Slots", "Carbo"), sep = "_")




pp_long$Slots <- factor(pp_long$Slots, levels = c("Breakfast",
                                                  "Morning.snack",
                                                  "Lunch",
                                                  "Afternoon.snack",
                                                  "Dinner",
                                                  "Before.bedtime.snack",
                                                  "Midnight.food"))



pp_long$Time <- as.character(pp_long$Slots)
pp_long$Time[pp_long$Slots == "Breakfast"] <- 7.5
pp_long$Time[pp_long$Slots == "Morning.snack"] <- 10.5
pp_long$Time[pp_long$Slots == "Lunch"] <- 13
pp_long$Time[pp_long$Slots == "Afternoon.snack"] <- 15.5
pp_long$Time[pp_long$Slots == "Dinner"] <- 18.5
pp_long$Time[pp_long$Slots == "Before.bedtime.snack"] <- 21
pp_long$Time[pp_long$Slots == "Midnight.food"] <- 26
pp_long$Time <- as.numeric(pp_long$Time)


cls1 <- ggplot(pp_long[pp_long$CW == 1, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo))  + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  scale_shape_manual(values=c(21,23,24)) + 
  theme(axis.text = element_text(size = 15), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        legend.position = "none"
        # legend.position = "bottom", 
        # legend.direction = "horizontal"
  ) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 1 days  (14.8%)", x = "", y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))

cls2 <- ggplot(pp_long[pp_long$CW == 2, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo))  + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  scale_shape_manual(values=c(21,23,24)) + 
  theme(axis.text = element_text(size = 15), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        legend.position = "none"
        # legend.position = "bottom", 
        # legend.direction = "horizontal"
  ) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 2 days  (30.9%)", x = "", y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))


cls3 <- ggplot(pp_long[pp_long$CW == 3, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo))  + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  scale_shape_manual(values=c(21,23,24)) + 
  theme(axis.text = element_text(size = 15), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        legend.position = "none"
        # legend.position = "bottom", 
        # legend.direction = "horizontal"
  ) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 3 days  (33.0%)", x = "", y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))


cls4 <- ggplot(pp_long[pp_long$CW == 4, ], aes(y = Prob, x=Time, group = Carbo, 
                                               linetype = Carbo)) + 
  geom_line(size = 1.2) +
  annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 17, xmax = 20, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") + 
  annotate("rect", xmin = 22, xmax = 29, ymin = 0, ymax = 1.00, 
           alpha = 0.3, fill = "grey") +
  geom_point(aes(shape = Carbo), size = 5,
             fill = "white") + 
  # scale_shape_discrete(solid=F) +
  theme(axis.title = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.line = element_line(colour = "black"), 
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 15),
        panel.grid.major = element_line(colour = "gray95",
                                        linetype = "dashed"),
        panel.grid.minor = element_line(colour = "gray95",
                                        linetype = "dashed"),
        # legend.position = "none"
        legend.position = "bottom",
        legend.direction = "horizontal"
  ) + 
  scale_shape_manual(values=c(21,23,24), 
                     labels =  c("Not eating\nany food", "Carbohydrate < 50%", "Carbohydrate >= 50%")) + 
  scale_linetype_manual(values=c("dotted", "dashed", "solid"),
                        labels =  c("Not eating\nany food", "Carbohydrate < 50%", "Carbohydrate >= 50%")) + 
  # theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  labs(title = "Class 4 days (21.4%)", y = "Probability") + 
  labs(shape="Responses to\ncarbohydrate intake", 
       linetype = "Responses to\ncarbohydrate intake")+
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) +
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam")) +
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(family = "Atlas Grotesk Medium", 
                                    size = 10, face = "bold", colour = "gray24", 
                                    hjust = 0, vjust = 1)) +
  labs(x = "Hours of the day", caption = "Note: 
Grey, and white shades indicate the 7 time slots;
Carbohydrate < 50% indicates that within the time slot, carbohydrate contributed less than 50% total energy intake; 
Carbohydrate >= 50% indicates that within the time slot, carbohydrate contributed higher or equal to 50% total 
      energy intake.")


library(cowplot)
plot_grid(cls1, cls2, cls3, cls4, ncol = 1, labels = c('A', 'B'), rel_heights=c(1,1,1,1.44))



chart.data <- CW4CB2 %>%
  group_by(CB, CW) %>%
  tally %>%
  group_by(CB) %>%
  mutate(pct = n/sum(n))


# chart.data$CW[chart.data$CW == 1] <- 0
# chart.data$CW[chart.data$CW == 3] <- 1
# chart.data$CW[chart.data$CW == 0] <- 3

chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
chart.data <- ddply(chart.data, .(CB),
                    transform, pos = cumsum(pct) - (0.5 * pct))



chart.data$CW <- factor(chart.data$CW, levels = c("4", "3", "2", "1"),
                        labels = c("Class 4 days", "Class 3 days", "Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2"),
                        labels = c("Individual\nclass 1\n(52.6%)",  "Individual\nclass 2\n(47.4%)"))



library(ggthemr)
ggplot() +
  geom_bar(aes(y = pct, x = CB, fill = CW), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, aes(x = CB, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)



## Visualisation of level 2 latent classes (CB = 3)




CW4CB3 <- read_table2("/home/wangcc-me/Documents/LSHTMproject/results/Timeslots/NDNSslot_CW4CB3.txt",
                      col_names = FALSE)

names(CW4CB3) <- c("Breakfast",
                   "Morning.snack",
                   "Lunch",
                   "Afternoon.snack",
                   "Dinner",
                   "Before.bedtime.snack",
                   "Midnight.food",
                   "ID_DAY",
                   "AGE",
                   "SEX",
                   "CPROB1",
                   "CPROB2",
                   "CPROB3",
                   "CPROB4",
                   "CPROB5",
                   "CPROB6",
                   "CPROB7",
                   "CPROB8",
                   "CPROB9",
                   "CPROB10",
                   "CPROB11",
                   "CPROB12",
                   "CB",
                   "CW",
                   "MLCJOINT",
                   "ID")




chart.data <- CW4CB3 %>%
  group_by(CB, CW) %>%
  tally %>%
  group_by(CB) %>%
  mutate(pct = n/sum(n))

chart.data$CW_new <- 0
chart.data$CW_new[chart.data$CW == 1] <- 3
chart.data$CW_new[chart.data$CW == 2] <- 4
chart.data$CW_new[chart.data$CW == 3] <- 2
chart.data$CW_new[chart.data$CW == 4] <- 1


chart.data <- chart.data[order(chart.data$CB, chart.data$CW_new),]
chart.data <- ddply(chart.data, .(CB),
                    transform, pos = cumsum(pct) - (0.5 * pct))



chart.data$CW_new <- factor(chart.data$CW_new, levels = c("4", "3", "2", "1"),
                            labels = c("Class 4 days", "Class 3 days", "Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3"),
                        labels = c("Individual\nclass 1\n(29.4%)",  "Individual\nclass 2\n(28.3%)",
                                   "Individual\nclass 3\n(42.3%)"))



library(ggthemr)
ggplot() +
  geom_bar(aes(y = pct, x = CB, fill = CW_new), data = chart.data, width = 0.6,
           stat="identity") +
  geom_text(data=chart.data, aes(x = CB, y = pos, label = paste0(sprintf("%1.1f", pct*100),"%")),
            size=4, colour="white", family="Atlas Grotesk Medium") +
  theme(legend.position="right", #legend.direction="horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 13), 
        axis.title = element_text(size = 18),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium"),
        text=element_text(family="Atlas Grotesk Light")) +
  labs(title = " ", x = "Between Individual classes", y = "Percentage") +
  scale_y_continuous(labels=percent)
