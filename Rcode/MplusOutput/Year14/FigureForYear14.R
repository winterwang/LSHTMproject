# This file is written to see the results for participants year 1-4 and 5-8 separately
# author; Chaochen Wang
# Date created: 2019-05-10
# Date updated: 
library(epiDisplay)
library(readr)
NDNSslot_CW3CB3 <- read_table2("results/Timeslots/NDNSslot_CW3CB3.txt",
                               col_names = FALSE)

names(NDNSslot_CW3CB3) <- c("H6_9",
                            "H9_12",
                            "H12_14",
                            "H14_17",
                            "H17_20",
                            "H20_22",
                            "H22_6",
                            "ID_DY",
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

with(NDNSslot_CW3CB3, tabpct(CW, SEX))


# Day level graph in  year 1-4, 2 day types, Time slots level 1 classes ----------------------------------------------
library(readr)
library(tidyverse)


CW2CB4Y14 <- read_table2("Rcode/MplusOutput/Year14/CW2CB4.txt",
                       col_names = FALSE)

CW2Y14 <- read_table2("Rcode/MplusOutput/Year14/LCA2.txt",
                         col_names = FALSE) #LCA2.txt

CW2CB2Y14 <- read_table2("Rcode/MplusOutput/Year14/CW2CB2.txt",
                         col_names = FALSE)

CW2CB3Y14 <- read_table2("Rcode/MplusOutput/Year14/CW2CB3.txt",
                         col_names = FALSE)


names(CW2CB4Y14) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "DAYNO",
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
                    #"CPROB9",
                    #"CPROB10",
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")


names(CW2Y14) <- c("H6_9",
                       "H9_12",
                       "H12_14",
                       "H14_17",
                       "H17_20",
                       "H20_22",
                       "H22_6",
                       "ID",
                       "DAYNO",
                       "AGE",
                       "SEX",
                       "CPROB1",
                       "CPROB2",
                       "C",
                       "ID_DAY")

names(CW2CB2Y14) <- c("Breakfast", 
                      "Morning.snack",
                      "Lunch",
                      "Afternoon.snack",
                      "Dinner",
                      "Before.bedtime.snack",
                      "Midnight.food",
                      "DAYNO",
                      "AGE",
                      "SEX",
                      "CPROB1",
                      "CPROB2",
                      "CPROB3",
                      "CPROB4",
                      #"CPROB5",
                      #"CPROB6",
                      #"CPROB7",
                      #"CPROB8",
                      #"CPROB9",
                      #"CPROB10",
                      "CB",
                      "CW",
                      "MLCJOINT",
                      "ID_DAY",
                      "ID")

names(CW2CB3Y14) <- c("Breakfast", 
                      "Morning.snack",
                      "Lunch",
                      "Afternoon.snack",
                      "Dinner",
                      "Before.bedtime.snack",
                      "Midnight.food",
                      "DAYNO",
                      "AGE",
                      "SEX",
                      "CPROB1",
                      "CPROB2",
                      "CPROB3",
                      "CPROB4",
                      "CPROB5",
                      "CPROB6",
                      #"CPROB7",
                      #"CPROB8",
                      #"CPROB9",
                      #"CPROB10",
                      "CB",
                      "CW",
                      "MLCJOINT",
                      "ID_DAY",
                      "ID")

pp <- CW2CB4Y14 %>% 
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
  labs(title = "Class 1 days (53.9%)", x = "", 
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
  labs(title = "Class 2 days (46.1%)", y = "Probability") + 
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

# cls3 <- cls3 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# cls3
plot_grid(cls1, cls2, ncol = 1, labels = c('A', 'B'), rel_heights=c(1,1.7))





# Day level graph in  year 1-4, 3 day types, Times slots level 1 classes --------



CW3CB2Y14 <- read_table2("Rcode/MplusOutput/Year14/CW3CB2.txt",
                         col_names = FALSE)

names(CW3CB2Y14) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "DAYNO",
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
                    # "CPROB10",
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")


CW3CB3Y14 <- read_table2("Rcode/MplusOutput/Year14/CW3CB3.txt",
                         col_names = FALSE)

names(CW3CB3Y14) <- c("Breakfast", 
                      "Morning.snack",
                      "Lunch",
                      "Afternoon.snack",
                      "Dinner",
                      "Before.bedtime.snack",
                      "Midnight.food",
                      "DAYNO",
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
                      # "CPROB10",
                      "CB",
                      "CW",
                      "MLCJOINT",
                      "ID_DAY",
                      "ID")


tab1(CW3CB2Y14$CW)
tab1(CW3CB3Y14$CW)


pp <- CW3CB2Y14 %>% 
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


cls1 <- ggplot(pp_long[pp_long$CW == 3, ], aes(y = Prob, x=Time, group = Carbo, 
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
  labs(title = "Class 1 days - High percentage carbohydrate day (37.0%)", x = "", 
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
  labs(title = "Class 2 days - Low percentage carbohydrate day (23.4%)", x = "", 
       y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))





cls3 <- ggplot(pp_long[pp_long$CW == 1, ], aes(y = Prob, x=Time, group = Carbo, 
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
  labs(title = "Class 3 days - Regular meals days (39.7%)", y = "Probability") + 
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
Carbohydrate >= 50% indicates CH contributed higher or equal to 50% total energy intake.")


library(cowplot)
cls1 <- cls1 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
cls2 <- cls2 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
# plot_grid(cls1, cls2, rel_heights = c(1,1), ncol = 1)

# dev.copy2pdf(file="../gemini/Fig/Fig01a.pdf",out.type="cairo", width=7.0, height=6.10)

cls3 <- cls3 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# cls3
plot_grid(cls1, cls2, cls3, ncol = 1, labels = c('A', 'B', 'C'), rel_heights=c(1,1,1.8))

# cls1 looks like regular meals days
# cls2 looks like low percentage CH days
# cls3 looks like high percentage CH days





# Day level graph in year 5-8, 2 day types, Time slots level 1 classes ----------------------------------------------
library(readr)
library(tidyverse)



CW2CB4Y58 <- read_table2("Rcode/MplusOutput/Year58/CW2CB4.txt",
                         col_names = FALSE)


names(CW2CB4Y58) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "DAYNO",
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
                    #"CPROB9",
                    #"CPROB10",
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")

tab1(CW2CB4Y58$CW)

pp <- CW2CB4Y58 %>% 
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
  labs(title = "Class 1 days (45.8%)", x = "", 
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
  labs(title = "Class 2 days (54.2%)", y = "Probability") + 
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
Carbohydrate >= 50% indicates CH contributed higher or equal to 50% total energy intake.")


library(cowplot)
cls1 <- cls1 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
cls2 <- cls2 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# plot_grid(cls1, cls2, rel_heights = c(1,1), ncol = 1)

# dev.copy2pdf(file="../gemini/Fig/Fig01a.pdf",out.type="cairo", width=7.0, height=6.10)

# cls3 <- cls3 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# cls3
plot_grid(cls1, cls2, ncol = 1, labels = c('A', 'B'), rel_heights=c(1,1.8))

# Day level graph in Women, 3 day types, Times slots level 1 classes --------

CW3CB2f <- read_table2("Rcode/MplusOutput/MultilevelWomen/CW3CB2f.txt",
                       col_names = FALSE)

names(CW3CB2f) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "DAYNO",
                    "AGE",
                    # "SEX",
                    "CPROB1",
                    "CPROB2",
                    "CPROB3",
                    "CPROB4",
                    "CPROB5",
                    "CPROB6",
                    # "CPROB7",
                    # "CPROB8",
                    # "CPROB9",
                    # "CPROB10",
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")

tab1(CW3CB2f$CW)

CW3CB3f <- read_table2("Rcode/MplusOutput/MultilevelWomen/CW3CB3f.txt",
                       col_names = FALSE)

names(CW3CB3f) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "DAYNO",
                    "AGE",
                    # "SEX",
                    "CPROB1",
                    "CPROB2",
                    "CPROB3",
                    "CPROB4",
                    "CPROB5",
                    "CPROB6",
                    "CPROB7",
                    "CPROB8",
                    "CPROB9",
                    # "CPROB10",
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")

tab1(CW3CB3f$CW)

pp <- CW3CB2f %>% 
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


cls1 <- ggplot(pp_long[pp_long$CW == 3, ], aes(y = Prob, x=Time, group = Carbo, 
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
  labs(title = "Class 1 days - High percentage carbohydrate day (40.8%)", x = "", 
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
  labs(title = "Class 2 days - Low percentage carbohydrate day (19.2%)", x = "", 
       y = "Probability",
       color = "Carbohydrate\nintake") + 
  # scale_shape_discrete(labels = c("Not eating", "< 50%", ">= 50%")) + 
  ylim(c(0,1)) + 
  scale_x_continuous(breaks = c(6, 9, 12, 14, 17, 20, 22, 29), 
                     labels = c("6\nam", "9\nam", "12\nnoon", "2\npm",
                                "5\npm", "8\npm", "10\npm", "6\nam"))





cls3 <- ggplot(pp_long[pp_long$CW == 1, ], aes(y = Prob, x=Time, group = Carbo, 
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
  labs(title = "Class 3 days - Regular meals days (40.0%)", y = "Probability") + 
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
       Carbohydrate >= 50% indicates CH contributed higher or equal to 50% total energy intake.")


library(cowplot)
cls1 <- cls1 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
cls2 <- cls2 + theme(plot.margin = unit(c(0, 0, -0.5, 0), "cm"))
# plot_grid(cls1, cls2, rel_heights = c(1,1), ncol = 1)

# dev.copy2pdf(file="../gemini/Fig/Fig01a.pdf",out.type="cairo", width=7.0, height=6.10)

cls3 <- cls3 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
# cls3
plot_grid(cls1, cls2, cls3, ncol = 1, labels = c('A', 'B', 'C'), rel_heights=c(1,1,1.8))

# cls1 looks like regular meals days
# cls2 looks like low percentage CH days
# cls3 looks like high percentage CH days






# Level 2 (CB=5) in men person classes distribution -------------------------------------
library(plyr)
library(readr)
library(scales)
library(tidyverse)
CW2CB5m <- read_table2("Rcode/MplusOutput/MultilevelMen/CW2CB5m.txt",
                       col_names = FALSE)

names(CW2CB5m) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "ID_DAYNO",
                    "AGE",
                    # "SEX",
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
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")




chart.data <- CW2CB5m %>% 
  group_by(CB, CW) %>% 
  tally %>% 
  group_by(CB) %>% 
  mutate(pct = n/sum(n))

# 
# chart.data$CW_new <- 0
# chart.data$CW_new[chart.data$CW == 1] <- 3
# chart.data$CW_new[chart.data$CW == 2] <- 1
# chart.data$CW_new[chart.data$CW == 3] <- 2
# 

chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
chart.data <- ddply(chart.data, .(CB), 
                    transform, pos = cumsum(pct) - (0.5 * pct)) 



chart.data$CW <- factor(chart.data$CW, levels = c("2", "1"),
                        labels = c("Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3", "4", "5"), 
                        labels = c("Individual\n class 1\n(16.5%)",  
                                   "Individual\n class 2\n(18.4%)",
                                   "Individual\n class 3\n(20.8%)", 
                                   "Individual\n class 4\n(21.5%)",
                                   "Individual\n class 5\n(22.5%)"))



library(ggthemr)
# ggthemr("dust", layout = "scientific")
# ggthemr("fresh", layout = "scientific")
ggthemr("greyscale", layout = "scientific")

ggplot() + 
  geom_bar(aes(y = pct, x = CB, fill = CW), data = chart.data, width = 0.6,
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


# Level 2 (CB=3) in men person classes distribution -------------------------------------

CW3CB3m <- read_table2("Rcode/MplusOutput/MultilevelMen/CW3CB3m.txt",
                       col_names = FALSE)

names(CW3CB3m) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "ID_DAYNO",
                    "AGE",
                    # "SEX",
                    "CPROB1",
                    "CPROB2",
                    "CPROB3",
                    "CPROB4",
                    "CPROB5",
                    "CPROB6",
                    "CPROB7",
                    "CPROB8",
                    "CPROB9",
                    # "CPROB10",
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")




chart.data <- CW3CB3m %>% 
  group_by(CB, CW) %>% 
  tally %>% 
  group_by(CB) %>% 
  mutate(pct = n/sum(n))


chart.data$CW_new <- 0
chart.data$CW_new[chart.data$CW == 1] <- 3
chart.data$CW_new[chart.data$CW == 2] <- 1
chart.data$CW_new[chart.data$CW == 3] <- 2


# chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
# chart.data <- ddply(chart.data, .(CB), 
#                     transform, pos = cumsum(pct) - (0.5 * pct)) 
# 


chart.data$CW_new <- factor(chart.data$CW_new, levels = c("1", "3", "2"),
                            labels = c("Regular\n meals days",
                                       "Low carbo-\nhydrate days",
                                       "High carbo-\nhydrate days"))
chart.data$CB <- factor(chart.data$CB, levels = c("3", "1", "2"), 
                        labels = c("Low CH\n eaters\n(30.6%)",  
                                   "Moderate CH\n eaters\n(33.5%)",
                                   "High CH\n eaters\n(35.9%)"))

chart.data$pos[1] <- 0.3943787 + 0.2692308 - 0.5*0.3943787
chart.data$pos[2] <- 0.3943787 + 0.2692308 + 0.3363905 - 0.5*0.3363905
chart.data$pos[3] <- 0.2692308 - 0.5*0.2692308
chart.data$pos[4] <- chart.data$pct[4] + chart.data$pct[6] - 0.5*chart.data$pct[4]
chart.data$pos[5] <- chart.data$pct[4] + chart.data$pct[6] + chart.data$pct[5] - 0.5*chart.data$pct[5]
chart.data$pos[6] <- chart.data$pct[6] - 0.5*chart.data$pct[6]
chart.data$pos[7] <- chart.data$pct[7] + chart.data$pct[9] - 0.5*chart.data$pct[7]
chart.data$pos[8] <- chart.data$pct[7] + chart.data$pct[9] + chart.data$pct[8] - 0.5*chart.data$pct[8]
chart.data$pos[9] <- chart.data$pct[9] - 0.5*chart.data$pct[9]


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



# Level 2 (CB=5) in women person classes distribution -------------------------------------

CW2CB5f <- read_table2("Rcode/MplusOutput/MultilevelWomen/CW2CB5f.txt",
                       col_names = FALSE)

names(CW2CB5f) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "ID_DAYNO",
                    "AGE",
                    # "SEX",
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
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")




chart.data <- CW2CB5f %>% 
  group_by(CB, CW) %>% 
  tally %>% 
  group_by(CB) %>% 
  mutate(pct = n/sum(n))

# 
# chart.data$CW_new <- 0
# chart.data$CW_new[chart.data$CW == 1] <- 3
# chart.data$CW_new[chart.data$CW == 2] <- 1
# chart.data$CW_new[chart.data$CW == 3] <- 2
# 

chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
chart.data <- ddply(chart.data, .(CB), 
                    transform, pos = cumsum(pct) - (0.5 * pct)) 



chart.data$CW <- factor(chart.data$CW, levels = c("2", "1"),
                        labels = c("Class 2 days", "Class 1 days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3", "4", "5"), 
                        labels = c("Individual\n class 1\n(17.1%)",  
                                   "Individual\n class 2\n(27.9%)",
                                   "Individual\n class 3\n(20.7%)", 
                                   "Individual\n class 4\n(19.0%)",
                                   "Individual\n class 5\n(15.3%)"))



library(ggthemr)
# ggthemr("dust", layout = "scientific")
# ggthemr("fresh", layout = "scientific")
ggthemr("greyscale", layout = "scientific")

ggplot() + 
  geom_bar(aes(y = pct, x = CB, fill = CW), data = chart.data, width = 0.6,
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



# Level 2 (CB=3) in women person classes distribution -------------------------------------

CW3CB3f <- read_table2("Rcode/MplusOutput/MultilevelWomen/CW3CB3f.txt",
                       col_names = FALSE)

names(CW3CB3f) <- c("Breakfast", 
                    "Morning.snack",
                    "Lunch",
                    "Afternoon.snack",
                    "Dinner",
                    "Before.bedtime.snack",
                    "Midnight.food",
                    "ID_DAYNO",
                    "AGE",
                    # "SEX",
                    "CPROB1",
                    "CPROB2",
                    "CPROB3",
                    "CPROB4",
                    "CPROB5",
                    "CPROB6",
                    "CPROB7",
                    "CPROB8",
                    "CPROB9",
                    # "CPROB10",
                    "CB",
                    "CW",
                    "MLCJOINT",
                    "ID_DAY",
                    "ID")




chart.data <- CW3CB3f %>% 
  group_by(CB, CW) %>% 
  tally %>% 
  group_by(CB) %>% 
  mutate(pct = n/sum(n))


chart.data$CW_new <- 0
chart.data$CW_new[chart.data$CW == 1] <- 3
chart.data$CW_new[chart.data$CW == 2] <- 1
chart.data$CW_new[chart.data$CW == 3] <- 2


chart.data <- chart.data[order(chart.data$CB, chart.data$CW),]
chart.data <- ddply(chart.data, .(CB),
                    transform, pos = cumsum(pct) - (0.5 * pct))



chart.data$CW <- factor(chart.data$CW, levels = c("1", "2", "3"),
                        labels = c("Regular\n meals days",
                                   "Low carbo-\nhydrate days",
                                   "High carbo-\nhydrate days"))
chart.data$CB <- factor(chart.data$CB, levels = c("1", "2", "3"), 
                        labels = c("Low CH\n eaters\n(23.5%)",  
                                   "Moderate CH\n eaters\n(28.7%)",
                                   "High CH\n eaters\n(46.0%)"))

chart.data$pos[1] <- chart.data$pct[1] + chart.data$pct[2] + chart.data$pct[3] - 0.5*chart.data$pct[1]
chart.data$pos[2] <- chart.data$pct[2] + chart.data$pct[3] - 0.5*chart.data$pct[2]
chart.data$pos[3] <- chart.data$pct[3] - 0.5*chart.data$pct[3]
chart.data$pos[4] <- chart.data$pct[4] + chart.data$pct[5] + chart.data$pct[6] - 0.5*chart.data$pct[4]
chart.data$pos[5] <- chart.data$pct[5] + chart.data$pct[6] - 0.5*chart.data$pct[5]
chart.data$pos[6] <- chart.data$pct[6] - 0.5*chart.data$pct[6]
chart.data$pos[7] <- chart.data$pct[7] + chart.data$pct[8] + chart.data$pct[9] - 0.5*chart.data$pct[7]
chart.data$pos[8] <- chart.data$pct[8] + chart.data$pct[9] - 0.5*chart.data$pct[8]
chart.data$pos[9] <- chart.data$pct[9] - 0.5*chart.data$pct[9]


library(ggthemr)
# ggthemr("dust", layout = "scientific")
# ggthemr("fresh", layout = "scientific")
ggthemr("greyscale", layout = "scientific")

ggplot() + 
  geom_bar(aes(y = pct, x = CB, fill = CW), data = chart.data, width = 0.6,
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
