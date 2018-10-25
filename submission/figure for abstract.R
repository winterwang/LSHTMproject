library(plyr)
library(readr)


# same bar plot flip the axis for abstract --------------------------------
library(ggthemr)
library(scales)
ggthemr("greyscale")

# Fig 1. ------------------------------------------------------------------

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
Text$TotalEner <-  c("753.1 ", "854.6 ", "1622.1 ", "950.8 ", 
                     "2328.6 ","1063.5 ", "422.4 ")
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

CB1sources$Slot <- factor(CB1sources$Slot, levels = c("10 pm – 6 am",
                                                      "8 pm – 10 pm",
                                                      "5 pm – 8 pm",
                                                      "2 pm – 5 pm",
                                                      "12 noon – 2 pm",
                                                      "9 am – 12 am",
                                                      "6 am – 9 am"))
Text <- data.frame(levels(CB1sources$Slot))
names(Text)[1] <- "Slot"
Text$TotalEner <-  c("753.1 ", "854.6 ", "1622.1 ", "950.8 ", 
                     "2328.6 ","1063.5 ", "422.4 ")

levels(CB1sources$Slot) <- c("10pm \n-6am",
                             "8pm \n-10pm",
                             "5pm \n-8pm",
                             "2pm \n-5pm",
                             "12noon \n-2pm",
                             "9am \n-12noon",
                             "6am \n-9am")


levels(Text$Slot)<- c("10pm \n-6am",
                      "8pm \n-10pm",
                      "5pm \n-8pm",
                      "2pm \n-5pm",
                      "12noon \n-2pm",
                      "9am \n-12noon",
                      "6am \n-9am")
Text$TotalEner <-  c("1622.1 ", "753.1 ", "1063.5 ", "2328.6 ", "950.8 ", "854.6 ", 
                     "422.4 ")
Text$pos <- rep(1.10, 7)
CB1 <- ggplot() +
  geom_bar(aes(y = pct, x = Slot, fill = Sources), data = CB1sources, width = 0.6,
           stat="identity") +
  geom_text(data=CB1sources, aes(x = Slot, y = pos, label = paste0(sprintf("%1.1f", pct*100))),
            size = 3.5, colour="white", family="Atlas Grotesk Medium") +
  geom_text(data= Text, aes(x = Slot, y = pos, label = TotalEner), 
            size = 3.5, colour = "black", family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="6am \n-9am", label="1.4"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="9am \n-12noon", label="1.8"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="12noon \n-2pm", label="3.1"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  theme(#legend.position="right", #legend.direction="horizontal",
    legend.title = element_blank(),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    plot.title=element_text(family="Atlas Grotesk Medium", size = 10, hjust = 0.5),
    text=element_text(family="Atlas Grotesk Light"),
    legend.position = "none") + 
  # legend.position = "bottom", 
  # legend.direction = "horizontal") +
  labs(title = "Low carbohydrate eaters (28.1%)\n[high fat and drinking at night]", x = " ",
       y = "Percentage of energy sources") +# for producing the graph to the kakenhi file
  #labs(title = "Fig.1. Low carbohydrate eaters (28.1%)\n[high fat and drinking at night]", x = " ",
  #     y = "Percentage of energy sources") +
  # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels=percent, limits = c(-0.03, 1.11)) + coord_flip() 


# Fig 2. ------------------------------------------------------------------

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
Text$TotalEner <-  c("296.9 ", "967.9 ", 
                     "1310.9 ", "1106.1 ", "1977.2 ", 
                     "1086.9 ",  "599.7 ")
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


CB2sources$Slot <- factor(CB2sources$Slot, levels = c("10 pm – 6 am",
                                                      "8 pm – 10 pm",
                                                      "5 pm – 8 pm",
                                                      "2 pm – 5 pm",
                                                      "12 noon – 2 pm",
                                                      "9 am – 12 am",
                                                      "6 am – 9 am"))
levels(CB2sources$Slot) <- c("10pm \n-6am",
                             "8pm \n-10pm",
                             "5pm \n-8pm",
                             "2pm \n-5pm",
                             "12noon \n-2pm",
                             "9am \n-12noon",
                             "6am \n-9am")
levels(Text$Slot)<- c("10pm \n-6am",
                      "8pm \n-10pm",
                      "5pm \n-8pm",
                      "2pm \n-5pm",
                      "12noon \n-2pm",
                      "9am \n-12noon",
                      "6am \n-9am")
Text$TotalEner <-  c("1310.9 ", "296.9 ",  "1086.9 ", "1977.2 ", "1106.1 ", "967.9 ", "599.7 ")
Text$pos <- rep(1.10, 7)
CB2 <- ggplot() +
  geom_bar(aes(y = pct, x = Slot, fill = Sources), data = CB2sources, width = 0.6,
           stat="identity") +
  geom_text(data=CB2sources, aes(x = Slot, y = pos, label = paste0(sprintf("%1.1f", pct*100))),
            size = 3.5, colour="white", family="Atlas Grotesk Medium") +
  geom_text(data= Text, aes(x = Slot, y = pos, label = TotalEner), 
            size = 3.5, colour = "black", family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="6am \n-9am", label="0.3"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="9am \n-12noon", label="0.1"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="12noon \n-2pm", label="0.6"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  geom_text(aes(y=-0.03, x="2pm \n-5pm", label="2.4"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  geom_text(aes(y=-0.03, x="5pm \n-8pm", label="3.7"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  theme(#legend.position="right", #legend.direction="horizontal",
    legend.title = element_blank(),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    plot.title=element_text(family="Atlas Grotesk Medium", size = 10, hjust = 0.5),
    text=element_text(family="Atlas Grotesk Light"),
    legend.position = "none", 
    axis.text.y=element_blank()) +
  # legend.position = "bottom", 
  # legend.direction = "horizontal") +
  labs(title = "Moderate carbohydrate eaters (28.8%)\n[eating late]", x = " ", 
       y = "Percentage of energy sources") +# for producing the graph to the kakenhi file
  #labs(title = "Fig.2. Moderate carbohydrate eaters (28.8%)\n[eating late]", x = " ", 
  #     y = "Percentage of energy sources") +
  # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_y_continuous(labels=percent, limits = c(-0.03, 1.11)) + 
  coord_flip()


# Fig 3. ------------------------------------------------------------------



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
Text$TotalEner <-  c("929.0 ", "746.4 ",  "1788.8 ",  "785.0 ", 
                     "2368.9 ", "860.2 ", "205.5 ")
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



CB3sources$Slot <- factor(CB3sources$Slot, levels = c("10 pm – 6 am",
                                                      "8 pm – 10 pm",
                                                      "5 pm – 8 pm",
                                                      "2 pm – 5 pm",
                                                      "12 noon – 2 pm",
                                                      "9 am – 12 am",
                                                      "6 am – 9 am"))
levels(CB3sources$Slot) <- c("10pm \n-6am",
                             "8pm \n-10pm",
                             "5pm \n-8pm",
                             "2pm \n-5pm",
                             "12noon \n-2pm",
                             "9am \n-12noon",
                             "6am \n-9am")
Text$TotalEner <-  c("1788.8 ",  "929.0 ", "860.2 ", "2368.9 ", "785.0 ",  "746.4 ",
                     "205.5 ")
levels(Text$Slot)<- c("10pm \n-6am",
                      "8pm \n-10pm",
                      "5pm \n-8pm",
                      "2pm \n-5pm",
                      "12noon \n-2pm",
                      "9am \n-12noon",
                      "6am \n-9am")
Text$pos <- rep(1.10, 7)

CB3 <- ggplot() +
  geom_bar(aes(y = pct, x = Slot, fill = Sources), data = CB3sources, width = 0.6,
           stat="identity") +
  geom_text(data=CB3sources, aes(x = Slot, y = pos, label = paste0(sprintf("%1.1f", pct*100))),
            size = 3.5, colour="white", family="Atlas Grotesk Medium") +
  geom_text(data = Text, aes(x = Slot, y = pos, label = TotalEner), 
            size = 3.5, colour = "black", family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="6am \n-9am", label="0.3"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="9am \n-12noon", label="0.2"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="12noon \n-2pm", label="1.1"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  geom_text(aes(y=-0.03, x="2pm \n-5pm", label="2.8"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  geom_text(aes(y=-0.03, x="5pm \n-8pm", label="4.0"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  theme(legend.direction="horizontal",
    legend.position = "bottom",
    #legend.title = element_blank(),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10), 
    axis.title = element_text(size = 10),
    axis.line = element_line(colour = "black"),
    plot.title=element_text(family="Atlas Grotesk Medium", size = 10, hjust = 0.5),
    text=element_text(family="Atlas Grotesk Light"),
    axis.text.y=element_blank())+
    #legend.position = "none") +
  #scale_fill_manual(values = c("#db735c", "#EFA86E", "#9A8A76", "#F3C57B"),
  #                  labels =  c("Carbo-\nhydrate","Fat", "Protein",  "Alcohol")) + 
  labs(title = "High carbohydrate eaters (43.1%)\n[eating early]", x = " ", 
       y = "Percentage of energy sources") +# for producing the graph to the kakenhi file
  #labs(title = "Fig.3. High carbohydrate eaters (43.1%)\n[eating early]", x = " ", 
  #     y = "Percentage of energy sources") +
  #theme(axis.text.x = element_text(angle = 14, hjust = 1)) +
  scale_y_continuous(labels=percent, limits = c(-0.03, 1.11))+ 
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE))

CB3noleg <- ggplot() +
  geom_bar(aes(y = pct, x = Slot, fill = Sources), data = CB3sources, width = 0.6,
           stat="identity") +
  geom_text(data=CB3sources, aes(x = Slot, y = pos, label = paste0(sprintf("%1.1f", pct*100))),
            size = 3.5, colour="white", family="Atlas Grotesk Medium") +
  geom_text(data = Text, aes(x = Slot, y = pos, label = TotalEner), 
            size = 3.5, colour = "black", family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="6am \n-9am", label="0.3"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="9am \n-12noon", label="0.2"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") +
  geom_text(aes(y=-0.03, x="12noon \n-2pm", label="1.1"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  geom_text(aes(y=-0.03, x="2pm \n-5pm", label="2.8"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  geom_text(aes(y=-0.03, x="5pm \n-8pm", label="4.0"), size = 3.5, colour = "black",
            family="Atlas Grotesk Medium") + 
  theme(#legend.direction="horizontal",
        #legend.position = "bottom",
        #legend.title = element_blank(),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10), 
        axis.title = element_text(size = 10),
        axis.line = element_line(colour = "black"),
        plot.title=element_text(family="Atlas Grotesk Medium", size = 10, hjust = 0.5),
        text=element_text(family="Atlas Grotesk Light"),
        axis.text.y=element_blank(),
        legend.position = "none") +
  #scale_fill_manual(values = c("#db735c", "#EFA86E", "#9A8A76", "#F3C57B"),
  #                  labels =  c("Carbo-\nhydrate","Fat", "Protein",  "Alcohol")) + 
  labs(title = "High carbohydrate eaters (43.1%)\n[eating early]", x = " ", 
       y = "Percentage of energy sources") + # for producing the graph to the kakenhi file
  #labs(title = "Fig.3. High carbohydrate eaters (43.1%)\n[eating early]", x = " ", 
  #     y = "Percentage of energy sources") +
  #theme(axis.text.x = element_text(angle = 14, hjust = 1)) +
  scale_y_continuous(labels=percent, limits = c(-0.03, 1.11))+ 
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE))

library(cowplot)
leg <- get_legend(CB3 + theme(legend.direction = "horizontal",legend.justification="center" ,legend.box.just = "bottom"))
abstract <- plot_grid(CB1, CB2, CB3noleg, ncol = 3, rel_widths =c(1.27,1.1,1.1), labels = c("A", "B", "C"))
p <- plot_grid( abstract, leg, ncol = 1, rel_heights = c(1, .05))
p
