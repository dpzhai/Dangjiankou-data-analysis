library(openxlsx) # for reading and writing excel
library(ggplot2)
library(cowplot) # For plot_grid()


PC.Rel <- read.xlsx(paste(Path, "Relative importance_HP in RDA.xlsx", sep=""), sheet=1)
PW.Rel <- read.xlsx(paste(Path, "Relative importance_HP in RDA.xlsx", sep=""), sheet=2)
MC.Rel <- read.xlsx(paste(Path, "Relative importance_HP in RDA.xlsx", sep=""), sheet=3)
MW.Rel <- read.xlsx(paste(Path, "Relative importance_HP in RDA.xlsx", sep=""), sheet=4)

#PC.Rel$Variable <- factor(PC.Rel$Variable, levels = c(PC.Rel$Variable))
#PW.Rel$Variable <- factor(PW.Rel$Variable, levels = c(PW.Rel$Variable))
#MC.Rel$Variable <- factor(MC.Rel$Variable, levels = c(MC.Rel$Variable))
#MW.Rel$Variable <- factor(MW.Rel$Variable, levels = c(MW.Rel$Variable))


PC.Rel$Group <- factor(PC.Rel$Group, levels = c("Soil", "Mi", "Lig", "AM"))
PW.Rel$Group <- factor(PW.Rel$Group, levels = c("Soil", "Mi", "Lig", "AM"))
MC.Rel$Group <- factor(MC.Rel$Group, levels = c("Soil", "Mi", "Lig", "AM"))
MW.Rel$Group <- factor(MW.Rel$Group, levels = c("Soil", "Mi", "Lig", "AM"))

Color <- c( '#B3DE69',  '#BC80BD', '#80B1D3', '#FDB462')

#********************************************************************************************** PC
PC.Rel_re <- with(PC.Rel, reorder(Variable, Var.exp))
PCName <- levels(PC.Rel_re)
PCName[1] <- "FN/BN"



Fig.PC <- ggplot(PC.Rel, aes(y=Var.exp, reorder(Variable, Var.exp)))+  # fill 给单个bar不同的颜色
  geom_bar(aes(fill=Group), stat = 'identity',width=0.5)+
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30),expand = c(0,0))+
  scale_x_discrete(labels=PCName)+
  scale_fill_manual(values = Color )+
  ylab(expression(paste("Variation explained (%)")))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black",  fill=NA, linewidth = 0.5,linetype = 1),
        text = element_text(size=14,family ="serif"),
        axis.text.x=element_text(family ="serif",color="black", size=14),
        axis.text.y=element_text(family ="serif",color="black", size=10),
        axis.line = element_line(colour="black"),
        axis.title = element_blank(),
        plot.margin=margin(t=5.5, r=8, b=5.5, l=5.5, unit = "pt"),
        legend.position="None")+
  annotate("text", x=11, y= 25, label="(a)", size=5,family ="serif", color = "black")+
  coord_flip()  # x y 轴互换位置

Fig.PC  #(3.8, 2.6)

#********************************************************************************************** PW
PW.Rel_re <- with(PW.Rel, reorder(Variable, Var.exp))
PWName <- levels(PW.Rel_re)
 
PWName[1] <- "FN/BN"


Fig.PW <- ggplot(PW.Rel, aes(y=Var.exp, reorder(Variable, Var.exp)))+  # fill 给单个bar不同的颜色
  geom_bar(aes(fill=Group), stat = 'identity',width=0.5)+
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30),expand = c(0,0))+
  scale_x_discrete(labels=PWName)+
  scale_fill_manual(values = Color )+
  ylab(expression(paste("Variation explained (%)")))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black",  fill=NA, linewidth = 0.5,linetype = 1),
        text = element_text(size=14,family ="serif"),
        axis.text.x=element_text(family ="serif",color="black", size=14),
        axis.text.y=element_text(family ="serif",color="black", size=10),
        axis.line = element_line(colour="black"),
        axis.title = element_blank(),
        plot.margin=margin(t=5.5, r=8, b=5.5, l=5.5, unit = "pt"),
        legend.position="None")+
  annotate("text", x=11, y= 25, label="(b)", size=5,family ="serif", color = "black")+
  coord_flip()  # x y 轴互换位置

Fig.PW  #(3.8, 2.6)


#********************************************************************************************** MC
MC.Rel_re <- with(MC.Rel, reorder(Variable, Var.exp))
MCName <- levels(MC.Rel_re)
  # "SM"       "AG"       "PH"       "NH4"      "PHO"      "Syringyl" 
                                         # "BNC"      "GN"       "Bacteria" "GP"       "Total Am_sugar" "FNC"      "TN" 
# MCName[11] <- "Total Am_sugar"


Fig.MC <- ggplot(MC.Rel, aes(y=Var.exp, reorder(Variable, Var.exp)))+  # fill 给单个bar不同的颜色
  geom_bar(aes(fill=Group), stat = 'identity',width=0.5)+
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30),expand = c(0,0))+
  scale_x_discrete(labels=MCName)+
  scale_fill_manual(values = Color )+
  ylab(expression(paste("Variation explained (%)")))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black",  fill=NA, linewidth = 0.5,linetype = 1),
        text = element_text(size=14,family ="serif"),
        axis.text.x=element_text(family ="serif",color="black", size=14),
        axis.text.y=element_text(family ="serif",color="black", size=10),
        axis.line = element_line(colour="black"),
        axis.title = element_blank(),
        plot.margin=margin(t=5.5, r=8, b=5.5, l=5.5, unit = "pt"),
        legend.position="None")+
  annotate("text", x=13, y= 25, label="(c)", size=5,family ="serif", color = "black")+
  coord_flip()  # x y 轴互换位置

Fig.MC  #(3.8, 2.6)

#********************************************************************************************** MW
MW.Rel_re <- with(MW.Rel, reorder(Variable, Var.exp))
MWName <- levels(MW.Rel_re)
 

Fig.MW <- ggplot(MW.Rel, aes(y=Var.exp, reorder(Variable, Var.exp)))+  # fill 给单个bar不同的颜色
  geom_bar(aes(fill=Group), stat = 'identity',width=0.5)+
  scale_y_continuous(limits = c(0, 30), breaks=c(0, 10, 20, 30),expand = c(0,0))+
  scale_x_discrete(labels=MWName)+
  scale_fill_manual(values = Color )+
  ylab(expression(paste("Variation explained (%)")))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black",  fill=NA, linewidth = 0.5,linetype = 1),
        text = element_text(size=14,family ="serif"),
        axis.text.x=element_text(family ="serif",color="black", size=14),
        axis.text.y=element_text(family ="serif",color="black", size=10),
        axis.line = element_line(colour="black"),
        axis.title = element_blank(),
        plot.margin=margin(t=5.5, r=8, b=5.5, l=5.5, unit = "pt"),
        legend.position="None")+
  annotate("text", x=13, y= 25, label="(d)", size=5,family ="serif", color = "black")+
  coord_flip()  # x y 轴互换位置

Fig.MW  #(3.8, 2.6)

plot_grid(Fig.PC,  Fig.PW, Fig.MC, Fig.MW, 
          nrow=2, ncol=2, # rel_heights = c(4,4,4,4.2), # rel_widths = 5,  , axis="tblr"
          align="hv")  #(7.5, 7.5)
