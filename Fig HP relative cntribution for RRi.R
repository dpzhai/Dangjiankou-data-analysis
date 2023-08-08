library(openxlsx)
library(cowplot) # For plot_grid()
library(ggplot2)
library(ggprism)
library(RColorBrewer)


PRR.Rel <- read.xlsx(paste(Path, "HP results for RRi.xlsx", sep=""), sheet=3)
MRR.Rel <- read.xlsx(paste(Path, "HP results for RRi.xlsx", sep=""), sheet=4)


PRR.Rel$Group <- factor(PRR.Rel$Group, levels =rev(c("Afforestation type", "Mi_necromass", "Lignin", "Soil", "Microbe")))
PRR.Rel$Variable <- factor(PRR.Rel$Variable, levels = c("Former_use", "Species", "BNC","Cinnamyl", "Syringyl",
                                                        "PH","TN", "PHO", "F.B", "PEO", "Ana", "GP", "Bacteria",
                                                        "GN", "Act", "BG"))


MRR.Rel$Group <- factor(MRR.Rel$Group, levels = rev(c("Afforestation type", "Mi_necromass", "Lignin", "Soil", "Microbe")))
MRR.Rel$Variable <- factor(MRR.Rel$Variable, levels = c("Former_use", "Species", "FNC","Vanillyl", "Syringyl",
                                                        "NH4", "PH","TN", "MBC", "GP", "BG","Act", "Ana", "PHO" ))


#***************************POC.RR
#*
Fig_PRR.Rel <- ggplot(PRR.Rel, aes(x=Group, y=Per, fill=Variable))+  # fill 给单个bar不同的颜色
  geom_bar(stat = 'identity', position="stack",color="black", width=0.5,size=0.25)+
  scale_y_continuous(limits = c(0, 53), breaks=c(0, 10, 20, 30, 40, 50),expand = c(0,0))+
  scale_x_discrete(labels=rev(c("Aff_type", "Mi_necromass", "Lig", "Soil", "Microbe")))+
  ylab(expression(paste("Relative percentage of contribution (%)")))+
  geom_text(aes(x=Group),
            y=PRR.Rel$Lab.Y+4,
            label=PRR.Rel$Lab.Y,
            size=6,family ="serif",color="black")+
  geom_text(aes(x=Group),
            y=PRR.Rel$Lab.Y+6.5,
            label="%",
            size=6,family ="serif",color="black")+
  #scale_fill_manual(values=brewer.pal(9,"GnBu"))+
  #scale_fill_prism(palette = "candy_bright")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black",  fill=NA, linewidth = 0.5,linetype = 1),
        text = element_text(size=14,family ="serif"),
        axis.text.x=element_text(family ="serif",color="black", size=16),
        axis.text.y=element_text(family ="serif",color="black", size=16),
        axis.line = element_line(colour="black"),
        axis.title.y = element_blank(),
        axis.ticks.length.x = unit(-2.75, "pt"),
        plot.margin=margin(t=5.5, r=5.5, b=5.5, l=5.5, unit = "pt"),
        legend.position="None")+
  annotate("text", x=5.2, y= 51, label="(a)", size=6, family ="serif", color = "black")+
  coord_flip()  # x y 轴互换位置

Fig_PRR.Rel  #(3.8, 2.6)

#**************************************************************************************

#* **************************        MAOC.RR
#* 
#* 

Fig_MRR.Rel <- ggplot(MRR.Rel, aes(x=Group, y=Per, fill=Variable))+  # fill 给单个bar不同的颜色
  geom_bar(stat = 'identity', position="stack",color="black", width=0.5,size=0.25)+
  scale_y_continuous(limits = c(0, 53), breaks=c(0, 10, 20, 30, 40, 50),expand = c(0,0))+
  scale_x_discrete(labels=rev(c("Aff_type", "Mi_necromass", "Lig", "Soil", "Microbe")))+
  ylab(expression(paste("Recative percentage of contribution (%)")))+
  geom_text(aes(x=Group),
            y=MRR.Rel$Lab.Y+4,
            label=MRR.Rel$Lab.Y,
            size=6,family ="serif",color="black")+
  geom_text(aes(x=Group),
            y=MRR.Rel$Lab.Y+6.5,
            label="%",
            size=6,family ="serif",color="black")+
  #scale_fill_manual(values=brewer.pal(9,"GnBu"))+
  #scale_fill_prism(palette = "candy_bright")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black",  fill=NA, linewidth = 0.5,linetype = 1),
        text = element_text(size=14,family ="serif"),
        axis.text.x=element_text(family ="serif",color="black", size=16),
        axis.text.y=element_text(family ="serif",color="black", size=16),
        axis.line = element_line(colour="black"),
        axis.title.y = element_blank(),
        plot.margin=margin(t=5.5, r=5.5, b=5.5, l=5.5, unit = "pt"),
        legend.position="None")+
  annotate("text", x=5.2, y= 51, label="(b)", size=6, family ="serif", color = "black")+
  coord_flip()  # x y 轴互换位置

Fig_MRR.Rel  #(10, 4)

