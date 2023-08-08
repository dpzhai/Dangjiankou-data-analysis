library(openxlsx)
library(ggplot2)
library(ggpubr)
library(ggsignif)#
library(tidyverse)
library(ggprism)#
library(vioplot)#
library(ggbeeswarm)
library(cowplot) # For plot_grid()
library(dplyr)  # for bind_cols() and %>%



TopSoil <- read.xlsx(paste(Path, "Soil top_20230221.xlsx", sep=""), sheet=1)
head(TopSoil)


TopSoil$Restoration <- factor(TopSoil$Restoration, levels = c("CW_PO", "CW_QV", "CW_PM", "SW_PO", "SW_QV", "SW_PM"))

Sig <- read.xlsx(paste(Path, "Sig of SOC.POC & MAOC means.xlsx", sep=""), sheet=2)

Sig$Restoration <- factor(Sig$Restoration, levels = c("CW_PO", "CW_QV", "CW_PM", "SW_PO", "SW_QV", "SW_PM"))

Treatment_color <- c("#ff7f00", "#4daf4a")

#********************************************************************** Useless **************************************************************
#*************************************************************
#*# Maximum values in each group (to locate the sig labels)
#*
Max <- TopSoil%>%
  group_by(Restoration, Land_use)%>%
  summarise(across(SM:MNC, max, na.rm=T))
#write.xlsx(Max, paste(Path, "Max of each group.xlsx", sep=""))
#************************************************************* Not use

Fig.POC <- ggplot(TopSoil, aes(x=Restoration, y=POC, colour=Land_use)) + 
  geom_violin(width = 0.8)+
  geom_beeswarm(groupOnX = Land_use, dodge.width =0.8)+  #蜜蜂点
  theme_classic() +
  geom_boxplot(alpha=1, outlier.size=0, size=0.3, width=0.2, position = position_dodge(width=0.8))+
  stat_summary(fun="mean",geom="point",shape=27, size=2, # fill="blue", 
               position = position_dodge(width=0.8))+
  scale_color_manual(values = Treatment_color, name="Land use", labels=c("Control", "Woodland"))
  

Fig.POC

#********************************************************************** Useless ending  ************************************************************

#*******************************************************************************   Fig use
#**********SOC
Fig.SOC <- ggplot(TopSoil, aes(x=Restoration, y=SOC, fill=Land_use)) + 
  geom_violin(width = 0.8, alpha=0.5)+
  geom_vline(xintercept=3.5, linetype=2, color="grey")+
  #geom_beeswarm(groupOnX = Land_use, dodge.width =0.8)+
  #theme_classic() +
  geom_boxplot(alpha=1, outlier.size=0, size=0.3, width=0.1, position = position_dodge(width=0.8))+
  stat_summary(fun="mean",geom="point",shape=18, size=3, #fill="blue", 
               position = position_dodge(width=0.8))+
  scale_fill_manual(values = Treatment_color, name="Land use", labels=c("Control", "Woodland"))+
  geom_text(data=Sig, aes(x=Sig.x1, y=Sy),
            label=Sig$Sig.s,
            size=4,family ="serif",color="black")+
  geom_text(data=Sig, aes(x=Restoration, y=Sig.s.y),
            # y=Sig$MAOC+Sig$MAOC.se+1,
            label=Sig$Sig.s.use,
            size=4,family ="serif",color="black")+
  scale_y_continuous(limits = c(-2, 100), 
                     breaks=c(0, 25, 50, 75),  
                     #position = "right", 
                     #sec.axis = dup_axis(),
                     expand = c(0,0))+
  scale_x_discrete(labels=c("PO", "QV", "PM", "PO", "QV", "PM"))+
  ylab(expression(paste("SOC (mg kg"^-1," soil)")))+
  annotate("text",family="serif",  x=0.6, y=95, label="(a)", size=5)+
  theme(text = element_text(size=14,family ="serif"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill='transparent', color='black', linewidth =0.5),
        axis.text.y=element_text(size=14,family ="serif",color="black"),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.line = element_line(colour='black', linewidth =0.5), 
        axis.ticks.length.x = unit(0, "cm"),
        legend.position = c(0.9, 0.8),
        legend.key.height = unit(0.1, "cm"),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(fill="transparent", color="transparent"),
        legend.text =element_text(family ="serif",color="black", size=10),
        legend.title =element_text(family ="serif",color="black", size=12),
        legend.key = element_blank(),
        plot.margin = margin(t=5.5, r=5.5, b=5.5, l=5.5, unit = "pt"))


Fig.SOC


#**********POC
Fig.POC <- ggplot(TopSoil, aes(x=Restoration, y=POC, fill=Land_use)) + 
  geom_violin(width = 0.8, alpha=0.5)+
  geom_vline(xintercept=3.5, linetype=2, color="grey")+
  #geom_beeswarm(groupOnX = Land_use, dodge.width =0.8)+
  #theme_classic() +
  geom_boxplot(alpha=1, outlier.size=0, size=0.3, width=0.1, position = position_dodge(width=0.8))+
  stat_summary(fun="mean",geom="point",shape=18, size=3, # fill="blue", 
               position = position_dodge(width=0.8))+
  scale_fill_manual(values = Treatment_color, name="Land use", labels=c("Control", "Woodland"))+
  geom_text(data=Sig, aes(x=Sig.x1, y=Py),
            label=Sig$Sig.p,
            size=4,family ="serif",color="black")+
  geom_text(data=Sig, aes(x=Restoration, y=Sig.p.y),
            # y=Sig$MAOC+Sig$MAOC.se+1,
            label=Sig$Sig.p.use,
            size=4,family ="serif",color="black")+
  scale_y_continuous(limits = c(-2, 40), 
                     breaks=c(0, 10, 20, 30),  
                     #position = "right", 
                     #sec.axis = dup_axis(),
                     expand = c(0,0))+
  scale_x_discrete(labels=c("PO", "QV", "PM", "PO", "QV", "PM"))+
  ylab(expression(paste("POC (mg kg"^-1," soil)")))+
  annotate("text",family="serif",  x=0.6, y=38, label="(b)", size=5)+
  theme(text = element_text(size=14,family ="serif"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill='transparent', color='black', linewidth =0.5),
        axis.text.y=element_text(size=14,family ="serif",color="black"),
        axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.line = element_line(colour='black', linewidth =0.5), 
        axis.ticks.length.x = unit(0, "cm"),
        legend.position = "none",
        legend.key.height = unit(0.1, "cm"),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(fill="transparent", color="transparent"),
        legend.text =element_text(family ="serif",color="black", size=10),
        legend.title =element_text(family ="serif",color="black", size=12),
        legend.key = element_blank(),
        plot.margin = margin(t=-5.5, r=5.5, b=5.5, l=5.5, unit = "pt"))


Fig.POC


#*******MAOC
Fig.MAOC <- ggplot(TopSoil, aes(x=Restoration, y=MAOC, fill=Land_use)) + 
  geom_violin(width = 0.8, alpha=0.5)+
  geom_vline(xintercept=3.5, linetype=2, color="grey")+
  #geom_beeswarm(groupOnX = Land_use, dodge.width =0.8)+
  #theme_classic() +
  geom_boxplot(alpha=1, outlier.size=0, size=0.3, width=0.1, position = position_dodge(width=0.8))+
  stat_summary(fun="mean",geom="point",shape=18, size=3, # fill="blue", 
               position = position_dodge(width=0.8))+
  scale_fill_manual(values = Treatment_color, name="Land use", labels=c("Control", "Woodland"))+
  geom_text(data=Sig, aes(x=Sig.x1, y=My),
            label=Sig$Sig.m,
            size=4,family ="serif",color="black")+
  geom_text(data=Sig, aes(x=Restoration, y=Sig.m.y),
            # y=Sig$MAOC+Sig$MAOC.se+1,
            label=Sig$Sig.m.use,
            size=4,family ="serif",color="black")+
  scale_y_continuous(limits = c(-2, 70), 
                     breaks=c(0, 20, 40, 60),  
                     #position = "right", 
                     #sec.axis = dup_axis(),
                     expand = c(0,0))+
  scale_x_discrete(labels=c("PO", "QV", "PM", "PO", "QV", "PM"))+
  ylab(expression(paste("MAOC (mg kg"^-1," soil)")))+
  annotate("text",family="serif",  x=0.6, y=66, label="(c)", size=5)+
  theme(text = element_text(size=14,family ="serif"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill='transparent', color='black', linewidth =0.5),
        axis.text=element_text(size=14,family ="serif",color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
        axis.line = element_line(colour='black', linewidth =0.5), 
        axis.ticks.length.x = unit(0, "cm"),
        legend.position = "none",
        legend.key.height = unit(0.1, "cm"),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(fill="transparent", color="transparent"),
        legend.text =element_text(family ="serif",color="black", size=10),
        legend.title =element_text(family ="serif",color="black", size=12),
        legend.key = element_blank(),
        plot.margin = margin(t=-5.5, r=5.5, b=5.5, l=5.5, unit = "pt"))



Fig.MAOC


plot_grid(Fig.SOC, Fig.POC, Fig.MAOC, 
          ncol=1, rel_heights = c(3, 2.9, 3.2),  
          align="v", axis="tblr")  #(8, 5.5)
