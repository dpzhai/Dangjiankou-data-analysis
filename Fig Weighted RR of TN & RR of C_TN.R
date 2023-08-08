library(openxlsx)
library(ggplot2)
library(cowplot) # For plot_grid()
library(scales) # for decimal


#****************************************************************************************************************
#*
#*                                      Fig RR of C_TN Relations
#*
#***************************************************************************************************************

RR <- read.xlsx(paste(Path, "Total RR_20230302.xlsx", sep=""), sheet =1)
head(RR)
RR$Restoration <- factor(RR$Restoration, levels = c("CW PO", "CW QV", "CW PM", "SW PO", "SW QV", "SW PM"))


# c("#F9766E", "#B79F00", "#01BA38", "#00BFC4", "#619DFF", "#F564E3")
# ColorValue <- c("#1b9e77","#d95f02", "#7570b3", "#e7298a", "#66a61e", "sky blue")  # 

ColorValue <- c("#F9766E", "#B79F00", "#1b9e77", "#00BFC4", "#619DFF", "#F564E3")
  
P.lm <- lm(POC.yi ~ TN.yi, data = RR)
P.re <- summary(P.lm)

P.RS <- P.re$r.squared

plot(RR$TN.yi, RR$POC.yi)
#*******************************************************  POC_TN
#**************
Fig.POC_N <- ggplot(RR, aes(x = TN.yi, y = POC.yi)) + 
  geom_point(size = 4, aes(colour = Restoration))+
  stat_smooth(aes(x = TN.yi, y = POC.yi),
              method ="lm",se = F, fullrange = F,
              formula = y ~ x,  # size=2,
              color="black",linetype=1)+
  scale_colour_manual(values = ColorValue,  
                      labels = c("CW PO", "CW QV", "CW PM", "SW PO", "SW QV", "SW PM"))+
  scale_y_continuous(limits = c(-2.5, 4), 
                     breaks=c(-2, -1, 0, 1, 2, 3), 
                     labels = number_format(accuracy=0.1, decimal.mark="."),
                     #position = "right", 
                     #sec.axis = dup_axis(),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(-1, 1.35),
                     breaks = c(-0.8, -0.4, 0, 0.4, 0.8, 1.2), 
                     expand = c(0,0))+
  annotate("text", x =0.9, y =-1.3, label = paste("italic(R) ^ 2 ==", format(round(P.RS, digits = 2), nsmall =2)), parse = T,family ="serif", size=4.5)+  #R2  
  # (parse = T 标签并非纯文本)
  #annotate("text", x =75, y =10, label = paste("italic(R) ^ 2 ==", " 0.70"), parse = T,family ="serif", size=4.5)+  # R2
  annotate("text", x = 0.9, y =-1.9, label = paste("italic(P) < 0.01"), parse = TRUE, family ="serif",size=4.5)+ # P value
  annotate("text",family="serif",  x= -0.8, y=3.5, label="(b)", size=5)+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black", fill = 'transparent', size = 0.5),
        axis.text = element_text(family="serif", colour = "black", size = 14),
        axis.title.y = element_text(family="serif", size = 14, colour = "black"), 
        axis.title.x = element_blank(),
        legend.title = element_text(family="serif", size = 14, colour = "black"), 
        legend.text = element_text(family="serif", size = 12, colour ="black"), 
        legend.position = "none", 
        legend.box = "horizontal",
        #legend.box.just = "bottom",
        legend.key=element_blank(),
        legend.spacing.y=unit(0.001, "pt"),
        legend.background =  element_rect(fill = "transparent"),
        plot.margin = margin(t=5.5, r=5.5, b=5.5, l=5.5, unit = "pt")) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1))+  # change the order of legend
  labs(x = expression(paste(italic(RR)," (TN)")), 
       y = expression(paste(italic(RR)," (POC)")), 
       #shape = "Land use",  
       colour = "Restoration") 

Fig.POC_N




#*******************************************************  MAOC_TN 

M.lm <- lm(MAOC.yi ~ TN.yi, data = RR)
M.re <- summary(M.lm)

M.R <- M.re$r.squared

#*****************

Fig.MAOC_N <- ggplot(RR, aes(x = TN.yi, y = MAOC.yi)) + 
  geom_point(size = 4, aes(colour = Restoration))+
  stat_smooth(aes(x = TN.yi, y = MAOC.yi),
              method ="lm",se = F, fullrange = F,
              formula = y ~ x,  # size=2,
              color="black",linetype=1)+
  scale_colour_manual(values = ColorValue,  
                      labels = c("CW PO", "CW QV", "CW PM", "SW PO", "SW QV", "SW PM"))+
  scale_y_continuous(limits = c(-1.5, 2), 
                     breaks=c( -1, -0.5, 0, 0.5, 1, 1.5),  
                     labels = number_format(accuracy=0.1, decimal.mark="."),
                     #position = "right", 
                     #sec.axis = dup_axis(),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(-1, 1.35),
                     breaks = c(-0.8, -0.4, 0, 0.4, 0.8, 1.2), 
                     expand = c(0,0))+
  annotate("text", x =0.9, y =-0.8, label = paste("italic(R) ^ 2 ==", format(round(M.R, digits = 2), nsmall =2)), parse = T,family ="serif", size=4.5)+  #R2  
  # (parse = T 标签并非纯文本)
  #annotate("text", x =75, y =10, label = paste("italic(R) ^ 2 ==", " 0.70"), parse = T,family ="serif", size=4.5)+  # R2
  annotate("text", x = 0.9, y =-1.2, label = paste("italic(P) < 0.01"), parse = TRUE, family ="serif",size=4.5)+ # P value
  annotate("text",family="serif",  x= -0.8, y=1.7, label="(c)", size=5)+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill='transparent', color='black', linewidth = 0.5), 
        #panel.border = element_rect(colour = "black", fill = NA, size = 1.2),
        axis.text = element_text(family="serif", colour = "black", size = 14),
        axis.title = element_text(family="serif", size = 14, colour = "black"), 
        #axis.title.x = element_blank(),
        legend.title = element_text(family="serif", size = 14, colour = "black"), 
        legend.text = element_text(family="serif", size = 12, colour ="black"), 
        legend.position = "none", 
        legend.box = "horizontal",
        #legend.box.just = "bottom",
        legend.key=element_blank(),
        legend.spacing.y=unit(0.0025, "cm"),
        legend.background =  element_rect(fill = "transparent"),
        plot.margin = margin(t=5.5, r=5.5, b=5.5, l=5.5, unit = "pt")) + 
  guides(shape = guide_legend(order = 2),col = guide_legend(order = 1))+  # change the order of legend
  labs(x = expression(paste(italic(RR)," (TN)")), 
       y = expression(paste(italic(RR)," (MAOC)")), 
       #shape = "Land use",  
       colour = "Restoration") 

Fig.MAOC_N

#********************


plot_grid(Fig.POC_N,  Fig.MAOC_N,  
          nrow=2, ncol=1, rel_heights = c(4, 4.2), #  rel_widths =5,  
          align="v") 

#****************************************************************************************************************
#*
#*                                      Fig effect size of TN
#*
#***************************************************************************************************************
#*

Top.RR.. <- read.xlsx(paste(Path, "Weighted RR_20230224.xlsx", sep=""), sheet=3)
head(Top.RR..)

Top.RR..$Species <- factor(Top.RR..$Species, levels = c("PO", "QV", "PM", "Mean"))

Species_color <- c("#F9766E", "#01BA38", "#619DFF", "purple")

#******************  TN top
FigRR..TN <- ggplot(Top.RR.., aes(x=No, y=TN.RR..))+
  geom_point(aes(color=Species), pch=16, size=5)+
  geom_errorbar(aes(x=No, ymin=TN.CI.low, ymax=TN.CI.hi), 
                stat = "identity", position = position_dodge(width=0.4), 
                width = 0.15)+
  geom_hline(yintercept=0, linetype=2, color="grey")+
  scale_color_manual(values = Species_color, name="Group")+
  geom_text(data=Top.RR.., aes(x=No),
            y=Top.RR..$TN.CI.hi+0.4,
            label=Top.RR..$TN.sig,
            size=6,family ="serif",color="black")+
  scale_x_continuous(limits = c(0.5, 9.5),
                     breaks = c(1,3, 4, 5, 7, 8, 9), 
                     # expand = c(0,0),
                     labels=c( "Mean", "PM","QV", "PO", "PM", "QV","PO"))+
  scale_y_continuous(limits = c(-1, 2.5), 
                     breaks=c(-0.5, 0, 0.5, 1, 1.5, 2), 
                     expand = c(0,0))+
  ylab(expression(paste("Response ratio")))+
  theme(text = element_text(size=14,family ="serif"),
        panel.grid = element_blank(),
        panel.background=element_rect(fill='transparent', color='black', linewidth = 0.5),
        axis.text=element_text(size=14,family ="serif",color="black"),
        axis.title.y = element_blank(),
        axis.line = element_line(colour='black', linewidth = 0.5), 
        legend.position = "none", 
        legend.key.height = unit(0.1, "cm"),
        legend.background = element_rect(fill="transparent"),
        legend.box.background = element_rect(fill="transparent", color="transparent"),
        legend.text =element_text(family ="serif",color="black", size=10),
        legend.title =element_text(family ="serif",color="black", size=12),
        legend.key = element_blank(),
        plot.margin = margin(t=5.5, r=5.5, b=5.5, l=5.5, unit = "pt"))+
  annotate("text",family="serif",  x=9.5, y=-0.5, label="(a)", size=5)+
  coord_flip() # Rotate figure

FigRR..TN

plot_grid(FigRR..TN, 
 plot_grid(Fig.POC_N,  Fig.MAOC_N,  
          nrow=2, ncol=1, rel_heights = c(4, 4.2), #  rel_widths =5,  
          align="v"),
 nrow=1, ncol=2, rel_widths =c(3.5, 4), # rel_heights = c(4, 4.2),   
 align="v")   #(7.8, 6.5)

