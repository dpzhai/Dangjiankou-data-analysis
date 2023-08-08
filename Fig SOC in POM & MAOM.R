library(openxlsx) # for reading and writing excel
library(ggplot2)
library(cowplot) # For plot_grid()



Soil <- read.xlsx(paste(Path, "Soil whole.xlsx", sep=""), sheet="Whole1")

TopSoil <- Soil[which(Soil$Layer=="Top"),]  # Top layer
head(TopSoil)



Select <- c("Soil_No", "Area", "Restoration","Species",  "Former_use", "Land_use", "POC", "MAOC", "SOC", "TN")


TopSoil$Land_use <- factor(TopSoil$Land_use, levels = c("W", "CK"))
TopSoil$Restoration <- factor(TopSoil$Restoration, levels = c("CW_PO", "CW_QV", "CW_PM", "SW_PO", "SW_QV", "SW_PM"))


ColorValue <- c("#F9766E", "#B79F00", "#1b9e77", "#00BFC4", "#619DFF", "#F564E3")
ShapeValue <- c(16,2)

#***********    Top
TS <- TopSoil[, Select]
TS.CK <- TopSoil[which(TopSoil$Land_use=="CK"), Select]
TS.W <- TopSoil[which(TopSoil$Land_use=="W"), Select] 
#***************************************************************************************************************************
#*
#*                                  POC ~ SOC
#*
#*
#**********************************************************Plot try
plot(TS$SOC, TS$POC)
plot(TS$SOC, TS$MAOC)
# identify(TS$SOC, TS$MAOC)


POClm <- lm(POC~SOC, data=TS)
plot(POClm)
anova(POClm)
summary(POClm)

#*******************************************************   Top
#  Non-linear fitting  CK
TPOCm.CK <- nls(POC~a*SOC^2+b*SOC+c, data=TS.CK,start = list(a=1, b=1, c=1))
TPOCre.CK <- summary(TPOCm.CK, rsq=T)
TPOCre.CK$coefficients

TP.Fitted.CK <- TPOCm.CK$m$predict()
TP.lm.CK <- lm(TP.Fitted.CK ~TS.CK$POC)
TP.lmRe.CK <- summary(TP.lm.CK)  # R-squared: 0.8122  p-value: < 2.2e-16
TP.RS.CK <- TP.lmRe.CK$r.squared

#  Non-linear fitting  W
TPOCm.W <- nls(POC~a*SOC^2+b*SOC+c, data=TS.W,start = list(a=1, b=1, c=1))
TPOCre.W <- summary(TPOCm.W, rsq=T)
TPOCre.W$coefficients

TP.Fitted.W <- TPOCm.W$m$predict()
TP.lm.W <- lm(TP.Fitted.W ~TS.W$POC)
TP.lmRe.W <- summary(TP.lm.W)  # R-squared:  0.8192  p-value: < 2.2e-16
TP.RS.W <- TP.lmRe.W$r.squared


#*******************************************************
Fig.TPOC <- ggplot(TS, aes(x = SOC, y = POC, group=Land_use)) + 
  geom_point(size = 4, aes(colour = Restoration,  shape = Land_use))+
  stat_smooth(aes(x = SOC, y = POC, linetype=Land_use),
              method ="nls",se = F, fullrange = F,
              method.args = list(formula = y ~ a*x^2+b*x+c, start=list(a=1, b=1, c=1)),  # size=2,
              color="black")+
  scale_colour_manual(values = ColorValue,  
                      labels = c("CW  PO", "CW  QV", "CW  PM", "SW  PO", "SW  QV", "SW  PM"))+
  scale_shape_manual(values = ShapeValue,
                     labels = c("Woodland", "Control"))+
  scale_linetype_manual(values = c(1, 2),
                        labels = c("Woodland", "Control"))+
  scale_y_continuous(limits = c(-3, 40), 
                     breaks=c(0, 10, 20, 30, 40),  
                     #position = "right", 
                     #sec.axis = dup_axis(),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(-4.5, 90),
                     breaks = c(0, 20, 40, 60, 80), 
                     expand = c(0,0))+
  #annotate("text", x =75, y =10, label = paste("italic(R) ^ 2 ==", format(round(TP.RS.CK, digits = 2), nsmall =2)), parse = T,family ="serif", size=4.5)+  #R2  
  # (parse = T 标签并非纯文本)
  #annotate("text", x =75, y =12, label = paste("italic(R) ^ 2 ==", format(round(TP.RS.W, digits = 2), nsmall =2)), parse = T,family ="serif", size=4.5)+  #R2  
  #annotate("text", x =75, y =10, label = paste("italic(R) ^ 2 ==", " 0.70"), parse = T,family ="serif", size=4.5)+  # R2
 # annotate("text", x = 75, y =6, label = paste("italic(P) < 0.01"), parse = TRUE, family ="serif",size=4.5)+ # P value
  annotate("text",family="serif",  x=2.5, y=38, label="(a)", size=5)+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
        axis.text.y = element_text(family="serif", colour = "black", size = 14),
        axis.text.x = element_blank(),
        axis.title.y = element_text(family="serif", size = 14, colour = "black"), 
        axis.title.x = element_blank(),
        axis.ticks.length.x = unit(-3.75, "pt"),
        legend.title = element_text(family="serif", size = 14, colour = "black"), 
        legend.text = element_text(family="serif", size = 12, colour ="black"), 
        legend.position = c(0.4, 0.75), 
        legend.box = "horizontal",
        #legend.box.just = "bottom",
        legend.key=element_blank(),
        #legend.spacing.y=unit(0.0025, "cm"),
        legend.key.width = unit(2, "cm"),
        legend.background =  element_rect(fill = "transparent"),
        plot.margin = margin(t=8, r=5.5, b=0, l=5.5, unit = "pt")) + 
  guides(shape = guide_legend(order = 2), col = guide_legend(order = 1), lty =guide_legend(order = 2))+  # change the order of legend
  labs(x = expression(paste("SOC (g kg"^-1, ")")), 
       y = expression(paste("POC (g kg"^-1, ")")), 
       shape = "Land use",  colour = "Restoration", linetype="Land use") 



Fig.TPOC


#***************************************************************************************************************************
#*
#*                                  MAOC ~ SOC
#*
#*
#*******************************************************   Top
#*******  Non-linear fitting Top MAOC
#***********  TW
M.TW <- nls(MAOC~M*SOC/(K+SOC), data = TWSoil, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
M.TW_re <- summary(M.TW, rsq=T)
M.TW_re$coefficients


M.TW.Fitted <- M.TW$m$predict()
M.TW.lm <- lm(M.TW.Fitted ~ TWSoil$MAOC)
M.TW.lmRe <- summary(M.TW.lm)  

#***********  TC
M.TC <- nls(MAOC~M*SOC/(K+SOC), data = TCSoil, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
M.TC_re <- summary(M.TC, rsq=T)
M.TC_re$coefficients


M.TC.Fitted <- M.TC$m$predict()
M.TC.lm <- lm(M.TC.Fitted ~ TCSoil$MAOC)
M.TC.lmRe <- summary(M.TC.lm) 

#*********************************

Fig.TMAOC <- ggplot(TS, aes(x = SOC, y = MAOC)) + 
  geom_point(size = 4, aes(colour = Restoration,  shape = Land_use))+
  stat_smooth(aes(x = SOC, y = MAOC, linetype=Land_use),
              method ="nls",se = F, fullrange = F,
              method.args = list(formula = y ~ a*x/(b+x), start=list(a=1, b=1), lower = c(a=0, b=0)),  # size=2,
              color="black")+
  scale_colour_manual(values = ColorValue, 
                      labels = c("CW  PO", "CW  QV", "CW  PM", "SW  PO", "SW  QV", "SW  PM"))+
  scale_shape_manual(values = ShapeValue, 
                     labels = c("Woodland", "Control"))+
  scale_linetype_manual(values = c(1, 2), 
                        labels = c("Woodland", "Control"))+
  scale_y_continuous(limits = c(-3, 60), 
                     breaks=c(0, 10, 20, 30, 40, 50),   
                     #position = "right", 
                     #sec.axis = dup_axis(),
                     expand = c(0,0))+
  scale_x_continuous(limits = c(-4.5, 90),
                     breaks = c(0, 20, 40, 60, 80), 
                     expand = c(0,0))+
  annotate("text",family="serif",  x=2.5, y=57, label="(b)", size=5)+
  theme(panel.background = element_blank(), 
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2),
        axis.text = element_text(family="serif", colour = "black", size = 14), 
        axis.title = element_text(family="serif", size = 14, colour = "black"), 
        legend.title = element_text(family="serif", size = 14, colour = "black"), 
        legend.text = element_text(family="serif", size = 12, colour ="black"), 
        legend.position = "none", 
        legend.key=element_blank(),
        #legend.spacing.y=unit(0.005, "cm"),
        legend.background =  element_rect(fill = "transparent"),
        legend.key.width = unit(2, "cm"),
        plot.margin = margin(t=-1.2, r=5.5, b=5.5, l=5.5, unit = "pt")) + 
  labs(x = expression(paste("SOC (g kg"^-1,")")), 
       y = expression(paste("MAOC (g kg"^-1,")")), 
       colour = "Restoration", shape = "Land use", linetype="Land use") 

Fig.TMAOC



plot_grid(Fig.TPOC, Fig.TMAOC, 
          ncol=1, align ="V", rel_heights = c(3.2, 3.5), 
           axis="tblr") # (6.5, 8) Portrait

