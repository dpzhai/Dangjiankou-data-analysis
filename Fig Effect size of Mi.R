library(openxlsx)
library(ggplot2)



Mi.RR <- read.xlsx(paste(Path, "Mi RR for Figure.xlsx", sep=""), sheet=1)

FigRR.Mi <- ggplot(Mi.RR, aes(x=No, y=Mean.RR))+
  geom_point(col="blue", pch=16, size=5)+
  geom_errorbar(aes(x=No, ymin=Low, ymax=Hi), 
                stat = "identity", position = position_dodge(width=0.4), 
                width = 0.15)+
  geom_hline(yintercept=0, linetype=2, color="grey")+
  #scale_color_manual(values = Species_color, name="Group")+
  geom_text(data=Mi.RR, aes(x=No),
            y=Mi.RR$Hi+0.2,
            label=Mi.RR$Sig,
            size=6,family ="serif",color="black")+
  scale_x_continuous(limits = c(0.5, 16.5),
                     breaks = c(1:16), 
                     # expand = c(0,0),
                     labels=rev(c(Mi.RR$Variable)))+
  scale_y_continuous(limits = c(-0.7, 2), 
                     breaks=c(-0.5, 0, 0.5, 1, 1.5), 
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
  #annotate("text",family="serif",  x=16.5, y=-0.5, label="(a)", size=5)+
  coord_flip() # Rotate figure

FigRR.Mi
