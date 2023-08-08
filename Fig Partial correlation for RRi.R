library(openxlsx)
library(ggplot2)



P.Pcor <- read.xlsx(paste(Path, "Partial correlation for P.RR_20230325.xlsx", sep=""), sheet=2)
M.Pcor <- read.xlsx(paste(Path, "Partial correlation for M.RR_20230325.xlsx", sep=""), sheet=2)

#**************************
P.Pcor$Control <- factor(P.Pcor$Control, levels = c("Zero_order", "Soil", "Microbe", "Lignin", "Mi_necromass"))
M.Pcor$Control <- factor(M.Pcor$Control, levels = c("Zero_order", "Soil", "Microbe", "Lignin", "Mi_necromass"))

P.Pcor$X.variable <- factor(P.Pcor$X.variable, levels = rev(c("TN", "pH",  "Bacteria",  "GP", "GN",  "Act", "Ana", "F.B", "BG", "PHO", "PEO", 
                                                          "Cinnamyl", "Syringyl", "BNC")))

M.Pcor$X.variable <- factor(M.Pcor$X.variable, levels = rev(c("TN", "pH",  "NH4", "MBC",  "GP", "Act", "Ana", "BG", "PHO",  
                                                          "Vanillyl", "Syringyl", "FNC")))

#****************

Fig.P.Pcor <- ggplot(P.Pcor, aes(x = Control, y = X.variable)) +
  geom_tile(aes(fill = estimate))+ 
  scale_fill_gradientn(colors = c('blue', 'grey95', 'red'), limit = c(-1, 1)) +  #自定义热图渐变填充色，例如负相关蓝色，正相关红色
  geom_text(aes(label = Label), size = 5, family ="serif") +  #在热图中展示指定列的标签文字
  #facet_grid(~variable2) + 
  theme(panel.grid = element_blank(),
        text = element_text(size=18,family ="serif"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = 'black')) +  #去除了刻度轴线条，并调整了坐标轴字体的风格
  labs(x = '\nPartial Correlation control', y = '', fill = "(Partial)\nCorrelations\n\nSpearman's r") +  #坐标轴标题和图例标题
  scale_x_discrete(expand = c(0, 0)) +  #以下两句用于坐标轴紧贴热图
  scale_y_discrete(expand = c(0, 0))
Fig.P.Pcor #(7.5, 5)


Fig.M.Pcor <- ggplot(M.Pcor, aes(x = Control, y = X.variable)) +
  geom_tile(aes(fill = estimate))+ 
  scale_fill_gradientn(colors = c('blue', 'grey95', 'red'), limit = c(-1, 1)) +  #自定义热图渐变填充色，例如负相关蓝色，正相关红色
  geom_text(aes(label = Label), size = 5, family ="serif") +  #在热图中展示指定列的标签文字
  #facet_grid(~variable2) + 
  theme(panel.grid = element_blank(),
        text = element_text(size=18,family ="serif"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(color = 'black')) +  #去除了刻度轴线条，并调整了坐标轴字体的风格
  labs(x = '\nPartial Correlation control', y = '', fill = "(Partial)\nCorrelations\n\nSpearman's r") +  #坐标轴标题和图例标题
  scale_x_discrete(expand = c(0, 0)) +  #以下两句用于坐标轴紧贴热图
  scale_y_discrete(expand = c(0, 0))
Fig.M.Pcor   #(7.4, 5)
