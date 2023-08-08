library(openxlsx) # for reading and writing excel
library(ggplot2)
library(cowplot) # For plot_grid()



# Trend <- read.xlsx(paste(Path, "C change rate.xlsx", sep=""), sheet=2)
# head(Trend)



#Trend$Land_use <- factor(Trend$Land_use, levels = c("W", "CK"))


POC.CK <- function(x) {2*0.0053*x+0.1565}
POC.W <- function(x) {2*0.0013*x+0.268}


par(mfrow = c(2, 1) ,  family = "serif", ps=14, mai= c(0.8,1.2, 0.05,0.1))

curve(POC.CK, from = 6, to = 90, xlab = "SOC", ylab = "(POC/SOC)' ", lty=2) 
curve(POC.W, from = 6, to = 90, xlab = "SOC", ylab = "(POC/SOC)' ", lty=1, add=T)

MAOC.W <- function(x) {41959.3443/(223.9220+x)^2}
MAOC.CK <- function(x) {5049.8990/(72.5928+x)^2}

curve(MAOC.CK, from = 6, to = 90, xlab = "SOC", ylab = "(MAOC/SOC)' ", lty=2) 
curve(MAOC.W, from = 6, to = 90, xlab = "SOC", ylab = "(MAOC/SOC)' ", lty=1, add=T)


# (5.5, 7.5)