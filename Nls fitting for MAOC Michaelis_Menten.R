library(openxlsx)



Soil <- read.xlsx(paste(Path, "Soil whole.xlsx", sep=""), sheet="Whole1")

# Data splitting
TSoil <- Soil[which(Soil$Layer=="Top"),]  # Top layer
SSoil <- Soil[which(Soil$Layer=="Sub"),]  # Sub layer

TWSoil <- TSoil[which(TSoil$Land_use=="W"),]   # Top W
TCSoil <- TSoil[which(TSoil$Land_use=="CK"),]  # Top CK

SWSoil <- SSoil[which(SSoil$Land_use=="W"),]   # Sub W
SCSoil <- SSoil[which(SSoil$Land_use=="CK"),]  # Sub CK


#  Non-linear fitting
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

#***********  SW
M.SW <- nls(MAOC~M*SOC/(K+SOC), data = SWSoil, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
M.SW_re <- summary(M.SW, rsq=T)
M.SW_re$coefficients


M.SW.Fitted <- M.SW$m$predict()
M.SW.lm <- lm(M.SW.Fitted ~ SWSoil$MAOC)
M.SW.lmRe <- summary(M.SW.lm)  

#***********  SC
M.SC <- nls(MAOC~M*SOC/(K+SOC), data = SCSoil, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
M.SC_re <- summary(M.SC, rsq=T)
M.SC_re$coefficients


M.SC.Fitted <- M.SC$m$predict()
M.SC.lm <- lm(M.SC.Fitted ~ SCSoil$MAOC)
M.SC.lmRe <- summary(M.SC.lm)


nlRe <- list("Top W" = M.TW_re$coefficients,  
             "Top CK" = M.TC_re$coefficients, 
             "Sub W" = M.SW_re$coefficients, 
             "Sub CK" = M.SC_re$coefficients) 


write.xlsx(nlRe, paste(Path, "MAOC saturation nls1.xlsx", sep=""), rowNames=T)

#**************************************************************************** Further splitting
#*

SplData <- split(Soil, ~ Layer + Former_use + Land_use)
SplData$Sub.CW.CK
SplData$Top.CW.CK

#************************************  Top
#***********  T_CW_W
CW.TW <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Top.CW.W, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
CW.TW_re <- summary(CW.TW, rsq=T)
CW.TW_re$coefficients

CW.TW.Fitted <- CW.TW$m$predict()
CW.TW.lm <- lm(CW.TW.Fitted ~ SplData$Top.CW.W$MAOC)
CW.TW.lmRe <- summary(CW.TW.lm) 

#***********  T_CW_C
CW.TC <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Top.CW.CK, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
CW.TC_re <- summary(CW.TC, rsq=T)
CW.TC_re$coefficients


CW.TC.Fitted <- CW.TC$m$predict()
CW.TC.lm <- lm(CW.TC.Fitted ~ SplData$Top.CW.CK$MAOC)
CW.TC.lmRe <- summary(CW.TC.lm) 

plot(SplData$Top.CW.CK$SOC, SplData$Top.CW.CK$MAOC)

#***********  T_SW_W
SW.TW <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Top.SW.W, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
SW.TW_re <- summary(SW.TW, rsq=T)
SW.TW_re$coefficients

SW.TW.Fitted <- SW.TW$m$predict()
SW.TW.lm <- lm(SW.TW.Fitted ~ SplData$Top.SW.W$MAOC)
SW.TW.lmRe <- summary(SW.TW.lm) 

#***********  T_SW_C
SW.TC <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Top.SW.CK, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
SW.TC_re <- summary(SW.TC, rsq=T)
SW.TC_re$coefficients


SW.TC.Fitted <- SW.TC$m$predict()
SW.TC.lm <- lm(SW.TC.Fitted ~ SplData$Top.SW.CK$MAOC)
SW.TC.lmRe <- summary(SW.TC.lm) 
#************************************  Sub
#***********  S_CW_W
CW.SW <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Sub.CW.W, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
CW.SW_re <- summary(CW.SW, rsq=T)
CW.SW_re$coefficients

CW.SW.Fitted <- CW.SW$m$predict()
CW.SW.lm <- lm(CW.SW.Fitted ~ SplData$Sub.CW.W$MAOC)
CW.SW.lmRe <- summary(CW.SW.lm) 

#***********  S_CW_Control
CW.SC <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Sub.CW.CK, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
CW.SC_re <- summary(CW.SC, rsq=T)
CW.SC_re$coefficients


CW.SC.Fitted <- CW.SC$m$predict()
CW.SC.lm <- lm(CW.SC.Fitted ~ SplData$Sub.CW.CK$MAOC)
CW.SC.lmRe <- summary(CW.SC.lm) 

plot(SplData$Sub.CW.CK$SOC, SplData$Sub.CW.CK$MAOC)

#***********  S_SW_W
SW.SW <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Sub.SW.W, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
SW.SW_re <- summary(SW.SW, rsq=T)
SW.SW_re$coefficients

SW.SW.Fitted <- SW.SW$m$predict()
SW.SW.lm <- lm(SW.SW.Fitted ~ SplData$Sub.SW.W$MAOC)
SW.SW.lmRe <- summary(SW.SW.lm) 

#***********  S_SW_C
SW.SC <- nls(MAOC~M*SOC/(K+SOC), data = SplData$Sub.SW.CK, start = list(M=1, K=1), lower = c(M=0, K=0), algorithm = "port")
SW.SC_re <- summary(SW.SC, rsq=T)
SW.SC_re$coefficients


SW.SC.Fitted <- SW.SC$m$predict()
SW.SC.lm <- lm(SW.SC.Fitted ~ SplData$Sub.SW.CK$MAOC)
SW.SC.lmRe <- summary(SW.SC.lm) 


#**************************************
nlRe2 <- list("Top CW.W" = CW.TW_re$coefficients,  
             "Top CW.C" = CW.TC_re$coefficients, 
             "Top SW.W" = SW.TW_re$coefficients,  
             "Top SW.C" = SW.TC_re$coefficients, 
             "Sub CW.W" = CW.SW_re$coefficients,  
             "Sub CW.C" = CW.SC_re$coefficients, 
             "Sub SW.W" = SW.SW_re$coefficients,  
             "Sub SW.C" = SW.SC_re$coefficients) 


write.xlsx(nlRe2, paste(Path, "MAOC saturation nls2.xlsx", sep=""), rowNames=T)
