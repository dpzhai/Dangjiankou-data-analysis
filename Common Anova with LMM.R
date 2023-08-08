library(openxlsx)
library(dplyr)  # for bind_cols() and %>%
library(lme4)
library(lmerTest)
library(emmeans)    


TopSoil <- read.xlsx(paste(Path, "Soil top_20230221.xlsx", sep=""), sheet=1)
head(TopSoil)

#*************************************************************  Statistics

TopSoil.N <- TopSoil[, c(3:7, 11:13)]
head(TopSoil.N)

TopSoil.N.log <- log10(TopSoil.N[,6:8])
TopSoil.log <- bind_cols(TopSoil.N[, 1:5], TopSoil.N.log) 
head(TopSoil.log)

# ***************  For POC
POC.LMM <- lmer(POC ~ Former_use*Land_use*Species+(1|Area), data = TopSoil.log)
POC.LM <- lm(POC ~ Former_use*Land_use*Species, data = TopSoil.log)
POC.Random <- anova(POC.LMM, POC.LM)
POC.Fixed <- anova(POC.LMM) 
POC.Pairwise <-  emmeans(POC.LMM, pairwise ~ Former_use*Land_use*Species)

ranova(POC.LMM)
anova(POC.LMM, POC.LM, refit=T)
anova(POC.LMM, POC.LM, refit=F)
# ***************  For MAOC
MAOC.LMM <- lmer(MAOC ~ Former_use*Land_use*Species+(1|Area), data = TopSoil.log)
MAOC.LM <- lm(MAOC ~ Former_use*Land_use*Species, data = TopSoil.log)
MAOC.Random <- anova(MAOC.LMM, MAOC.LM)
MAOC.Fixed <- anova(MAOC.LMM) 
MAOC.Pairwise <-  emmeans(MAOC.LMM, pairwise ~ Former_use*Land_use*Species)


# ***************  For SOC

SOC.LMM <- lmer(SOC ~ Former_use*Land_use*Species+(1|Area), data = TopSoil.log)
SOC.LM <- lm(SOC ~ Former_use*Land_use*Species, data = TopSoil.log)
SOC.Random <- anova(SOC.LMM, SOC.LM)
SOC.Fixed <- anova(SOC.LMM) 
SOC.Pairwise <-  emmeans(SOC.LMM, pairwise ~ Former_use*Land_use*Species)

Dataset_names <- list ('Sheet1' = SOC.Random, 'Sheet2' = SOC.Fixed, 
                       'Sheet3' = SOC.Pairwise$emmeans, 'Sheet4' = SOC.Pairwise$contrasts,
                       'Sheet5' = POC.Random, 'Sheet6' = POC.Fixed, 
                       'Sheet7' = POC.Pairwise$emmeans, 'Sheet8' = POC.Pairwise$contrasts,
                       'Sheet9' = MAOC.Random, 'Sheet10' = MAOC.Fixed, 
                       'Sheet11' = MAOC.Pairwise$emmeans, 'Sheet12' = MAOC.Pairwise$contrasts) 
write.xlsx(Dataset_names, paste(Path, "Common Lmer results for ANOVA test.xlsx", sep=""), rowNames=T) 


