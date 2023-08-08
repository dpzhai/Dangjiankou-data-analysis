library(openxlsx)
library(rdacca.hp)
library(vegan)
library(fastDummies)

library(hier.part)

RRi <- read.xlsx(paste(Path, " RRi with dummy.xlsx", sep=""), sheet=2)

colnames(RRi)


#**************************************  POC.yi  HP
# C <- c("POC.yi", "MAOC.yi", "SOC.yi")
P.Soil <- c("TN.yi", "PH.yi")
P.Microbe <- c("Bacteria.yi", "GP.yi", "GN.yi", "Act.yi", "Ana.yi", "F.B.yi", "BG.yi", "PHO.yi", "PEO.yi")
P.Lignin <- c("Cinnamyl.yi", "Syringyl.yi")
P.AM <-"BNC.yi"
P.Aff <- c("Former_use", "Species_PM", "Species_PO")

P.Select <- c(P.Aff, P.Soil, P.Microbe, P.Lignin, P.AM)


# P.RR

POC.RR <- RRi[,  "POC.yi"]
EnvPrr.use <- RRi[, P.Select]

PRR.fit <- rda(formula = POC.RR ~ Former_use + Species_PM + Species_PO + PH.yi + TN.yi + 
                 Bacteria.yi + GP.yi + GN.yi + Act.yi + Ana.yi + F.B.yi + BG.yi + PHO.yi + PEO.yi +
                 Cinnamyl.yi + Syringyl.yi +
                 BNC.yi, 
              data = RRi)
summary(PRR.fit)
vif.cca(PRR.fit)
anova(PRR.fit, by = "axis")

plot(PRR.fit)


rdacca.hp(dv=POC.RR, iv=EnvPrr.use, method="RDA", type = "R2")


#**************************************   MAOC.yi  HP

M.Soil <- c("TN.yi", "PH.yi", "NH4.yi")
M.Microbe <- c("MBC.yi", "GP.yi",	"Act.yi", "Ana.yi", "BG.yi", "PHO.yi")
M.Lignin <- c("Vanillyl.yi",  "Syringyl.yi")
M.AM <- "FNC.yi"
M.Aff <- c("Former_use", "Species_PM", "Species_PO")

M.Select <- c(M.Aff, M.Soil, M.Microbe, M.Lignin, M.AM)


# M.RR

MAOC.RR <- RRi[,  "MAOC.yi"]
EnvMrr.use <- RRi[, M.Select]

MRR.fit <- rda(formula = MAOC.RR ~ Former_use + Species_PM + Species_PO + PH.yi + TN.yi + NH4.yi +
                 MBC.yi + GP.yi + Act.yi + Ana.yi + BG.yi + PHO.yi +
                 Vanillyl.yi + Syringyl.yi +
                 FNC.yi, 
               data = RRi)
summary(MRR.fit)
vif.cca(MRR.fit)
anova(MRR.fit, by = "axis")

plot(MRR.fit)


rdacca.hp(dv=MAOC.RR, iv=EnvMrr.use, method="RDA", type = "R2")



