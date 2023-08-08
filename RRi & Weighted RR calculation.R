library(openxlsx) # for reading and writing excel
library(dplyr)  # for bind_cols() and %>%
library(metafor)




TopSoil <- read.xlsx(paste(Path, "Soil top_20230221.xlsx", sep=""), sheet=1)
head(TopSoil)


Means_Area <- read.xlsx(paste(Path, "Means new for RR_20230224.xlsx", sep=""), sheet =1) 

W <- Means_Area[which(Means_Area$Land_use=="W"),]
CK <- Means_Area[which(Means_Area$Land_use=="CK"),]



##calculate response ratio and corresponding sampling variances  
#  dat<-escalc(m1i=Me, sd1i=Se, n1i=Ne, m2i=Mc, sd2i=Sc, n2i=Nc, measure="ROM", data=mydata, append=TRUE)


#*********          yi & vi
# 1 POC
POC.RR <- escalc(measure = "ROM", n1i = W$POC.n, n2i = CK$POC.n, m1i = W$POC, # "ROM" for the log transformed ratio of means
                 m2i = CK$POC, sd1i = W$POC.sd, sd2i = CK$POC.sd, replace = T)

# 2 MAOC
MAOC.RR <- escalc(measure = "ROM", n1i = W$MAOC.n, n2i = CK$MAOC.n, m1i = W$MAOC, # "ROM" for the log transformed ratio of means
                  m2i = CK$MAOC, sd1i = W$MAOC.sd, sd2i = CK$MAOC.sd, replace = T)

# 3 SOC
SOC.RR <- escalc(measure = "ROM", n1i = W$SOC.n, n2i = CK$SOC.n, m1i = W$SOC, # "ROM" for the log transformed ratio of means
                 m2i = CK$SOC, sd1i = W$SOC.sd, sd2i = CK$SOC.sd, replace = T)



##meta-anaylsis, calculate the weighted response ratio of each biomes
POC.res <- rma.uni(yi,vi, weights=wi, method="REML", data=POC.RR,  digits=8)

POC.RR$wi <- 1/(POC.RR$vi)

summary(POC.res)
POC.res$beta  # coefficient    =   effect size   (se ?)

##data bias test, if P>0.05, the dataset is unbias
regtest(rall) 
##draw funnel plot (Figure S1)
funnel(POC.res)
##check the sources of heterogeneity
reg=rma(yi, vi, data=data2, mods=~Nrate)