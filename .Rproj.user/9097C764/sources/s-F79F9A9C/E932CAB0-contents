#just a test

source("~/GitHub/Instant-Gas-Exchange-Measurements/R/GXread.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/CS_GXcurve.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/GetValue_GXcurve.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/Find_GXcurve.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/plotGX.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/PGMean.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/summary_GXcurve.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/summary_GXvalue.R")
source("~/GitHub/Instant-Gas-Exchange-Measurements/R/summary_GXmean.R")

leak1_718 = read.GX(filename = "KX 07-18-2017 ril leak1_.csv",leaf_rep = 1,genolist = "genotypic imformation.csv")
leak2_718 = read.GX(filename = "KX 07-18-2017 RIL leak2_.csv",leaf_rep = 1,genolist = "genotypic imformation.csv")
steward_718 = read.GX(filename = "TMW 07-18-2017 ril stewart_.csv",leaf_rep = 1,genolist = "genotypic imformation.csv")
leak1_719 = read.GX(filename = "KX 07-19-2017 RIL_ leak1.csv",leaf_rep = 2,genolist = "genotypic imformation.csv")
leak1_720 = read.GX(filename = "nicole-7-20-2017-ril_leak1.csv",leaf_rep = 1,genolist = "genotypic imformation.csv")
steward_720 = read.GX(filename = "KX 07-20-2017 RIL_Stewart.csv",leaf_rep = 1,genolist = "genotypic imformation.csv")
leak1_721 = read.GX(filename = "cm 07-21-2017 ril leak1_.csv",leaf_rep = 2,genolist = "genotypic imformation.csv")
leak2_721 = read.GX(filename = "cm 07-21-2017 ril leak2_.csv",leaf_rep = 2,genolist = "genotypic imformation.csv")
steward_721 = read.GX(filename = "KX 07-21-2017 RIL STEWARD_.csv",leaf_rep = 2,genolist = "genotypic imformation.csv")


AllCurve = CS(leak1_718,leak2_718,leak1_719,leak1_720,leak1_721,leak2_721,steward_720,steward_721,steward_718)
AllValue = GetValue(AllCurve)
AllMean = PGMean(AllValue, "genotype")



plot.GXcurve("Cond",genotype = c("Z019E0142"),dataset=all)







