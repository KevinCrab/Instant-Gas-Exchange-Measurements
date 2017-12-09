source("readGX.R")
source("summary_GXcurve.R")
source("CS_GXcurve.R")
source("Find_GXcurve.R")
source("GetValue_GXcurve.R")
source("plot_GXcurve.R")
gl = "genotypic imformation.csv"

leak1_718 = read.GX(filename = "KX 07-18-2017 ril leak1_.csv",leaf_rep = 1,gl)
leak2_718 = read.GX(filename = "KX 07-18-2017 RIL leak2_.csv",leaf_rep = 1,gl)
steward_718 = read.GX(filename = "TMW 07-18-2017 ril stewart_.csv",leaf_rep = 1,gl)
leak1_719 = read.GX(filename = "KX 07-19-2017 RIL_ leak1.csv",leaf_rep = 2,gl)
# leak2_719 = read.GX(filename = "KX 07-19-2017 RIL_ leak2.csv",leaf_rep = 2)
leak1_720 = read.GX(filename = "nicole-7-20-2017-ril_leak1.csv",leaf_rep = 1,gl)
#leak2_720 = read.GX(filename = "nicole-7-20-2017-ril-leak2_.csv",leaf_rep = 1)
steward_720 = read.GX(filename = "KX 07-20-2017 RIL_Stewart.csv",leaf_rep = 1,gl)
leak1_721 = read.GX(filename = "cm 07-21-2017 ril leak1_.csv",leaf_rep = 2,gl)
leak2_721 = read.GX(filename = "cm 07-21-2017 ril leak2_.csv",leaf_rep = 2,gl)
steward_721 = read.GX(filename = "KX 07-21-2017 RIL STEWARD_.csv",leaf_rep = 2,gl)


filename = "nicole-7-20-2017-ril-leak2_.csv"

all = CS(leak1_718,leak2_718,leak1_719,leak1_720,leak1_721,leak2_721,
        steward_720,steward_721,steward_718)


mean = GetValue(all)

sub = all[which(all$name %in% head(unique(all$name))),]

plot(sub,sub,"Photo")

plotGX("Photo",genotype = "Z019E0032", dataset=all)


plot.GXcurve("Cond",genotype = c("Z019E0142"),dataset=all)




