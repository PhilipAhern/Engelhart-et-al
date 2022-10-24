library(gdata)
library(ggplot2)

## read data 
data = read.xls('~/Downloads/Philip_Btheta_IL10_screen.xlsx', header= T, stringsAsFactors = F)

anno =  data[c(1:6), c(1,2)]
expdata = data[,c(5:10)]

## IL 10 
### convert the IL10 expression to log2 scale 
expdata$IL10.log2 = log2(expdata$X.IL.10. + 0.1)

## mean and sd 
mean.IL10 = mean(expdata$IL10.log2)
sd.IL10  = sd(expdata$IL10.log2)

lb = mean.IL10 - 2*sd.IL10
rb = mean.IL10 + 2*sd.IL10

## samples with IL10 expression less than mean-2*sd
expdata.ln.mean.2sd = dplyr::filter(expdata, IL10.log2 < lb)



## pam 
mean.pam = mean(expdata$Pam_ratio)
sd.pam = sd(expdata$Pam_ratio)

lp = mean.pam - 2*sd.pam

expdata.pam.mean.2sd = dplyr::filter(expdata, Pam_ratio < lp)

## samples overlap
intersect(expdata.ln.mean.2sd$Sample_ID, expdata.pam.mean.2sd$Sample_ID)

##Plot IL.log2 distribution
ggplot(expdata, aes(x=IL10.log2)) + geom_density() 
##Plot IL.log2 distribution by OD group 
ggplot(expdata, aes(x=factor(OD_ID), y = IL10.log2))+geom_boxplot()

### Take the OD=1 as the normal growth group 
expdata.growth1=dplyr::filter(expdata,OD_ID == 1)
expdata.growth1$IL10.log2 = log2(expdata.growth1$X.IL.10. + 0.1)
ggplot(expdata.growth1, aes(x=IL10.log2)) + geom_density()

mean.growth1.IL10 = mean(expdata.growth1$IL10.log2)
sd.growth1.IL10  = sd(expdata.growth1$IL10.log2)
lb.growth1 = mean.growth1.IL10 - 2*sd.growth1.IL10
expdata.growth1.ln.mean.2sd.=dplyr::filter(expdata.growth1,IL10.log2<lb.growth1)
View(expdata.growth1.ln.mean.2sd.)
write.csv(expdata.growth1.ln.mean.2sd.,file="~/Dropbox/Philip_IL10/Growth_OD1_significant.csv")

#Do the same analysis for pam_ratio data
 mean.pam = mean(expdata.growth1$Pam_ratio)
 sd.pam = sd(expdata.growth1$Pam_ratio)
 lp = mean.pam - 2*sd.pam
 expdata.growth1.pam.mean.2sd = dplyr::filter(expdata.growth1, Pam_ratio < lp)
 table(expdata.growth1.pam.mean.2sd$Pam_ratio)
 ggplot(expdata.growth1, aes(x=Pam_ratio)) + geom_density() 
 write.csv(expdata.growth1.pam.mean.2sd,file="~/Dropbox/Philip_IL10/Growth_OD1_pam_significant.csv")

 ### Take all the growth group 
 expdata.growth=dplyr::filter(expdata,OD_ID != 6)
 expdata.growth$IL10.log2 = log2(expdata.growth$X.IL.10. + 0.1)
 ggplot(expdata.growth, aes(x=IL10.log2)) + geom_density()
 mean.IL10 = mean(expdata.growth$IL10.log2)
 sd.IL10  = sd(expdata.growth$IL10.log2)
 lb = mean.IL10 - 2*sd.IL10
 expdata.growth.ln.mean.2sd.=dplyr::filter(expdata.growth,IL10.log2<lb)
 write.csv(expdata.growth.ln.mean.2sd.,file="~/Dropbox/Philip_IL10/All_growth_ones_significant.csv")
 
 mean.pam = mean(expdata.growth$Pam_ratio)
 sd.pam = sd(expdata.growth$Pam_ratio)
 lp = mean.pam - 2*sd.pam
 expdata.growth.pam.mean.2sd = dplyr::filter(expdata.growth, Pam_ratio < lp)
 table(expdata.growth.pam.mean.2sd$Pam_ratio)
 write.csv(expdata.growth.pam.mean.2sd,file="~/Dropbox/Philip_IL10/All_growth_ones_pam_significant.csv")
 ggplot(expdata.growth, aes(x=Pam_ratio)) + geom_density() 
 
 expdata.growth$zscore = (expdata.growth$Pam_ratio-mean.pam)/sd.pam
 expdata.growth$pvalue = 1-pnorm(abs(expdata.growth$zscore))
 expdata.growth$padj<-p.adjust(expdata.growth$pvalue,method = "fdr")
 expdata.growth.sig=dplyr::filter(expdata.growth,padj<0.05)
 write.csv(,file="~/Dropbox/Philip_IL10/All_growth_ones_pam_significant.csv")
 
 
 