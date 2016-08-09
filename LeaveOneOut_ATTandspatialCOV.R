## Leave one out LOO analysis of the ATT and spatial COV data
library(gdata)
library(ggplot2)
setwd('~/Google Drive/Papers_abstracts_Drive/PreDIVA_Henk_onArterialTransitTime_spatialCoV/Analysis/')
datain<-read.xls('PreDiva_baseline_FOR_BRAD_new.xlsx')

# Define ATT
att<-as.numeric(levels(datain$ATT))[datain$ATT]
# Define non-crushed CBF spatial CoV
scov<-datain$CoVnc

index<-seq(1,length(att))
att.diff<-as.numeric()
att.loo.index<-as.numeric()
att.pererror<-as.numeric()
rsquared.loo<-as.numeric()

for (loo.index in 1:length(att) ) {
  # create a vector of all values with one datum LOO
  att.loo<-att[index != loo.index]
  scov.loo<-scov[index != loo.index]
  # identify the true value for att and scov
  att.truth<-att[loo.index]
  scov.truth<-scov[loo.index]
  # perform regression
  loo.fit<-lm(att.loo ~ scov.loo)
  summary.loo.fit<-summary(loo.fit)
  rsquared.loo[loo.index]<-summary.loo.fit$r.squared
  # estimate what the att would be given the fit and the true sCoV value
  att.loo.index[loo.index] <- loo.fit$coefficients[1] + loo.fit$coefficients[2]*scov.truth
  # create a difference vector between true and LOO estimated values of ATT
  # att.diff[loo.index] <- att.truth - att.loo.index[loo.index]
  att.pererror[loo.index]<-100*abs((att.truth - att.loo.index[loo.index]))/(att.truth + att.loo.index[loo.index])/2
}
# Calculate the mean of the true and fitted ATT values
att.mean.trueANDfit<-rowMeans(data.frame(att,att.loo.index))

plot(att,rsquared.loo,pch=19,col="blue",cex=.8,xlab="ATT value removed",ylab="LOO R-squared",main="Effect of LOO on ATT and sCOV"); grid()
## Figure 4 for spatial CoV paper, option 1
plot(att,att.pererror,col="red",pch=19,ylab="% error in ATT",xlab="Empirical ATT"); grid(); box()
## Figure 4 for spatial CoV paper, option 2: this looks better
a.gg <- ggplot(data.frame(att.mean.trueANDfit,att.pererror), aes(x=att.mean.trueANDfit, y=att.pererror))
a.gg + geom_point(colour="red",size=2,pch=20)  +
  geom_point(colour="black",size=2,pch=21)  +
  labs(title='Estimating ATT from CoV model', x='ATT [ms]', y='% Error in ATT')+
  theme(plot.title = element_text(size=25)) + 
  theme(axis.title.y = element_text(face="bold", size=25)) +
  theme(axis.title.x = element_text(face="bold", size=25)) +
  theme(text = element_text(size=25))

## Figure 4 for spatial CoV paper, option 3: do this as a Bland Altman plot using ggplot2 option
library(BlandAltmanLeh)
b.gg<-bland.altman.plot(att,att.loo.index, graph.sys= "ggplot2")
b.gg + geom_point(colour="red",size=2,pch=20)  +
  geom_point(colour="black",size=2,pch=21)  +
  labs(title='Estimating ATT from CoV model', x='ATT mean of true and fit [ms]', y='ATT true minus fit [ms]') +
  theme(plot.title = element_text(size=25)) + 
  theme(axis.title.y = element_text(face="bold", size=25)) +
  theme(axis.title.x = element_text(face="bold", size=25)) +
  theme(text = element_text(size=25))




