rm(list=ls())
dat<-read.csv("/publication_web.csv",header=TRUE,sep="\t",stringsAsFactors=FALSE)
dat$YEAR<- as.numeric(dat$YEAR)

countries<-(unique(dat$COUNTRY))
years<-(1993:2018)

R<-matrix(0,nrow=length(countries), ncol=length(years))

for (i in 1:length(countries)){
  dat.i<-dat[which(dat$COUNTRY==countries[i]),]
  for(j in 1:length(years)){
    ind.j <- dat.i$YEAR==years[j]
    if(sum(ind.j,na.rm=TRUE)>0){
      R[i,j]<-sum(dat.i$X.SAMPLES[which(ind.j)])
    }
  }
}

C<-R;
for(i in 1:nrow(C)){ C[i,]<-cumsum(C[i,])}
C<-t(data.frame(C))
colnames(C)<-countries
library(reshape2)

id<-order(C[26,],decreasing = TRUE)
C<-C[,id]

library(ggplot2)
#ggplot needs a dataframe
data <- as.data.frame(C)
#id variable for position in matrix
data$yr <- years
#reshape to long format
plot_data <- melt(data,id.var="yr")


#plot
ggplot(plot_data, aes(x=yr,y=value,group=variable,colour=variable)) +
  labs(color = "")+
  scale_y_log10(name = "Cumulative number of samples by publication per country (log scale)", limits=c(1, 100000), breaks=c(0, 10,100,1000,10000,10000, 10000, 100000))+
  scale_x_continuous(name="Years", limits=c(1993, 2018), breaks=c(1993, 1998,2003,2008,2013,2018) )+
  geom_line()



#Write CSV with Country&Count
cbind(countries,rowSums(R))
write.csv( cbind(countries,rowSums(R)) , "countryAndCount.csv" )
