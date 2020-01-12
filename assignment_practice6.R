str(PaloAlto)
library("tidyverse")
PaloAlto[,-1] %>% apply(2,mean)
mean(PaloAlto$`East Palo Alto`)
sum(is.na(PaloAlto$`Fair Oaks`))
?sapply

sum(is.na(PaloAlto))
colnames(PaloAlto[,-1])


PaloAlto[,-1] %>% apply(2,mean)
PaloAlto[,-1] %>% apply(2,median)
PaloAlto[,-1] %>% apply(2,max)-PaloAlto[,-1] %>% apply(2,min)
PaloAlto[,-1] %>% apply(2,sd)
library("moments")
PaloAlto[,-1] %>% apply(2,skewness)
PaloAlto[,-1] %>% apply(2,kurtosis)
((PaloAlto[,-1] %>% apply(2,sd))/(PaloAlto[,-1] %>% apply(2,mean)))*100

CommFrame <- data.frame("Names"=colnames(PaloAlto[,-1]))
CommFrame
CommFrame<- data.frame("Mean Travel Time"=PaloAlto[,-1] %>% apply(2,mean),"Standard Deviation of TT"=PaloAlto[,-1] %>% apply(2,sd))
CommFrame <- rownames_to_column(CommFrame,"Community Names")
CommFrame

CommFrame <- data.frame(CommFrame,"Upper 95%"= qnorm(0.975,CommFrame$Mean.Travel.Time,(CommFrame$Standard.Deviation.of.TT/sqrt(220))))
CommFrame

CommFrame <- data.frame(CommFrame,"Lower 95%"= qnorm(0.025,CommFrame$Mean.Travel.Time,(CommFrame$Standard.Deviation.of.TT/sqrt(220))))                        
CommFrame


qnorm(0.975)*1.167965/sqrt(220)
(22.13615-21.82748)/2

CommFrame <- data.frame(CommFrame,"Margin.Error"=(CommFrame$Upper.95. - CommFrame$Lower.95.)/2)
CommFrame

CommFrame <- data.frame(CommFrame,"Target Sample Size"=round(((qnorm(0.995)*CommFrame$Standard.Deviation.of.TT)/0.1)^2))
CommFrame

str(PaloAlto)


avg_hr <-  tapply(PaloAlto$`East Palo Alto`,PaloAlto$Time,mean)

names(avg_hr)
avg_hr

?barplot
barplot(avg_hr,width=1)
str(avg_hr)
class(avg_hr)
plot(avg_hr,type="o",col="red",xlab="Hourly Times",ylab="Averge travel time")


avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL

avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S")

str(avg_hr)

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")

ggplot

avg_hr <-  tapply(PaloAlto$`Fair Oaks`,PaloAlto$Time,mean)
avg_hr
avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL
avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")



avg_hr <-  tapply(PaloAlto$Googleplex,PaloAlto$Time,mean)
avg_hr
avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL
avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")




avg_hr <-  tapply(PaloAlto$`Los Altos Hills`,PaloAlto$Time,mean)
avg_hr
avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL
avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")


avg_hr <-  tapply(PaloAlto$`Manzanita Park`,PaloAlto$Time,mean)
avg_hr
avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL
avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")




avg_hr <-  tapply(PaloAlto$`Menlo Park`,PaloAlto$Time,mean)
avg_hr
avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL
avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")




avg_hr <-  tapply(PaloAlto$`Sharon Heights`,PaloAlto$Time,mean)
avg_hr
avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL
avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")

avg_hr <-  tapply(PaloAlto$`Stanford U.`,PaloAlto$Time,mean)
avg_hr
avg_hr <- data.frame(avg_hr,"Time"=format(as.POSIXlt(names(avg_hr),format="%I %p"),format="%H:%M:%S"),stringsAsFactors = FALSE)
avg_hr
rownames(avg_hr) <- NULL
avg_hr<-avg_hr[ order(avg_hr$Time),]
avg_hr

ggplot(avg_hr,aes(x=Time,y=avg_hr,group=1)) +geom_line(color='steelblue')+geom_point(color="red") + labs(x="Hourly Time",y="Avg. Travel Time")

qnorm(0.1,mean=527.5,sd=54.24,lower.tail=FALSE) 
qnorm(0.85,mean=527.5,sd=54.24,lower.tail=FALSE)
pnorm(600,mean=527.5,sd=54.24,lower.tail=TRUE) 

CommFrame

sum(PaloAlto$`East Palo Alto`>20)/length(PaloAlto$`East Palo Alto`)

DF<- data.frame("Community"=c("East_Palo_Alto","Fair Oaks","Googleplex","Los Altos Hills","Manzanita Park","Menlo Park","Sharon Heights","Stanford U."))

DF<- data.frame(DF,"targeted_prop"=c(sum(PaloAlto$`East Palo Alto`>20)/length(PaloAlto$`East Palo Alto`),
                                     sum(PaloAlto$`Fair Oaks`>20)/length(PaloAlto$`Fair Oaks`),
                                     sum(PaloAlto$Googleplex>20)/length(PaloAlto$Googleplex),
                                     sum(PaloAlto$`Los Altos Hills`>20)/length(PaloAlto$`Los Altos Hills`),
                                     sum(PaloAlto$`Manzanita Park`>20)/length(PaloAlto$`Manzanita Park`),
                                     sum(PaloAlto$`Menlo Park`>20)/length(PaloAlto$`Menlo Park`),
                                     sum(PaloAlto$`Sharon Heights`>20)/length(PaloAlto$`Sharon Heights`),
                                     sum(PaloAlto$`Stanford U.`>20)/length(PaloAlto$`Stanford U.`)))
DF




DF<- data.frame(DF,"Sample_Size*Prop"=DF$targeted_prop*220,"Sample_size*(1-prop)"=(1-DF$targeted_prop)*220)
DF

qnorm(0.965)
PST<- sqrt((0.02727273*(1-0.02727273))/220)
PST
UL<-0.02727273+ PST*qnorm(0.965)
LL<-0.02727273 - PST*qnorm(0.965)
UL
LL


DF

round(((qnorm(0.975)/0.01)^2)*0.93636364*(1-0.93636364))
round(((qnorm(0.975)/0.01)^2)*0.94545455*(1-0.94545455))
round(((qnorm(0.975)/0.01)^2)*0.45000000*(1-0.45000000))
round(((qnorm(0.975)/0.01)^2)*0.02727273*(1-0.02727273))

CommFrame

qnorm(0.95)
qnorm(0.975)












































     