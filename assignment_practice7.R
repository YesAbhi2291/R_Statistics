func<- function(n,e) {
  a <- n/(1+e)
  sum <- sum(as.vector(a)) 
  print(sum)
}

x<- c(1:10000)
for (val in x){
N<- as.vector(sample(CertaintyHouse$N,84,replace=TRUE))
E<- as.vector(sample(CertaintyHouse$E,84,replace=TRUE))
a <- N/(1+E)
sum <- sum(as.vector(a)) 
print(sum)
}

x<- c(1:10000)
for (val in x)
{N<- as.vector(sample(CertaintyHouse$N,84,replace=TRUE))
E<- as.vector(sample(CertaintyHouse$E,84,replace=TRUE))
a <- N/(1+E)
sum <- sum(as.vector(a)) 
print(sum)
}  

N<- as.vector(sample(CertaintyHouse$N,84,replace=TRUE))
  N
  E<- as.vector(sample(CertaintyHouse$E,10000,replace=TRUE))
  E
  a <- N/(1+E)
  a
  sum <- sum(as.vector(a)) 
  print(sum)
  
}


N<- as.vector(sample(CertaintyHouse$N,84,replace=TRUE))
N
E<- as.vector(sample(CertaintyHouse$E,10000,replace=TRUE))
E
a <- N/(1+E)
a

cumsum(a)
sum <- sum(as.vector(a)) 
print(sum)

CertaintyHouse$N/((CertaintyHouse$E)+1)



func(CertaintyHouse$N,CertaintyHouse$E)


replicate(10,sample(CertaintyHouse$N),sample(CertaintyHouse$E))




str(CertaintyHouse)
dim(CertaintyHouse)


func(CertaintyHouse$N,CertaintyHouse$E)




V <-  sample(sum((CertaintyHouse$N/(CertaintyHouse$E+1))),size=10000,replace=TRUE)

V

hist(V)

mean(V)
median(V)
sd(V)
max(V)-min(V)
(sd(V)/mean(V))*100
library("moments")
skewness(V)
kurtosis(V)


quantile(V,prob=0.975)
quantile(V,prob=0.025)

library("tidyverse")

PaloAlto


CommFrame<- data.frame("Mean Travel Time"=PaloAlto[,-1] %>% apply(2,mean),"Standard Deviation of TT"=PaloAlto[,-1] %>% apply(2,sd))
CommFrame
CommFrame <- rownames_to_column(CommFrame,"Community Names")
CommFrame
CommFrame <- data.frame(CommFrame,"Z.stat" =(CommFrame$Mean.Travel.Time- 20)/(CommFrame$Standard.Deviation.of.TT/sqrt(220))
Z.stat

CommFrame <- data.frame(CommFrame,"Z.stat" =((CommFrame$Mean.Travel.Time- 20)/(CommFrame$Standard.Deviation.of.TT/sqrt(220))))
CommFrame

t.test(PaloAlto$`East Palo Alto`,mu=20,alternative="greater")
t.test(PaloAlto$`Fair Oaks`,mu=20,alternative="greater")
t.test(PaloAlto$Googleplex,mu=20,alternative = "greater")
t.test(PaloAlto$`Los Altos Hills`,mu=20,alternative = "greater")
t.test(PaloAlto$`Manzanita Park`,mu=20,alternative = "greater")
t.test(PaloAlto$`Menlo Park`,mu=20,alternative = "greater")
t.test(PaloAlto$`Sharon Heights`,mu=20,alternative="greater")
t.test(PaloAlto$`Stanford U.`,mu=20,alternative="greater")
  
head(PaloAlto)

t.test(PaloAlto$`East Palo Alto`,mu=35,alternative="less")
t.test(PaloAlto$`Fair Oaks`,mu=35,alternative="less")
t.test(PaloAlto$Googleplex,mu=35,alternative = "less")
t.test(PaloAlto$`Los Altos Hills`,mu=35,alternative = "less")
t.test(PaloAlto$`Manzanita Park`,mu=35,alternative = "less")
t.test(PaloAlto$`Menlo Park`,mu=35,alternative = "less")
t.test(PaloAlto$`Sharon Heights`,mu=35,alternative="less")
t.test(PaloAlto$`Stanford U.`,mu=35,alternative="less")




t
