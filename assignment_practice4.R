str(Entrees)

z <- prop.table((table(Entrees$Gender,Entrees$Entrée)))
z
z[2,1]

sum(z[2,])+sum(z[,1])-z[2,1]

#c) It will be a woman or not order beef.

#z[w or bc]= z[w]+z[bc]-z(w and bc)

sum(z[1,]) + (1-sum(z[1,]))*(1-sum(z[,1]))


#d) It will be neither a man nor order beef. 

1-(sum(z[2,])+sum(z[,1])-z[2,1])

#e) It will order beef given that is a man. 

z[2,1]/sum(z[2,])

z[2,1]/sum(z[,1])
#g) Are the events "the customer is a man" and "orders chicken" independent? Explain. 

z

sum(z[2,])*sum(z[,2])

z[1,2]+z[1,3]+z[1,4]
sum(z[1,])*0.7113821


sum(z[1,])+sum(z[,2])+sum(z[,3])+sum(z[,4])-z[1,2]-z[1,3]-z[1,4]


U<- data.frame(prop.table(table(Tracy$Years)),stringsAsFactors = FALSE)

U
str(U)
class(U)
colnames(U)[1]<- "X"
colnames(U)[2]<- "P.X"
str(U)
U

E.X <- sum((as.numeric(as.character(U$X)))*U$P.X)
E.X
V.X<- sum((((as.numeric(as.character(U$X)))-E.X)^2)*U$P.X)
V.X
SD.X<- sqrt(V.X)
SD.X
CV.X<- SD.X/E.X
CV.X

View(Tracy)

str(Tracy)
m<-mean(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)
m
median(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)

max(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)- min(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)

sd(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)


sd(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)/mean(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)
   

library("moments")

skewness(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)
kurtosis(Tracy$`Maintenance Cost per Mile`,na.rm=TRUE)

str(Tracy)

D<- data.frame(Tracy)
D
str(U)

DF<- data.frame(U,"Z_900"=m*(as.numeric(as.character(U$X)))*9000,"Z_12000"=m*(as.numeric(as.character(U$X)))*12000)
DF

E.Z<- 0.6*(sum(DF$Z_900*DF$P.X)) + 0.4*(sum(DF$P.X*DF$Z_12000))
E.Z

Var.Z<- 0.6*(sum(((DF$Z_900-E.Z)^2)*DF$P.X)) + 0.4*(sum(((DF$Z_12000-E.Z)^2)*DF$P.X))
Var.Z
SD.Z <- sqrt(Var.Z)
SD.Z

CV.Z<- SD.Z/E.Z


CV.Z

library("tidyverse")

DF %>% filter(Z_900 <= 3000 |  Z_12000 <= 3000)





