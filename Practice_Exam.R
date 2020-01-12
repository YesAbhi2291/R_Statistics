(-0.76)^2


attach(PracticeExam2F2019)

PracticeExam2F2019

dollreg <- lm(`Unit Price`~`Available Dolls`+Month)
summary(dollreg)

predicted_11<-coef(dollreg)[1]+coef(dollreg)[2]*200+coef(dollreg)[3]*11
predicted_11

coef(dollreg)[2]

l<- data.frame(11,200)
l
colnames(l)<- c("Month","Available Dolls")
predict(dollreg,l,interval="prediction",level=0.95)
predict(dollreg,l,interval="confidence",level=0.95)

qt(0.975,df=39)


2*pt(-2.3, df=39)

r<- data.frame("Chessman"=c("Pawn","Knight","Rook","Bishop","Queen"))

r<- data.frame(r,"Industry_Standard_Weight(ounces)"=c(0.5,1,1,1,3),"Sample_weight_average(ounces)"=c(0.46,1.32,1.51,0.98,3.25),"Sample_SD"=c(0.11,1.82,0.97,0.13,0.73))
r

r<- data.frame(r,"Standard_Error"=r$Sample_SD/sqrt(40))
r
r <- data.frame(r,"t.stat"= (r$Sample_weight_average.ounces.-r$Industry_Standard_Weight.ounces.)/r$Standard_Error)
r
r<- data.frame(r,"p-value"= ifelse(r$t.stat > 0,2*(1-pt(r$t.stat,df=39)),2*pt(r$t.stat,df=39)))
r

r<- data.frame(r,"Decision"=ifelse(r$p.value > 0.05,"Fail to reject H0","Reject Ho"))
r


1.44/sqrt(60)
4.34-4
0.34/ 0.1859032
(1-pt(1.828909,df=59))
qt(0.975,59)

PracticeExam2F2019

s<- data.frame(PracticeExam2F2019,"Month.2"=(PracticeExam2F2019$Month)^2)
s

prodlm <- lm(APT~Month+Model+Month.2,data=s)
summary(prodlm)


step(prodlm,s,direction="backward")
prodlm1<- lm(APT~Model+Month.2,data=s)
summary(prodlm1)

coef(prodlm1)[1]+coef(prodlm1)[2]+coef(prodlm1)[4]*(41^2)
predict(prodlm1,data.frame("Month.2"=41^2,"Model"="generic"),interval="prediction",level=0.95)




rm(PracticeExam1F2019_1_)
rm(PracticeExam2F2019)
rm(list=ls())


PracticeExam2F2019

a<- PracticeExam2F2019[,2:4]
a
attach(a)
logit<- glm(Buy~Age+Kids,data=a,family=binomial(link="logit"))
summary(logit)

McFadR<-1-(logit$deviance/logit$null.deviance)
McFadR
1-pchisq(129.49-107.45,df=2)
exp(0.11472)
exp(1.46987)

c<- coef(logit)
c

L<- coef(logit)[1]+coef(logit)[2]*Age+coef(logit)[3]*Kids
V<- exp(L)/(1+exp(L))
V
a
a<- data.frame(a,"Predicted Prob"=V)
a


x<- data.frame("User"=c(1,2,3,4,5,6,7,8,9),"Age"=c(20,20,20,25,25,25,30,30,30),"Kids"=c(0,1,2,0,1,2,0,1,2))

x
attach(x)
B<- c[1]+c[2]*x$Age+c[3]*x$Kids
B
n<- exp(B)/(1+exp(B))


x<- data.frame(x,"Predicted Probability"=n*100)
x


attach(PracticeExam2F2019)
PracticeExam2F2019
t.test(PracticeExam2F2019$`Audience Size`[PracticeExam2F2019$Location == "U.S."],mu=3200,alternative="less")
t.test(PracticeExam2F2019$`Audience Size`[PracticeExam2F2019$Location == "U.K."],mu=3200,alternative="less")

PracticeExam2F2019<- data.frame(PracticeExam2F2019,"Humor"=ifelse(PracticeExam2F2019$`Contains Humor?`=="Yes",1,0))
PracticeExam2F2019
t.test(PracticeExam2F2019$Humor,mu=0.5)
BW.US<- PracticeExam2F2019$Audience.Size[PracticeExam2F2019$Location=="U.S."]
BW.UK<- PracticeExam2F2019$Audience.Size[PracticeExam2F2019$Location=="U.K."]
BW.US
BW.UK
t.test(BW.US,BW.UK,mu=50,var.equal=TRUE)
t.test(BW.US,BW.UK,mu=50,var.eqaul=FALSE)
var.test(BW.US,BW.UK)




attach(PracticeExam2F2019)
PracticeExam2F2019
oring<- glm(`Number of Incidents`~`Launch Temperature (F)`,family=poisson(link="log"))
summary(oring)
McFadd<- 1-(oring$deviance/oring$null.deviance)
McFadd
1-pchisq(oring$null.deviance-oring$deviance,df=1)
exp(0.10894)
v<- coef(oring)
v
j<- data.frame("Launch Temperature(F)"=c(55,60,65,70,75))
j<- data.frame(j,"Predicted_lambda"=exp(v[1]+v[2]*j$Launch.Temperature.F.))
j
j<- data.frame(j,"Prob(Incidents>=1)"=(1-dpois(0,lambda=j$Predicted_lambda)))
j

Upper_Limit<- 29.5+(3.5/sqrt(20))*qt(0.995,df=19)
Lower_Limit<- 29.5-(3.5/sqrt(20))*qt(0.995,df=19)
Upper_Limit
Lower_Limit


UL<- (12/20) +sqrt((12/20)*(8/20)/20)*qnorm(0.95)
LL<- (12/20) -sqrt((12/20)*(8/20)/20)*qnorm(0.95)
UL
LL


((qnorm(0.975)^2)*(12/20)*(8/20))/0.05^2
SellingPrice<- runif(100,min=22,max=27)
Volume=rnorm(100,mean=550,sd=125)
VC<- PracticeExam2F2019$`Variable Cost per Jean`
VC
Variable_Cost<- sample(VC,size=100,replace=TRUE)
Variable_Cost
NetProfit<- SellingPrice*Volume-Variable_Cost*Volume-9000
NetProfit
means=replicate(1000,(mean(sample(NetProfit,100,replace=TRUE))))
means
UpperBound<- quantile(means,prob=0.94)
LowerBound<- quantile(means,prob=0.06)
UpperBound
LowerBound


d<- PracticeExam2F2019[,2:5]
d
v<- lm(Demand~Price+`Advertising Expenditure`+Campaign,data=d)
summary(v)
plot(v)
r<- residuals(v)
r
plot(r,d$`Advertising Expenditure`)
plot(r,d$Price)Price
regx1<- lm(Demand~Price+`Advertising Expenditure`+Campaign,data=d)
regx2<- lm(Price~Demand+`Advertising Expenditure`+Campaign,data=d)
regx3<- lm(`Advertising Expenditure`~Demand+Price+Campaign,data=d)
summary(regx1)$r.square
VIF1<- 1/(1-summary(regx1)$r.square)
VIF2<- 1/(1-summary(regx2)$r.square)
VIF3<- 1/(1-summary(regx3)$r.square)
VIF1
VIF2
VIF3

step(v,d,direction="backward")
v<- lm(Demand~+`Advertising Expenditure`+Campaign,data=d)
summary(v)

PracticeExam2F2019

predic<- data.frame(PracticeExam2F2019,"PredictionInterval"=predict(v,PracticeExam2F2019,interval="prediction",level=0.98))
predic
confid<- data.frame(PracticeExam2F2019,"PredictionInterval"=predict(v,PracticeExam2F2019,interval="confidence",level=0.98))
confid

PracticeExam2F2019
Prog.Error<-PracticeExam2F2019[,2]
Prog.Error
ks.test(PracticeExam2F2019$Errors,"ppois",lambda=4.8)
ks.test(PracticeExam2F2019$Errors,"pbinom",size=10,prob=0.48)


fo=table(PracticeExam2F2019$Industry)
fo
fe=500 * c(0.25,0.10,0.30,0.20,0.15)
fe
chi.stat<- sum(((fo-fe)^2)/fe)
1-pchisq(chi.stat,df=4)
fe1<- 500 *c(0.2,0.2,0.2,0.2,0.2)
fe1
chi.stat1<- sum(((fo-fe1)^2)/fe1)
1-pchisq(chi.stat1,df=4)
