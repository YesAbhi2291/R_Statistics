attach(eggs)
reg1<-lm(Cases~`First Week?`+Easter+`Egg Price`+`Beef Price`+`Pork Price`+`Chicken Price`+`Cereal Price`)
summary(reg1)


str(eggs)

step(reg1,data=eggs,direction="backward")
nrow(eggs)

reg2<- lm(Cases~`First Week?`+Easter+`Beef Price`+`Pork Price`+`Chicken Price`)
summary(reg2)
predict(reg2,eggs,interval="prediction",level=0.92)

eggs_predict <- data.frame(eggs,predict(reg2,eggs,interval="prediction",level=0.92))
eggs_predict
eggs_confidence <- data.frame(eggs,predict(reg2,eggs,interval="confidence",level=0.92))
eggs_confidence

Taxes
attach(Taxes)
reg3<- lm(`Taxes Owed($)`~`Tax Return Number`+`Gross Pre-tax Income($)`+`Schedule-A Deductions($)`+`Schedule-C Income($)`+`Schedule C Deductions Percentage(%)`+`Home Office`)
summary(reg3)
step(reg3,data=Taxes,direction="backward")

reg4<- lm(`Taxes Owed($)`~`Schedule C Deductions Percentage(%)`+`Home Office`+`Gross Pre-tax Income($)`)
summary(reg4)


Taxes
fitted(reg4)

Taxes1<- data.frame(Taxes,"Predicted_taxes"=predict(reg4,Taxes))
Taxes1


SE <- sd(Taxes$`Taxes Owed($)`)/sqrt(10)
SE

Taxes1<- data.frame(Taxes1,"Tax_Score"=(Taxes1$Taxes.Owed...-Taxes1$Predicted_taxes)/SE)
Taxes1

ifelse(Taxes1$Tax_Score<-3,"RedFlag","No")

Taxes1$Tax_Score < -3


Taxes1<- data.frame(Taxes1,"RedFlag"=ifelse(Taxes1$Tax_Score < -3,"Yes","No"))
Taxes1



