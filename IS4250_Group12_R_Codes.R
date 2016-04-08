### Logistic Regression Analysis ###
dataset = read.csv("IS4250_Group12_Data.csv", header = T)
attach(dataset)

library(pscl)
numbermes <- hypercol+hypertrig+hipressure+diabetes+obesity
atleast1mes <- (numbermes>0)

logit.model1 = glm(atleast1mes~PIRI.25, binomial)                   # model without biographic data
summary(logit.model1)                                               # p-value = 0.0044
exp(cbind(OR = coef(logit.model1), confint(logit.model1)))          # OR = 1.7786, 95% CI = (1.1942, 2.6414)
pR2(logit.model1)                                                   # Cragg and Uhler¡¯s pseudo R^2 = 0.01943

plot(PIRIstd, atleast1mes, xlab="Psychological Injury Risk Indicator",
     ylab="Presence of at least 1 MES Component",
     main="PIRI Standard Value VS At Least 1 MES Component")
curve(predict(logit.model1, data.frame(PIRI.25=x),type="resp"), add = T)
points(PIRI.25, fitted(logit.model1), pch=20) 

logit.model2 = glm(atleast1mes~PIRI.25+sex+age+smoker+
                       alcohol+exercise+sleep, binomial)            # model with biographic data
summary(logit.model2)                                               # p-value = 0.0382
exp(cbind(OR = coef(logit.model2), confint(logit.model2)))          # OR = 1.5956, 95% CI = (1.0233, 2.4805)
pR2(logit.model2)                                                   # Cragg and Uhler¡¯s pseudo R^2 = 0.11080
