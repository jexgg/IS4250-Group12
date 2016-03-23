library(QuantPsyc)

dataset = read.csv("PIRI_metab_Dryad.csv", header = T)
View(dataset)
attach(dataset)

### Fisher's Exact Test ###
MES = matrix(c(rep(0,571)))
for (i in 1:571) {
    if (hypercol[i]+hypertrig[i]+hipressure[i]+diabetes[i]+obesity[i]>=3)
        MES[i] = 1
}
high.PIRI.high.MES = 0
high.PIRI.low.MES = 0
low.PIRI.high.MES = 0
low.PIRI.low.MES = 0
for (i in 1:571) {
    if (PIRI.25[i]==1 && MES[i]==1)
        high.PIRI.high.MES = high.PIRI.high.MES + 1
    else if (PIRI.25[i]==1 && MES[i]==0)
        high.PIRI.low.MES = high.PIRI.low.MES + 1
    else if (PIRI.25[i]==0 && MES[i]==1)
        low.PIRI.high.MES = low.PIRI.high.MES + 1
    else
        low.PIRI.low.MES = low.PIRI.low.MES + 1
}

contigency.table = matrix(c(high.PIRI.high.MES, high.PIRI.low.MES, low.PIRI.high.MES, low.PIRI.low.MES),
                          ncol = 2, byrow = T)
fisher.test(contigency.table)           # p-value = 0.01643, 95% CI = (1.121676, 23.531668) OR = 4.810699

### Linear Regression Analysis ###
linear.model = lm(MES~PIRIstd)
summary(linear.model)                   # p-value = 0.0001501
lm.beta(linear.model)                   # beta = 0.1579931

### Logistic Regression Analysis ###
MES2 = MES
for (i in 1:571) {
    if (hypercol[i]==0 && hypertrig[i]==0 && hipressure[i]==0 && diabetes[i]==0 && obesity[i]==0)
        MES2[i] = 0
    else
        MES2[i] = 1
}
logit.model.1 = glm(MES2~PIRI.25, data=dataset, family=binomial)
summary(logit.model.1)
exp(cbind(OR = coef(logit.model.1), confint(logit.model.1)))
