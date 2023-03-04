

library(readr)
library(forcats)
library(performance)
library(olsrr)
library(mvinfluence)
library(lmvar)
library(ggcorrplot)


CSE <- read_csv("Data.csv")
CSE <- CSE[!CSE$Close == "null", ]
summary(CSE)



CSE$pltInf <- as.character(CSE$pltInf)
CSE$pltInf <- fct_recode(CSE$pltInf,
                         "not_influenced" = "0",
                         "influenced" = "1")


str(CSE)


## 80% datset taken for modeling

#testing = CSE[sample(1:4035,),replace=FALSE]
#testing <- testing[,c('Date','Open','High','Low','Close','Volume','pltInf')]
#View(testing)

smp_size <- floor(0.80 * nrow(CSE))
set.seed(123)
train_ind <- sample(seq_len(nrow(CSE)), size = smp_size)
training <- CSE[train_ind, ]
testing <- CSE[-train_ind, ]
# 
testing <- testing[,c('Date','Open','High','Low','Close','Volume','pltInf')]
View(testing)


## Exploratory data analysis

cor(testing[,c('Open','High','Low','Close','Volume')])


## Base model


library(kableExtra)
library(jtools)

base_model <- lm(formula = Close ~ Open + High + Low + Volume + pltInf, data = testing)

summary(base_model)
# summ(base_model, scale = FALSE, n.sd = 4)
anova(base_model)

check_model(base_model)


model2 <- lm(formula = Close ~ Open + High + Low + pltInf, data = testing)
anova(model2)
summary(model2)
check_model(model2)

car::vif(model2)

model3 <- lm(formula = Close ~ Low + High + pltInf, data = testing)
anova(model3)
summary(model3)
check_model(model3)

car::vif(model3)

model4 <- lm(formula = Close ~ Open + Low + pltInf, data = testing)
anova(model4)
summary(model4)
check_model(model4)

car::vif(model4)


model4 <- lm(formula = Close ~ High, data = training)
anova(model4)
summary(model4)
check_model(model4)

final.model <- lm( Close ~ Low + Volume + pltInf, data = training)


predictions = cbind( testing, Close = predict(final.model, newdata = testing))
predictions
write.csv(predictions, 'predictions.csv', row.names = F)
