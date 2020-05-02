#################simple linearer#######
######################calories##############
library(readr)
calories <- read_csv("E:/data science r studio/Assignment code 1/simple ineare regresstion/calories.csv")
View(calories)

#Explolarity  data analisis
summary(calories)

# check relation in data

plot(calories$`Weight gained (grams)`,calories$`Calories Consumed`,main="scatterplot")
plot(calories)

#co-relation
attach(calories)
cor(calories)
cor(`Weight gained (grams)`,`Calories Consumed`)

#builed the model

model<-lm(`Weight gained (grams)`~`Calories Consumed`)
summary(model)

###pradic the model
pred<-predict(model)
pred

model$residuals
sum(model$residuals)
 mean(model$residuals)

 sqrt(sum(model$residuals^2))/nrow(calories) 
sqrt(mean(model$residuals^2)) 

confint(model,level = .95)
predict(model,interval = "predict")
summary(model)

##ggplot
library(ggplot2)

ggplot2(data = calories, aes(x = `Weight gained (grams)`, y =`Calories Consumed`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories, aes(x=`Weight gained (grams)`, y=pred))
#copmaring predic
#plot(pred.type="l",lty=1.8,col="blue")
l#ine(pred,type="l",col="blue")

#finding accuracy
#rmse<-sqrt(mean(model$residuals^2)) 
#rmse

plot(log(`Weight gained (grams)`),`Calories Consumed`)
cor(log(`Weight gained (grams)`), `Calories Consumed`)

model_log <- lm(`Calories Consumed`~ log(`Weight gained (grams)`))   # lm(Y ~ X)

summary(model_log)

