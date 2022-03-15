library(InformationValue)
library(caret)
library(gmodels)
library(MLmetrics)
library(car)
library(ResourceSelection)
library(dplyr)
library(rpart)
library(party)          # for visualizing trees
library(partykit)       # for visualizing trees
library(ggplot2)        # for graphics
library(ROCR)           # for graphics
library(rattle)      		# fancy tree plot
library(rpart.plot)			# enhanced tree plots
library(RColorBrewer)		# 
roc = function (data)
{
  pred <- prediction(data$fitted.values, data$y)    #ROC curve for training data
  perf <- performance(pred,"tpr","fpr") 
  
  plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
  abline(0, 1, col="red")  
}

precvsrec = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, measure = "prec", x.measure= "rec")
  
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
}


acc = function(data)
{

  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, measure = "acc")
  
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))  
}


lift = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, measure = "lift")
  
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
}
  
auc = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, "auc")
  y = perf@y.values
  print("auc")
  print(y)
}

aucpr = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf4 = performance(pred, "aucpr")
  y1 = perf4@y.values
  print("aucpr")
  print(y1)
}

prbe = function (data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf5 = performance(pred, "prbe")
  y3 = perf5@y.values
  print("prbe")
  print(y3)
}



honors$pred<-as.numeric(fit$fitted.values>0.5)

#D statistic (2009)

dstatistic = function (data,model)
{
  
  fv<-fitted(model)
  predVals <-  data.frame(trueVal=data$true, predclass=data$pred,predProb=fv)
  data.1<-predVals[predVals$trueVal==1,]
  data.0<-predVals[predVals$trueVal==0,]
  y2 = mean(data.1$predProb) - mean(data.0$predProb) #get separation 
  print("dstatistics")
  print(y2)
}


chart = function (data, model)
{
  
  fv<-fitted(model)
  predVals <-  data.frame(trueVal=data$true, predclass=data$pred,predProb=fv)
  predVals$group<-cut(predVals$predProb,seq(1,0,-.1),include.lowest=T)
  predVals$group
  xtab<-table(predVals$group,predVals$trueVal)
  
  KS<-data.frame(Group=numeric(10),
                 CumPct0=numeric(10),
                 CumPct1=numeric(10),
                 Dif=numeric(10))
  
  #fill data frame with information: Group ID, 
  #Cumulative % of 0's, of 1's and Difference
  for (i in 1:10) {
    KS$Group[i]<-i
    KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
    KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
    KS$Dif[i]<-abs(KS$CumPct0[i]-KS$CumPct1[i])
  }
  print("KS")
  print(KS)
  print(KS[KS$Dif==max(KS$Dif),])
  
  maxGroup<-KS[KS$Dif==max(KS$Dif),][1,1]
  #and the K-S chart
  ggplot(data=KS)+
    geom_line(aes(Group,CumPct0),color="blue")+
    geom_line(aes(Group,CumPct1),color="red")+
    geom_segment(x=maxGroup,xend=maxGroup,
                 y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
    labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")
}

chart(heartfailure1)



matt = function (data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf <- performance(pred, "mat")
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
}


evaluationfuction = function (data, model, method)
{
  if (method == 'binary-class')
  {
    roc(model)
    auc(model)
    acc(model)
    precvsrec(model)
    aucpr(model)
    lift(model)
    prbe(model)
    print(Concordance(model$y, model$fitted.values))
    matt(model)
    print(F1_Score(data$pred,data$true))
    print(ConfusionDF(data$pred, data$true))
    print(LogLoss(y_pred = model$fitted.Values,y_true=data$pred))
    print(LiftAUC(data$pred, data$true))
    print(GainAUC(data$pred, data$true))
    dstatistic (data,model)
    chart(data,model)
  }
  
  if (method == 'multi-class')
  {
    print(ConfusionDF(data$pred, data$true))
    print(MultiLogLoss(model$fitted.values, data$true))
    acc(model)
  }
  
}


honors <- read.csv("honors.csv")  #you will need to import your data
fit_honors <- glm(data=honors, hon ~ math + read + female, family="binomial")
honors$pred<-as.numeric(fit_honors$fitted.values>0.5)
pearsonRes <-residuals(fit_honors,type="pearson")
devianceRes <-residuals(fit_honors,type="deviance")
rawRes <-residuals(fit_honors,type="response")
studentDevRes<-rstudent(fit_honors)
fv<-fitted(fit_honors)

names(honors)[5]="true"
predVals <-  data.frame(trueVal=honors$true, predClass=honors$pred, predProb=fv, 
                        rawRes, pearsonRes, devianceRes, studentDevRes)

evaluationfuction(honors, fit_honors, 'binary-class')



heartfailure = read.csv("heartFailure.csv")
#head(heartfailure)
heartfailure$death = as.factor(heartfailure$death)
heartfailure$sex = as.factor(heartfailure$sex)
table(heartfailure$death)

#cross-validation 
k=5
fold.size = nrow(heartfailure)/k
testing.index = (k-1)*fold.size + 1:fold.size
training <- heartfailure[-testing.index,]
testing <- heartfailure[testing.index,]

train_control = trainControl(method="boot", number = 10)

fit = train(death ~ serum_creatinine+ejection_fraction+age+sex+
              creatinine_phosphokinase+serum_sodium, data = heartfailure, 
            trControl = train_control, method = "glm", family = "binomial")
summary(fit)
fit$resample
fit$results


#scale the variables

fit1 <- glm(data=heartfailure, death ~ serum_creatinine+ejection_fraction, 
           family="binomial")
summary(fit1)

#scale the variables
fit2 <- glm(data=heartfailure, death ~ serum_creatinine+ejection_fraction+age+sex+
             creatinine_phosphokinase+serum_sodium, 
           family="binomial")
summary(fit2)
#the shows the estimate, standard errors, z-score and p values 
#for each of the coefficients of the model. 
#The model started with adding more predictors variables but the r
#values of p for each of the predictors  shows that they 
#are not significant in determining the value of "death"
#the p vales for "serum creatine" and "ejection_fraction" shows that they are significant 
#in the prediction as the p values are  very low(<0.025). Additional age, and 
#creatinine_phosphokinase are also useful in the analysis. 


#the null residual suggests the response by the model if we
#consider only the intercept. A lower value equates to a better model. 
#this value is constant irrespective of the other predictors 
#the residual deviance indicates the response of the model when all 
#the variables are included. These values was observed to reduce as some more ]
#predictor variables was added to the model. 

#comparing the two models above with the AIC and deviance residuals show that 
#the second model is better. The AIc is lower for the second model; from  283.11
#to 272.55. 
#and the residual deviance reduces from 277.11 to 258.55. 
#therefore the second model is picked as the working model. 


#the hosmer-lemeshow goodness of fit test. This is to show how well the model 
#fits the data. 

hoslem.test(heartfailure$death, fitted(fit2))
#the pvalue here is 0.5365 which means that there is no significant 
#evidence to show that the model is a poor fit for the data. 
hoslem.test(heartfailure$death, fitted(fit1))
#the pvalue here is 0.002587 which means that there is significant 
#evidence to show that the model is a poor fit for the data. 



plot(predict(fit2),residuals(fit2),col=c("blue","red")[heartfailure$death])
abline(h=0,lty=2,col="grey")
#the residual analysis show that when two lines, the blue line and the red line. 
#if the true value is 0, then we always predict more and have a negative residuals
#(the blue line) and if it is 1, then we would under estimate and the residuals 
#would be positive (the red points). 


rl=loess(residuals(fit2)~predict(fit2))
X<-data.frame(yHat=rl$x,predictedR=rl$fitted)
X<-X[order(X[,1]),]
lines(X,col="black",lwd=1)

rl=loess(residuals(fit2)~predict(fit2))
y=predict(rl,se=TRUE)
segments(predict(fit2),y$fit+2*y$se.fit,predict(fit2),y$fit-2*y$se.fit,col="green")

#the green line is supposed to fit at the horizontal line for a good 
#model and the line is along the zero line which means it is a good model with 
#residuals alsomg zero. 


residualPlots(fit2)
#this plot shows how properly fitted the regression model is. 
#it plots each of the predictors against the pearson residuals. 
#if the model is properly fitted, there would be no correlation between 
#the predictor and the residuals. The trend would be a horizontal line or very 
#close to one. The lines for each of the predictors variables can approximate a 
#horizontal line. The lines are horizontal, the model can be said to be well fitted. 
#also, the stastiscal testing is done to check if any of the predictors is still 
#statistically significant. The idea is that if the model is properly specified, 
#no additional predictor variables that are statisifcally significant can be found. 
barplot(cooks.distance(fit2))
influenceIndexPlot(fit2,id.n = 3)
#observations 2 and 218 have the highest hat values, 132 and 229 have the largest 
#cook distance from the plot. this points could be outliers according to the cooks 
#distance. 
#if all the cook distance is high then the model is not well fitted to the data.
#In this case however, the cook distance are low and only few observations have 
#a relatively high. 

#influence.measures(fit2)

influencePlot(fit2, col = "red", id.n = 3)
#observations that have a substatinal effect in the estimates of coefficient are 
#called influential observations. Influence is a product of the leverage and 
#outliers. The observations 229 has the largest standardized residuals and second
#largest cook distance. 
#132 has the largest cook distance. 

vif(fit2)

#the variables do not show any correlation to each other as they 
#have a low value of vif for each of the predictor variables. This means the model 
#is not highly multicorrelated. 

#b
exp(coefficients(fit2))
exp(confint(fit2))
#the intercept indicate the log odds of the whole population of interest to be on 
#die from a heart attack with no predictor variables in the model.

#the model is a multivariate logistic model. 
#1. serum_creatinine: after adjusting for all the cofounders (ejection_fractio, 
#age, sex, creatinine_phosphokinase, serum_sodium and smoking), the odd ratio is
#1.88012, with 95% CI being 1.4426 and 2.6877. This means that with every 1 
#increase in the serum_creatinine level, the odd of dying from a heart attack 
#increases by 88%. 

#2. ejection fraction: after adjusting for all the cofounders (serum_creatinine, 
#age, sex, creatinine_phosphokinase, serum_sodium and smoking), the odd ratio is
#0.93219, with 95% CI being 0.90448 and 0.95852. This means that with every 1 
#increase in the ejection_fraction, the odd of dying from a heart attack 
#decreases by 6.79%. this means the chnances of survival increases by 6.79% for 
#a unit increase in ejection fracrtion. 

#3. age: after adjusting for all the cofounders (ejection_fraction, 
#serum_creatnine, sex, creatinine_phosphokinase, serum_sodium and smoking), the odd ratio is
#1.057, with 95% CI being  1.0316 and 1.08467. This means that with every 1 
#increase in the age, the odd of dying from a heart attack 
#increases by 5.7%.

#4. sex: after adjusting for all the cofounders (ejection_fraction, 
#age, serum_creatnine, creatinine_phosphokinase, serum_sodium and smoking), 
#the odd ratio is
#0.6222, with 95% CI being 0.3175 and 1.204.this means that the odd of surviving
#for male (sex=0) is 37.78% less likely as compared to female (sex = 0)

#5. creatinine_phosphokinase: after adjusting for all the cofounders (ejection_fraction, 
#serum_creatnine, sex, age, serum_sodium and smoking), the odd ratio is
#1.00002337, with 95% CI being  0.9999 and 1.000519. This means that with odd 
#ratio is very close to one which means the change in creatinine_phosphokinase 
#does not have a significant impact on the survival rate. 

#6: serum_sodium: after adjusting for all the cofounders (ejection_fraction, 
#serum_creatnine, sex, creatinine_phosphokinase, age and smoking), the odd ratio is
#0.9471, with 95% CI being  0.88635 and 1.01 This means that with every 1 
#increase in the serum_sodium, the odd of dying from a heart attack 
#decreases by 5.29%.this means the chances of survival increases by 5.29% for 
#for 1 increase in sodium_serum level. 


#c
#CrossTable(heartfailure$death, honors$, prop.chisq=F, prop.t=F, prop.c=F, prop.r =F)

#decision trees
heartfailure = read.csv("heartFailure.csv")
heartfailure$death = as.factor(heartfailure$death)

fitDT1<-rpart(death~serum_creatinine+ejection_fraction+age+sex+
                creatinine_phosphokinase+serum_sodium,data=heartfailure,                   
            parms=list(split="information"),   
            control=rpart.control(xval=20))


summary(fitDT1)

#to show the plot of the tree
prp(fitDT1, type=2, extra=104, nn=TRUE, fallen.leaves=TRUE,
    faclen=0,varlen=0, shadow.col="grey", branch.lty=3)


# colors the tree
fancyRpartPlot(fitDT1)

# or the party and partykit packages 
fitDTparty1<-as.party(fitDT1)
plot(fitDTparty1)
plotcp(fitDT1) 
#this plot shows the cp value that gives the minimum CV error. The minumimum 
#point in this case is the 0.029 which is closely followed by 0.098. 

pfit1<-prune(fitDT1,cp=0.029)
fancyRpartPlot(pfit1) #pruning at the level of cp = 0.029 doesnt make any diffrence 
#from the old tree. 
pfit<-prune(fitDT1,cp=0.098)  #and we can prune to this level. This level gives 
# a less complex tree with two leaves nodes. 
fancyRpartPlot(pfit)

evaluationfuction(fitDT1, heartfailure1, 'binary-class')


heartfailure1 = heartfailure
heartfailure1$pred<-predict(fitDT1, newdata=heartfailure, type = "prob")[,2] 

view(heartfailure1)
names(heartfailure1)[12] = "y"
names(heartfailure1)[13]="fitted.values"


#c)
heartfailure = read.csv("heartFailure.csv")
names(heartfailure)[12] = "true"
pearsonRes <-residuals(fit2,type="pearson")
devianceRes <-residuals(fit2,type="deviance")
rawRes <-residuals(fit2,type="response")
studentDevRes<-rstudent(fit2)
fv<-fitted(fit2)
#view(heartfailure)
#let's go ahead and create a classification based on the probability
heartfailure$pred<-as.numeric(fit2$fitted.values>0.5)


predVals <-  data.frame(trueVal=heartfailure$true, predClass=heartfailure$pred, 
                        predProb=fv, 
                        rawRes, pearsonRes, devianceRes, studentDevRes)

evaluationfuction(heartfailure, fit2, 'binary-class')

#heartfailure1$true = as.numeric(heartfailure1$true)

#heartfailure1 = read.csv("heartFailure.csv")
#heartfailure$pred
heartfailure$true
LogLoss(y_pred = fit2$fitted.values, y_true = heartfailure$pred)
heartfailure1$true
heartfailure1 = heartfailure


#the roc shows the tpr vs the fnr. This the most effective evaluation metrics 
#because it visualizes the accuracy of predictions for a whole range of cutoff 
#values. 
#if we had a perfect model, the ROC curve would pass through the upper left 
#corner — indicating no error. The most important takeout from a roc is the auc. 
#the auc is the measure of the ROC curve. The auc is 0.7992. Usually, 
#a auc above 0.7 is a good model. The closer the auc to 1, the better the model. 


#aucpr is the area under the precision and recall curve 


#Concordance: In how many pairs does the probability of ones is higher than the 
#probability of zeros divided by the total number of possible pairs. 
#The higher the values better is the model. 
#The value of concordance lies between 0 and 1.

#Similar to concordance, we have disconcordance which states in how many pairs 
#the probability of ones was less than zeros. If the probability of ones is 
#equal to 1 we say it is a tied pair.The number of tied pairs in this case is 0. 

#The dstatistics shows the difference between the predictions for the 1 and 0. 
#This shows the separation between the two classifications. The larger the value 
#of separation the better the model. The d statistics value for this model is 
#0.2483 

#K-S measures the degree of separation between the distributions 
#of the positive and negative death value. K-S = |cumulative % of total death — 
#cumulative % of  survivors|. The higher the value, the better the model is at 
#separating the positive from negative cases. If a model cannot separate positive 
#from negative cases, the K-S for all deciles will be 0. The best decile is the 
#decile 3 has it the highest separation. 

#using  the predictive 
#model using data from other patients,  "0 / 1" is 
#identified as the "target" field and other factors are used as predictors. 
#As a result of the predictive model, 
#we are able to sort entire patient list in decreasing order of 
#expected death. Consequently, rather than a random selection, the patients can 
#be sorted based on the most likely and so on.the lift line 
#(response before predictive model) indicates the gained 
#from using the predictive model.  Gain > 1 means the results from the 
#predictive model are better than random.

#A lift chart shows the actual lift, which is the ratio between results with an 
#without the model. In data mining, you can think of it as measuring “…the 
#change in concentration of a particular class when the model is used to select 
#a group from the general population.” The higher the lift 
#(i.e. the further up it is from the baseline), the better the model.
#the lift curves here shows the lift values against the cutoff. This can help 
#the lift vluae for the model for the cutoff vlaue of the model. 
