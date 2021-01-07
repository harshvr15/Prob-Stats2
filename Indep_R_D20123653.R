#libraries to load
library(readr)
library(lm.beta)
library(stargazer)
library(Epi)#ROC Curve
library(DescTools)#Pseudo Rsquare statistics
library(stargazer)
library(foreign)#read SPSS file.
library(arm)#for invlogit calculating predicted probabilities
library(lmtest)#Simple calculation of Chi-square for model
library(car)#Needed to test for colinearity of predictors
library(generalhoslem)#Needed to test assumption of linearity
library("regclass")#For confusion matrix
library(gmodels)
library(dplyr)
library(ROSE)
library(ggplot2)
library(semTools)
library(psych)

#reading the csv file from the local machine
df = read_csv("bank-additional-full.csv")

#dimension of the data frame
dim(df)

#searching for NA & Missing values
apply(is.na(df),2,sum)
colSums(df == "unknown")

#converting target variable from Yes/No to 0/1
df$y=ifelse(df$y=="yes",1,0)

#cleaning the unknown values for 5 tables based on their correlation with the target variable
CrossTable(df$job,df$y)
df = df %>% filter( job!= "unknown")

CrossTable(df$marital,df$y)
df = df %>% filter( marital!= "unknown")

CrossTable(df$education,df$y)
df$education[df$education=="unknown"]="university.degree"

CrossTable(df$default,df$y)

CrossTable(df$housing,df$y)
df = df %>% filter( housing!= "unknown")

#changing the values from yes/no to 0/1
df$housing = ifelse(df$housing == "yes",1,0)
df$default = ifelse(df$default == "yes",1,0)
df$loan = ifelse(df$loan == "yes",1,0)

#changing the datatype to factor
df$job <- as.numeric(as.factor(df$job))
df$marital <- as.numeric(as.factor(df$marital))
df$education <- as.numeric(as.factor(df$education))
df$month <- as.numeric(as.factor(df$month))
df$contact <- as.numeric(as.factor(df$contact))
df$poutcome <- as.numeric(as.factor(df$poutcome))

#sampling the data 
df=ovun.sample(y ~ ., data = df, method = "over",N = 70632)$data
-------------------------------------------------------------------------------------
#descriptive stats
psych::describeBy(df$education,df$y, mat=TRUE)
psych::describeBy(df$duration,df$y, mat=TRUE)

-------------------------------------------------------------------------------------
#normality for variable 1 :education
  
gg <- ggplot(df, aes(x=education))
gg <- gg+ggtitle("Histogram for education")
#Change the label of the x axis
gg <- gg + labs(x="Education")
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1.5, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
#adding a normal curve
#use stat_function to compute a normalised score for each value of education
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$education, na.rm=TRUE), sd=sd(df$education, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

#Create a qqplot
qqnorm(df$education, main="Figure 2 - QQ Plot")
qqline(df$education, col=2) #show a line on the plot

#get summary statistics
mean(df$education)      
sd(df$education)        
length(df$education)    

#skewness and kurtosis
eduskew<-semTools::skew(df$education)
edukurt<-semTools::kurtosis(df$education)
eduskew[1]/eduskew[2]
edukurt[1]/edukurt[2]

#checking the next level normailty outliers
zedu<- abs(scale(df$education))
FSA::perc(as.numeric(zedu), 1.96, "gt")
---------------------------------------------
  
#normality for variable 2 :duration
  
gg <- ggplot(df, aes(x=duration))
gg <- gg+ggtitle("Histogram for duration")
#Change the label of the x axis
gg <- gg + labs(x="Duration")
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
#adding a normal curve
#use stat_function to compute a normalised score for each value of duration
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$duration, na.rm=TRUE), sd=sd(df$duration, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

#Create a qqplot
qqnorm(df$duration, main="Figure 2 - QQ Plot")
qqline(df$duration, col=2) #show a line on the plot

#get summary statistics
mean(df$duration)
sd(df$duration)
length(df$duration)

#skewness and kurtosis
durationskew<-semTools::skew(df$duration)
durationkurt<-semTools::kurtosis(df$duration)
durationskew[1]/durationskew[2]
durationkurt[1]/durationkurt[2]

#checking the next level normailty outliers
zduration<- abs(scale(df$duration))
FSA::perc(as.numeric(zduration), 1.96, "gt") 
FSA::perc(as.numeric(zduration), 3.29, "gt") 
-------------------------------------------------------

#normality for variable 3 :age
  
gg <- ggplot(df, aes(x=age))
gg <- gg+ggtitle("Histogram for age")
#Change the label of the x axis
gg <- gg + labs(x="Age")
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
#adding a normal curve
#use stat_function to compute a normalised score for each value of age
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df$age, na.rm=TRUE), sd=sd(df$age, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

#Create a qqplot
qqnorm(df$age, main="Figure 2 - QQ Plot")
qqline(df$age, col=2) #show a line on the plot

#get summary statistics
mean(df$age)
sd(df$age)
length(df$age)

#skewness and kurtosis
ageskew<-semTools::skew(df$age)
agekurt<-semTools::kurtosis(df$age)
ageskew[1]/ageskew[2]
agekurt[1]/agekurt[2]

#checking the next level normailty outliers
zage<- abs(scale(df$age))
FSA::perc(as.numeric(zage), 1.96, "gt") 
FSA::perc(as.numeric(zage), 3.29, "gt")  
  
---------------------------------------------------------------------------- 
#LOGISIC REGRESSION
  
  
#MODEL1
logmodel1 <- glm(y ~ education+duration, data = df, na.action = na.exclude, family = binomial(link=logit))
#Full summary of the model
summary(logmodel1)
modelChi <- logmodel1$null.deviance - logmodel1$deviance
modelChi
pseudo.R2 <- modelChi / logmodel1$null.deviance
pseudo.R2
chidf <- logmodel1$df.null - logmodel1$df.residual
chidf
Epi::ROC(form=df$y ~ df$education+df$duration+df$age, plot="ROC")
exp(coefficients(logmodel1))
DescTools::PseudoR2(logmodel1, which="CoxSnell")
DescTools::PseudoR2(logmodel1, which="Nagelkerke")
stargazer(logmodel1, type="text") 


#Hosmer and Lemeshow test (binary model)
generalhoslem::logitgof(df$y, fitted(logmodel1))

#Collinearity
vifmodel<-car::vif(logmodel1)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
1/vifmodel

------------------------------------------------------------------------------
  
#MODEL2
logmodel2 <- glm(y ~ education+age+duration, data = df, na.action = na.exclude, family = binomial(link=logit))
#Full summary of the model
summary(logmodel2)
modelChi <- logmodel2$null.deviance - logmodel1$deviance
modelChi
pseudo.R2 <- modelChi / logmodel2$null.deviance
pseudo.R2
chidf <- logmodel2$df.null - logmodel2$df.residual
chidf
Epi::ROC(form=df$y ~ df$marital+df$age+df$education+df$duration, plot="ROC")
exp(coefficients(logmodel2))
DescTools::PseudoR2(logmodel2, which="CoxSnell")
DescTools::PseudoR2(logmodel2, which="Nagelkerke")
stargazer(logmodel2, type="text") 

#Hosmer and Lemeshow test (binary model)
generalhoslem::logitgof(df$y, fitted(logmodel2))

#Collinearity
vifmodel<-car::vif(logmodel2)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
1/vifmodel

---------------------------------------------------------------------------------

#COMPARISON
stargazer(logmodel1, logmodel2, type="text") #Quick model comparison
