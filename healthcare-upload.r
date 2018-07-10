d_dt = read.csv("mental-heath-in-tech-2016_20161114.csv",
                check.names=FALSE)

rdata = c("Are you self-employed?",
          "Is your employer primarily a tech company/organization?",
          "Do you have a family history of mental illness?",
          "Have you had a mental health disorder in the past?",
          "Have you been diagnosed with a mental health condition by a medical professional?",
          "What is your age?",
          "What is your gender?",
          "What country do you work in?",
          "What US state or territory do you work in?")

nd_dt = d_dt[rdata]

colnames(nd_dt) <- c("self_employed",
                     "company_tech_or_not",
                     "family_history",
                     "past_mental_disorder",
                     "diagnosed",
                     "age",
                     "gender",
                     "work_location",
                     "work_location_state")



#Find Missing values
length(which(is.na(nd_dt)))
library(Hmisc)
#Changed all NAs in 2nd column to be val "2" - self employed status
nd_dt[,2] =  as.factor(impute(nd_dt$company_tech_or_not,2))
#Convert the categorical variables to factors - yes(1), no(0)
nd_dt$self_employed = as.factor(nd_dt$self_employed)

#Converting from string to int value yes(0), no(1), I don't know(2)
nd_dt$family_history = factor(nd_dt$family_history, 
                              levels = c("Yes", "No","I don't know"),
                              labels=c(0,1,2))

#Converting from string to int value yes(0), no(1), Maybe(2)
nd_dt$past_mental_disorder = factor(nd_dt$past_mental_disorder, 
                                    levels = c("Yes", "No","Maybe"),
                                    labels=c(0,1,2))

#Converting from string to int value yes(0), no(1)
nd_dt$diagnosed = factor(nd_dt$diagnosed, 
                        levels = c("Yes", "No"),
                        labels=c(0,1))
#Gender has many variables, so we will just clssify them to Male(0), Female(1), Other(2)
var = tolower(nd_dt$gender)
nd_dt$gender=as.factor(ifelse(var == "male" | var == "m" ,0, 
                  ifelse(var == "female" | var == "f",1,2)))

#Data exploration
library(ggplot2)
ggplot(nd_dt, aes(x = family_history, fill = diagnosed))+ geom_bar()
hist(nd_dt$age, xlim=c(10,70),xlab = "Age")
barplot(table(nd_dt$company_tech_or_not),
        xlab="Company Profile - Tech",ylab="Frequency",
        main="Tech Company or not",
        ylim = c(1000, 1500),
        names.arg = c("No", "Yes", "NA"))

#Checking number of outcome variables
table(nd_dt$diagnosed)/nrow(nd_dt)

#Splitting data into training and test set
library(caTools)
set.seed(123)
sdata = sample.split(nd_dt$diagnosed, SplitRatio = 0.8)

training_set = subset(nd_dt, sdata == T)
test_set = subset(nd_dt, sdata == F)

library(randomForest)
#Fitting the model
#to find best mtry
bestmtry = tuneRF(training_set, training_set$diagnosed,ntreeTry = 300,stepFactor = 1.2,improve = 0.01,
                  trace = T,plot=T)
bestmtry

#mtry - number of variables selected at each split(floor(sqrt(No. of independant vars)))
#low mtry - less correlation between trees(good), decreases strength of each tree(bad)
modelRandom = randomForest(diagnosed~.,data=training_set,mtry=bestmtry,ntree=300,importance = T)
modelRandom
#Extract 1 tree
getTree(modelRandom, 1)

#Finding the important variable
importance(modelRandom)
varImpPlot(modelRandom)

#Prediction
PredictionWithClass = predict(modelRandom, test_set, type = "class")
t = table(predictions = PredictionWithClass, actual = test_set$diagnosed)
t

#Accuracy
accuracy = (sum(diag(t))/sum(t))
accuracy

#Misclassifcation Rate - How often is the classifier wrong?
error_rate =(1-accuracy)
error_rate

#Plot ROC curve and calculate AUC metric
library(pROC)
PredictionWithProbs = predict(modelRandom, test_set, type = "prob")
auc = auc(test_set$diagnosed, PredictionWithProbs[,2])
auc
plot(roc(test_set$diagnosed, PredictionWithProbs[,2]))
