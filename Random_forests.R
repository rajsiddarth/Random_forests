
rm(list=ls(all=TRUE))

library(RCurl)
data=read.table(text=getURL("https://raw.githubusercontent.com/rajsiddarth/Random_forests/master/disease_data.txt"),header=F, dec='.'
                               ,col.names=c('target','age','gender','steroid','antivirals',
                            'fatigue','malaise','anorexia','liverBig',
                            'liverFirm','spleen','spiders','ascites',
                            'varices','bili','alk','sgot','albu','protime',
                            'histology'),
                na.strings=c('?'), sep=',')
str(data)


#Converting to respective numeric and categorical data using variable description
num_Attr = c("age", "bili", "alk", "sgot", "albu", "protime")
cat_Attr = setdiff(names(data), num_Attr)
num_Data = data.frame(sapply(data[,num_Attr], as.numeric))
cat_Data = data.frame(sapply(data[,cat_Attr], as.factor))
data=cbind(num_Data,cat_Data)
str(data)

#Converting target to  1: Die(+ve) and 0: Live (-ve)
data$target= ifelse(data$target==1, 1, 0 ) 
sum(is.na(data))

#Using knn imputation to fill NA's
library(DMwR)
data = knnImputation(data = data, k = 5)
str(data)
data["target"]=data.frame(sapply(data["target"], as.factor))

#Splitting data to train and test sets

set.seed(1234)
#install.packages("caTools")
library(caTools)
temp=sample.split(data$target,SplitRatio = 0.7)
train_data=data[temp,]
test_data=data[!temp,]

# classification model using randomforest
#install.packages("randomForest")
library(randomForest)

model = randomForest(target ~ ., data=train_data,keep.forest=TRUE, ntree=30)

print(model)
# Important attributes
#Variable importance plots
model$importance
varImpPlot(model)

#Error metrics
# Predict on train data
pred_train = predict(model, train_data[,setdiff(names(train_data),"target")],
                     type="response", norm.votes=TRUE)

# confusion matrix and accuracy
cmatrix_train = table("Actual"= train_data$target, "Predicted" = pred_train)

accuracy_train= sum(diag(cmatrix_train))/sum(cmatrix_train)

# Predicton Test Data
pred_test = predict(model, test_data[,setdiff(names(test_data),"target")],
                    type="response", norm.votes=TRUE)

cmatrix_test = table("Actual"= test_data$target, "Predicted" = pred_test);

accuracy_test= sum(diag(cmatrix_test))/sum(cmatrix_test)





