read.csv("D:\\Swarnim\\Documents\\Advanced Certification in Data Analytics - IIT Madras\\Capstone Project\\census-income.data")->census
View(census)

library(gdata)

colnames(census)[colnames(census)=="X39"]<-"Age"
colnames(census)[colnames(census)=="Workclass"]<-"Income"
colnames(census)[colnames(census)=="Bachelors"]<-"Education"
colnames(census)[colnames(census)=="State.gov"]<-"Workclass"
colnames(census)[colnames(census)=="X13"]<-"Education_Num"
colnames(census)[colnames(census)=="Never.married"]<-"Maritial Status"
colnames(census)[colnames(census)=="Adm.clerical"]<-"Occupation"
colnames(census)[colnames(census)=="Not.in.family"]<-"Relationship"
colnames(census)[colnames(census)=="White"]<-"Race"
colnames(census)[colnames(census)=="Male"]<-"Gender"
colnames(census)[colnames(census)=="X2174"]<-"X1"
colnames(census)[colnames(census)=="X0"]<-"X2"
colnames(census)[colnames(census)=="X3"]<-"Hours.per.week"
colnames(census)[colnames(census)=="United.States"]<-"Country"
colnames(census)[colnames(census)=="Earnings"]<-"Capital.gain"

#1 Data Preprocessing :

#a) Replace all missing values with NA

library(dplyr)

census[census == " ?"] <- NA

#b). Remove all rows that contain NA Values

na.omit(census)->census

#c). Remove whitespaces
library(stringr)

census %>% mutate_if(is.character, str_trim)->census


#2). Data Manipulation :- 
library(stringr)
str(census)

#a). Extract education column and store in census_ed

census$Education -> census_ed
as.data.frame(census_ed) -> census_ed
View(census_ed)

#b). extract all the columns from age to relationship and store it in "census_seq"

census_seq=c(census[0:8])
as.data.frame(census_seq)->census_seq
View(census_seq)

#c). extract column number "5,8,11" and store it in "census_col"

census_col=c(census[5],census[8],census[11])
as.data.frame(census_col)->census_col
View(census_col)

#d). extract all male employees who work in state-gov and store it in male_gov

male_gov=(census$Gender =="Male" & census$Workclass =="State-gov")
subset(census,male_gov =="True")-> male_gov
as.data.frame(male_gov)->male_gov
View(male_gov)

#e). Extract all the 39 year olds who either have a bachelor's degree or who are native of
     #United States and store the result in “census_us”

census_us=(census$Age=="39" & (census$Education =="Bachelors" | census$Country =='United-States'))
subset(census,census_us=="True")->census_us
as.data.frame(census_us)->census_us
View(census_us)

#f). Extract 200 random rows from the “census” data frame and store it in “census_200”

sample_n(census,200)->census_200
View(census_200)

#g). Get the count of different levels of the “workclass” column.

count(census,"Workclass")
table(census$Workclass)

#h). Calculate the mean of “capital.gain” column grouped according to “workclass” 
##Column name in my dataset is "Income" not "Capital.gain"## 

census %>% group_by(Workclass) %>% summarise(mean(Income))
summarise(group_by(census,Workclass),mean(Income))

#3). Data Visualization :-

#a). Build a bar-plot for the “relationship” column and fill the bars according to the “race” column

ggplot(census, aes(x=Relationship, fill=Race))+xlab("Category of Relationships")+ylab("Count of Categories")+geom_bar()+ggtitle("Distribution of Relationships by Race")
ggplot(census, aes(x=Relationship, fill=Gender))+xlab("Category of Relationships")+ylab("Count of Categories")+geom_bar(position = "dodge")+ggtitle("Distribution of Relationships by Gender")  

#b). Build a Histogram for the “age” column with number of bins equal to 50.

ggplot(census, aes(x=Age, fill=x))+geom_histogram(bins=50)+ggtitle("Distribution of Age")+theme(panel.background=(element_rect(fill="black")),plot.background=(element_rect((fill="white"))))+guides(fill=guide_legend(title = "Yearly Income"))

#c). Build a scatter-plot between “capital.gain” and “hours.per.week”. Map “capital.gain” on the xaxis and “hours.per.week” on the y-axis.

ggplot(census, aes(x="Capital.gain", y="Hours.per.week", Col=X))+xlab("Capital Gain")+ylab("Hours per week")+ggtitle("Capital gain vs hours per week by income")+geom_point(size=2, alpha=0.6)+labs(col="Yearly Income")

#d).  Build a box-plot between “education” and “age” column.Map “education” on the x-axis and “age” on the y-axis.

ggplot(census, aes(x=Education, y=Age, fill=Gender))+geom_boxplot()+ggtitle("Box-plot of age with education and gender")


# 4.	Linear Regression:
census=read.csv("F:\\census-income_.csv")
library(ggplot2)
library(plotly)
library(reshape2)
library(caTools)
library(readr)

sample.split(census$hours.per.week, SplitRatio = 0.70)-> split_tag_census
subset(census, split_tag_census==T)->train_census
subset(census, split_tag_census==F)->test_census

nrow(census)
nrow(train_census)
nrow(test_census)

lm(hours.per.week~education.num,data=train_census)-> mod_census
summary(mod_census)

predict(mod_census,newdata = test_census)-> result_census
cbind(Actual=test_census$education.num,Predicted=result_census)-> final_data_census
as.data.frame(final_data_census)->final_data_census
final_data_census$Actual-final_data_census$Predicted->error_census
as.data.frame(error_census)->error_census
View(error_census)
cbind(final_data_census,error_census)->final_final_data_census
View(final_final_data_census)
plot(mod_census)
sqrt(mean(final_final_data_census$error_census)^2)-> RSME_census
RSME_census

# 5.	Logistic Regression:
census=read.csv("F:\\census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
library(ROCR)
library(pROC)

# a)	Build a simple logistic regression model as follows:

sample.split(census$X,SplitRatio = 0.65)->census_simlog
subset(census,census_simlog==T)->train_simlog
subset(census,census_simlog==F)->test_simlog
glm(as.factor(X)~occupation,data=train_simlog,family = "binomial")->mod_simlog
predict(mod_simlog,newdata=test_simlog,type="response")->result_simlog
prediction(result_simlog,test_simlog$X)->pred_simlog
performance(pred_simlog,"acc")->acc_simlog
plot(acc_simlog)
table(test_simlog$X,result_simlog>0.45)->mat_simlog
sum(diag(mat_simlog))/sum(mat_simlog)->acc1_simlog #0.78439
performance(pred_simlog,"tpr","fpr")->roc_simlog
plot(roc_simlog,colorize=T)
auc(test_simlog$X,result_simlog)->auc_simlog
auc_simlog #Area under the curve: 0.733

# b) Build a multiple logistic regression model as follows:
census=read.csv("F:\\census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
library(ROCR)
library(pROC)

sample.split(census$X,SplitRatio = 0.8)->census_mullog
subset(census,census_mullog==T)->train_mullog
subset(census,census_mullog==F)->test_mullog
glm(as.factor(X)~age+workclass+education,data=train_mullog,family = "binomial")->mod_mullog
predict(mod_mullog,newdata=test_mullog,type="response")->result_mullog
prediction(result_mullog,test_mullog$X)->pred_mullog
performance(pred_mullog,"acc")->acc_mullog
plot(acc_mullog)
table(test_mullog$X,result_mullog>0.45)->mat_mullog
sum(diag(mat_mullog))/sum(mat_mullog)->acc1_mullog #0.74017
performance(pred_mullog,"tpr","fpr")->roc_mullog
plot(roc_mullog,colorize=T)
auc(test_mullog$X,result_mullog)->auc_mullog
auc_mullog 

# 6.	Decision Tree:

census=read.csv("F:\\census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
library(tree)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(corrplot)
library(ggcorrplot)

sample.split(census$X,SplitRatio = 0.70)-> split_tag_census_dt
subset(census,split_tag_census_dt==T)->train_census_dt
subset(census,split_tag_census_dt==F)->test_census_dt
dim(train_census_dt)
dim(test_census_dt)

prop.table(table(train_census_dt$X)) 
prop.table(table(test_census_dt$X))

mod_census_dt=rpart(X~.,data=census,method="class")
rpart.plot(mod_census_dt)
predict(mod_census_dt,newdata = test_census_dt,type = "class")->predict_unseen_census_dt

table(test_census_dt$X,predict_unseen_census_dt)->table_mat_census_dt
table_mat_census_dt

sum(diag(table_mat_census_dt))/sum(table_mat_census_dt)->acc_census_dt
acc_census_dt
compare_census_dt=data.frame(Actual_data=test_census_dt$X,Predicted_data=predict_unseen_census_dt)
View(compare_census_dt)
table(compare_census_dt$Actual_data)
table(compare_census_dt$Predicted_data)

mutate(compare_census_dt,Check=ifelse(compare_census_dt$Actual_data==compare_census_dt$Predicted_data,"Matching","Not Matching"))->match_census_dt
View(match_census_dt)
table(match_census_dt$Check)

# 7.	Random Forest:
census=read.csv("F:\\census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(randomForest)
library(superml)

sample.split(census,SplitRatio=0.80)->split_tag1_census_rf
subset(census,split_tag1_census_rf==T)->train_census_rf
subset(census,split_tag1_census_rf==F)->test_census_rf
nrow(train_census_rf)
nrow(test_census_rf)
randomForest(X~.,data=train_census_rf,mtry=3,ntree=300)->mod_forest_census_rf

importance(mod_forest_census_rf)
varImpPlot(mod_forest_census_rf)
plot(mod_forest_census_rf)

predict(mod_forest_census_rf,newdata=test_census_rf,type="class")->result_forest_census_rf
head(result_forest_census_rf)

table(Actual=test_census_rf$X,Predicted=result_forest_census_rf)-> t_census_rf
View(t_census_rf)

plot(t_census_rf)

sum(diag(t_census_rf))/sum(t_census_rf)-> acc_census_rf
acc_census_rf