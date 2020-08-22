data=read.csv(file.choose())
data1=data


#feature engineering
data1$gender <- as.integer(data1$gender == "M")
data1$ssc_b <- as.integer(data1$ssc_b == "Central")
data1$hsc_b <- as.integer(data1$hsc_b == "Central")
data1$degree_t <- as.integer(data1$degree_t == "Sci&Tech")
data1$workex <- as.integer(data1$workex == "Yes")
data1$specialisation <- as.integer(data1$specialisation == "Mkt&HR")
data1$status <- as.integer(data1$status == "Placed")
data1$hsc_s <- ifelse(data1$hsc_s == "Science", 1, ifelse(data1$hsc_s == "Commerce", 2, ifelse(data1$hsc_s == "Arts", 3,     4)))
data1$ssc_p <- ifelse(data1$ssc_p >= 90, 1,ifelse((data1$ssc_p>=80)&(data1$ssc_p<90), 2,ifelse((data1$ssc_p>=70)&(data1$ssc_p<80), 3, ifelse((data1$ssc_p>=60)&(data1$ssc_p<70),4, ifelse((data1$ssc_p>=50)&(data1$ssc_p<60),5,ifelse((data1$ssc_p>=40)&(data1$ssc_p<50),6,7))))))
data1$hsc_p <- ifelse(data1$hsc_p >= 90, 1,ifelse((data1$hsc_p>=80)&(data1$hsc_p<90), 2,ifelse((data1$hsc_p>=70)&(data1$hsc_p<80), 3, ifelse((data1$hsc_p>=60)&(data1$hsc_p<70),4, ifelse((data1$hsc_p>=50)&(data1$hsc_p<60),5,ifelse((data1$hsc_p>=40)&(data1$hsc_p<50),6,7))))))
data1$degree_p <- ifelse(data1$degree_p >= 90, 1,ifelse((data1$degree_p>=80)&(data1$degree_p<90), 2,ifelse((data1$degree_p>=70)&(data1$degree_p<80), 3, ifelse((data1$degree_p>=60)&(data1$degree_p<70),4, ifelse((data1$degree_p>=50)&(data1$degree_p<60),5,ifelse((data1$degree_p>=40)&(data1$degree_p<50),6,7))))))
data1$etest_p=data2$etest_p
data1$mba_p=data2$mba_p
data1$etest_p <- ifelse(data1$etest_p >= 80, 1,ifelse((data1$etest_p>=60)&(data1$etest_p<80), 2,  3))
data1$mba_p <- ifelse((data1$mba_p>=70)&(data1$mba_p<80), 1, ifelse((data1$mba_p>=60)&(data1$mba_p<70),2, ifelse((data1$mba_p>=50)&(data1$mba_p<60),3,ifelse((data1$mba_p>=40)&(data1$mba_p<50),4,   5))))

data2=subset(data1,select=sl_no:status)

#spliting data
library(caTools)
set.seed(390)
split <- sample.split(data2, SplitRatio = 0.75)
split

train.data <- subset(data2, split== "TRUE")
test.data <- subset(data2, split== "FALSE")

logit_model1 <- glm(status ~ .,family=binomial(link='logit'),data = train.data[-c(1,2,7,9,11,12)])
summary(logit_model1)

fitted.results <- predict(logit_model1,newdata=test.data,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
library(caret)
confusionMatrix(table(test.data$status, fitted.results))
cm = table(test.data[, 14], fitted.results)
cm

data3=subset(data1,status==1)
set.seed(390)
split <- sample.split(data2, SplitRatio = 0.75)
split

train1.data <- subset(data2, split== "TRUE")
test1.data <- subset(data2, split== "FALSE")
my_sol=write.csv(data2,file="placement_mod.csv",row.names=FALSE)
