#第一步：数据读取与观察
#https://www.kaggle.com/c/titanic
titanic<-read.csv(file.choose())
#查看数据
summary(titanic)#population
head(titanic)#top 6

#第二步：数据处理
#1、数据属性规范
titanic$Pclass <- factor(titanic$Pclass)#因子类型定义，分类变量
#2、缺失值处理
sum(is.na(titanic$Age))
sum(is.na(titanic$Cabin))
summary(titanic$Cabin)
#可视化呈现
library(colorspace)
library(grid)
library(data.table)
library(VIM)
library(mice)
library(ggplot2)
NaPlot <- aggr(titanic,
               col=c("cyan", "red"), 
               numbers=TRUE, 
               labels=names(data), 
               cex.axis=.7, 
               gap=3, 
               ylab=c("Histogram  of missing data","Pattern")) #缺失值信息可视化

#Age缺失值处理
median(titanic$Age,na.rm = T)
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age,na.rm = T)
titanic$Embarked[is.na(titanic$Embarked)] <- 'S'
summary(titanic)

#第3步：探索性分析
#拆分训练集和测试集
#随机划分：
library(lattice)
library(caret) #版本不够
base <- data.frame(predict(dummyVars(~., data = titanic), titanic))
trainid <- createDataPartition(base$PassengerId, p = 0.75, list = F)
 train <- base[trainid,]
 test <- base[-trainid,]#方法错误
 #直接读取，全样本作为测试集（准确率偏高一些）
 train<-read.csv(file.choose())
 test<-read.csv(file.choose())
 
#（1）乘客所坐的船舱是如何影响生还率
library(ggplot2) 
Pclass_S <- table(train$Survived, train$Pclass) 
Pclass_S_prop <- prop.table(Pclass_S, 2) 
ggplot(data = train, aes(x = Pclass, fill = factor(Survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(1:3)) + labs(x = 'Pclass')
# 查看生还率
Pclass_S_prop

#（2）性别：泰坦尼克号上，人们是否也会遵循女士优先的准则呢？
Sex_S <- table(train$Survived, train$Sex)
Sex_S_prop <- prop.table(Sex_S, 2)
ggplot(data = train, aes(x = Sex, fill = factor(Survived)))+geom_bar(stat='count', position='dodge')

#（3）年龄的影响？
Agedata <- as.numeric(unlist(train$Age))
Age_Level<-cut(Agedata, breaks = c(0, 15, 30, 45, 60, 75, 90), labels = c('kids', 'teenagers', 'prime', 'middle', 'agedness', 'senium' ))
Age_S <- table(train$Survived, Age_Level)
Age_S_prop <- prop.table(Age_S, 2)
ggplot(data = data.frame(train$Survived, Agedata), aes(x = cut(Agedata, breaks = c(0, 15, 30, 45, 60, 75, 90)), fill = factor(train.Survived)))+geom_bar(stat='count', position='dodge') + labs(x = 'Age') +  scale_x_discrete(labels = c('kids', 'teenagers', 'prime', 'middle', 'agedness', 'senium'))

#(4)Family
Sibsp_S <- table(train$Survived, train$SibSp)
Parch_S <- table(train$Survived, train$Parch)
Sibsp_S_prop <- prop.table(Sibsp_S, 2)
Parch_S_prop <- prop.table(Parch_S, 2)
ggplot(data = train, aes(x = SibSp, fill = factor(Survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(0:8)) + labs(x = 'Sibsp')
ggplot(data = train, aes(x = Parch, fill = factor(Survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(0:6)) + labs(x = 'Parch')
Families <- train$SibSp +train$Parch
ggplot(data = train, aes(x = Families, fill = factor(Survived)))+geom_bar(stat='count', position='dodge') + scale_x_continuous(breaks=c(0:10)) + labs(x = 'Families')


#(5) 现金和港口
#一个乘客上船时所带的现金，以及他所登船的港口会对他成为幸存者有影响么？这两个看似和成为幸存者毫无关系的因素，可能正从侧面表现出了幸存者所拥有的属性。那么还是首先从简单的单因素统计绘图开始。其中将Fare这一变量分为三个区间，第一个区间为(0, 50]标签为poor，第二个区间为(50, 100]标签为middle，第三个区间为(100, 600]标签为rich。
Faredata <- as.numeric(unlist(train$fare))
Fare_S <- table(train$Survived, cut(Faredata, breaks = c(0, 50, 100, 600), labels = c('poor', 'middle', 'rich')))
Fare_S_prop <- prop.table(Fare_S, 2)
ggplot(data = data.frame(train$Survived, Faredata), aes(x = cut(Faredata, breaks = c(0, 50, 100, 600)), fill = factor(train.Survived)))+geom_bar(stat='count', position='dodge') + labs(x = 'Fare') +  scale_x_discrete(labels = c('poor', 'middle', 'rich'))
Embarked_S <- table(train$Survived, train$Embarked)
Embarked_S_prop <- prop.table(Embarked_S, 2)
ggplot(data = train, aes(x = Embarked, fill = factor(Survived)))+geom_bar(stat='count', position='dodge')

#过程4：规律揭示或建模

library(dplyr)
library(stringr)
library(colorspace)
library(grid)
library(data.table)
library(VIM)
library(mice)
library(ggplot2)
library(lattice)
library(caret) #版本不够
#Data
base <- data.frame(predict(dummyVars(~., data = titanic), titanic))
#逻辑回归方法
logistic <- glm(Survived ~Pclass+Sex+Age+SibSp+Parch+Fare,data = train[, -1],family = 'binomial'(link = 'logit'))
summary(logistic)

#K临近
set.seed(123)
result_KNN_R <- train[random, 1]
accuracy_KNN_R <- vector()
for(i in 1:30){
  KNN_M <- knn(train = Random_data[-random, 2:8], test = Random_data[random, 2:8], cl = Random_data[-random, 1], k = i)
  CT_KNN_R <- table(result_KNN_R, KNN_M)
  accuracy_KNN_R <- c(accuracy_KNN_R, sum(diag(CT_KNN_R))/sum(CT_KNN_R)*100)
}
accuracy_KNN_R_Max <- max(accuracy_KNN_R)

#随机森林法
library(randomForest)
library(ggplot2)
set.seed(123)
RF <- randomForest(factor(base$Survived) ~ ., data = base, importance = TRUE)

RF_tree <- plot(RF)
tree <- c(1:500)
OOB <- data.frame(tree, RF_tree)
ggplot(data = OOB, aes(x = tree))+geom_line(aes(y = OOB), colour = "black", size = 0.8)+geom_line(aes(y = X0), colour = "red", size = 0.8)+geom_line(aes(y = X1), colour = "green", size = 0.8) + labs(y = "Error.rate") + theme_bw()

#过程5：测试、预测与修正
test<-read.csv(file.choose())#test datsets,test3

test$predict <- predict(logistic, test, type='response')
test$predictClass <- NULL
test$predictClass[test$predict >= 0.5] <- 1 #逻辑回归得到的是一个概率值，而如何去划分需要我们指定，本例中大于等于0.5为正例，即：幸存。0.5也可以划分为0，已经测试过这个例子中不影响预测结
test$predictClass[test$predict < 0.5] <- 0
test$predictClass[is.na(test$predict)] <- 0
table(test$Survived, test$predictClass)
