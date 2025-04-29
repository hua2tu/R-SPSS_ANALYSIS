#lasso回归分析



rm(list=ls())
library(glmnet)
library(rms)
library(openxlsx) 
library(Hmisc)
library(nortest)
library(car)
library(agricolae)
library(pROC)
library(mRMRe)
library(ggplot2)


#进行lasso分析
data<-read.xlsx("C:/Users/txh/Desktop/standardized_data2.xlsx")
data<-as.data.frame(data)
print(paste('总样本数:',nrow(data),ncol(data)))
# 缺失数据的识别
sum(is.na(data))  # 输出缺失值个数
colSums(is.na(data))      #查看每列缺失值数量
str(data)

data$fall_down<- factor(data$fall_down)
data$gender<- factor(data$gender)
data$hibpe<- factor(data$hibpe)
data$diabe<- factor(data$diabe)
data$cancre<- factor(data$cancre)
data$lunge<- factor(data$lunge)
data$hearte<- factor(data$hearte)
data$stroke<- factor(data$stroke)
data$psyche<- factor(data$psyche)
data$arthre<- factor(data$arthre)
data$dyslipe<- factor(data$dyslipe)
data$livere<- factor(data$livere)
data$kidneye<- factor(data$kidneye)
data$digeste<- factor(data$digeste)
data$memrye<- factor(data$memrye)
data$drinkl<- factor(data$drinkl)
data$smoken<- factor(data$smoken)
data$disability<- factor(data$disability)
data$eyesight_distance<- factor(data$eyesight_distance)
data$eyesight_close<- factor(data$eyesight_close)
data$hear<- factor(data$hear)
data$hip<- factor(data$hip)
data$edu_xxyx<- factor(data$edu_xxyx)
data$edu_xx<- factor(data$edu_xx)
data$edu_zx<- factor(data$edu_zx)
data$edu_gzjys<- factor(data$edu_gzjys)
data$grip<- factor(data$grip)


y<-data$fall_down
#除去因变量，提取自变量
yavars<-names(data) %in% c("fall_down")
X <- as.data.frame(data[!yavars])
X <- model.matrix(~.,data=X)[,-1]      #[,-1]是为了去掉截距
#Lasso回归
fit <- glmnet(X,y,alpha=1, family = "binomial")     #nlambda=100  可以选择lambda数量
plot(fit, xvar="lambda", label=TRUE)    #LASSO系数路径图"""
set.seed(123)
cv.fit <- cv.glmnet(X,y,alpha=1,nfolds = 10,family="binomial")
plot(cv.fit)     #LASSO正则化路径图（确定特征数量）
abline(v=log(c(cv.fit$lambda.min, cv.fit$lambda.1se)), lty=2)
plot(cv.fit$glmnet.fit,xvar="lambda")
abline(v=log(cv.fit$lambda.1se), lty=2) #把lambda取最小值时对应的虚线绘制到LASSO图中
best_model <- glmnet(X, y, alpha = 1,  family = "binomial",lambda = cv.fit$lambda.1se,intercept = T)
coef(best_model)

coefficients <- coef(best_model )
coefficients_df <- data.frame(
  variable = rownames(coefficients),
  coefficient = as.numeric(coefficients)
)

# 筛选出非零系数的变量
selected_variables <- coefficients_df[coefficients_df$coefficient != 0, ]
print(selected_variables)


#正式
data<-read.xlsx("C:/Users/txh/Desktop/standardized_data2.xlsx")
#subset_2 <- data[, c('fall_down','gender',  'adlab_c',  'cesd10','sleep','disability', 'eyesight_distance', 'hip','iadl','chronic pain')]
subset_2 <- data[,c(34,1,2,20,21,33,35,38,40,49)]
write.xlsx(subset_2,"C:/Users/txh/Desktop/3.xlsx",rowNames = FALSE )


data<-read.xlsx("C:/Users/txh/Desktop/standardized_data1.xlsx")
#subset_2 <- data[, c('fall_down','gender',  'adlab_c',  'arthre','livere','kidneye','memrye','drinkl','mwaist','cesd10','sleep','bl_plt','bl_hdl','disability', 'eyesight_distance', 'hip','iadl','edu_zx','grip','chronic pain')]
subset_2 <- data[,c(34,1,2,10,12,13,15,16,18,20,21,23,28,33,35,38,40,45,47,49)]
write.xlsx(subset_2,"C:/Users/txh/Desktop/3.xlsx",rowNames = FALSE )

#logistic回归分析
data<-read.xlsx("C:/Users/txh/Desktop/3.xlsx")
dim(data)
data$fall_down<- factor(data$fall_down)
data$gender<- factor(data$gender)
data$disability<- factor(data$disability)
data$eyesight_distance<- factor(data$eyesight_distance)
data$hip<- factor(data$hip)

fit<-glm(fall_down~.,family = binomial(link="logit"),data=data)
summary(fit)
exp(coef(fit))
exp(confint(fit))


library(caret)
data<-read.xlsx("C:/Users/txh/Desktop/3.xlsx")
data$fall_down<- factor(data$fall_down)
data$gender<- factor(data$gender)
data$disability<- factor(data$disability)
data$eyesight_distance<- factor(data$eyesight_distance)
data$hip<- factor(data$hip)

model.Omics <- glm(fall_down~.,data=data,binomial(link='logit'))
probTrainOmics<-predict.glm(object =model.Omics,newdata=data,type = "response")
predTrainOmics<-ifelse(probTrainOmics>=0.5,1,0)
xtab <- table(predTrainOmics, data$fall_down)   #两个变量必须为分类变量
confusionMatrix(xtab)

rocTrainOmicsA <- roc(data$fall_down, probTrainOmics,levels=c("1","0"),ci=T,auc=T) 
rocTrainOmicsA       #AUC及其95%CI
plot(rocTrainOmicsA,
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.4, print.auc.y=0.4, # 设置AUC值坐标为（x，y）
     col="red",    # 设置ROC曲线颜色
     auc.polygon = TRUE,
     auc.polygon.col = "azure",
     legacy.axes = TRUE,
     thresholds="best", # 基于youden指数选择roc曲线最佳阈值点
     print.thres="best",
     lty=1)



#临床变量合并影像组学（RS值）
dataClinic<-read.xlsx("C:/Users/txh/Desktop/3.xlsx")
dataClinic<-as.data.frame(dataClinic)
print(paste('总样本数:',nrow(dataClinic),ncol(dataClinic)))
#分割临床数据为train&test数据集
set.seed(888)
nn<-0.7
sub<-sample(1:nrow(dataClinic),round(nrow(dataClinic)*nn))
trainClicnic<-dataClinic[sub,]#取0.7的数据做训练集
testClicnic<-dataClinic[-sub,]#取0.3的数据做测试集
print(paste('训练集数:',nrow(trainClicnic),ncol(trainClicnic)))
print(paste('验证集数:',nrow(testClicnic),ncol(testClicnic)))
write.xlsx(trainClicnic,"C:/Users/txh/Desktop/05data/trainClinic.xlsx",rowNames = FALSE )
write.xlsx(testClicnic,"C:/Users/txh/Desktop/05data/testClinic.xlsx",rowNames = FALSE )

#机器学习分析
library(randomForest) 
library(e1071) 
library(caret)
#LR分析
#组学模型
trainClinicOmics<-read.xlsx("C:/Users/txh/Desktop/05data/trainClinic.xlsx")
trainClinicOmics$fall_down<-factor(trainClinicOmics$fall_down)
modelTrainOmicsLR <- glm(fall_down~.,data=trainClinicOmics,family = binomial(link="logit"))
summary(modelTrainOmicsLR)
#查看模型在训练集的验证
probLRTrainOmics<-predict(object =modelTrainOmicsLR,newdata=trainClinicOmics,type="response")   #预测分类
pred <- ifelse(probLRTrainOmics>=0.5,1,0)       #预测值
xtab <- table(pred, trainClinicOmics$fall_down)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainOmicsLR <- roc(trainClinicOmics$fall_down, probLRTrainOmics,levels=c("1","0"),ci=T,auc=T) 
rocTrainOmicsLR
#查看模型在验证集的验证
testClinicOmics<-read.xlsx("C:/Users/txh/Desktop/05data/testClinic.xlsx")
testClinicOmics$fall_down<-factor(testClinicOmics$fall_down)
probLRTestOmics<-predict(object =modelTrainOmicsLR,newdata=testClinicOmics,type="response")   #预测分类
pred <- ifelse(probLRTestOmics>=0.5,1,0)       #预测值
xtab <- table(pred, testClinicOmics$fall_down)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestOmicsLR <- roc(testClinicOmics$fall_down, probLRTestOmics,levels=c("1","0"),ci=T,auc=T) 
rocTestOmicsLR


#RF分析
#临床模型
set.seed(888)
#组学模型
modelTrainOmicsRF <- randomForest(fall_down~.,data=trainClinicOmics,importance=TRUE)
summary(modelTrainOmicsRF)
#查看模型在验证集的验证
pred<-predict(object =modelTrainOmicsRF,newdata=testClinicOmics)   #预测分类
probRFTestOmics <- predict(modelTrainOmicsRF, newdata = testClinicOmics, type = "prob")[,2]       #预测值
xtab <- table(pred, testClinicOmics$fall_down)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestOmicsRF <- roc(testClinicOmics$fall_down, probRFTestOmics,levels=c("0","1"),ci=T,auc=T) 
rocTestOmicsRF



#SVC分析
#svc分析数据处理 
trainClinicOmics<-read.xlsx("C:/Users/txh/Desktop/05data/trainClinic.xlsx")
trainClinicOmics$fall_down<-factor(trainClinicOmics$fall_down)


# 设置交叉验证参数
ctrl <- trainControl(method = "repeatedcv",  # 使用重复交叉验证
                     number = 10,           # 10折交叉验证
                     repeats = 10,          # 重复10次
                     classProbs = TRUE,     # 对于分类问题，计算类别概率
                     summaryFunction = multiClassSummary)  # 使用多类分类的评估指标

# 定义参数网格
svmGrid <- expand.grid(C = c(0.1, 1, 10, 100),  # cost参数
                       sigma = c(0.01, 0.1, 1))  # gamma参数

# 训练SVM模型
set.seed(123)  # 设置随机种子以确保结果可重复
svm_model <- train(fall_down~., 
                   data = trainClinicOmics, 
                   method = "svmRadial",  # 使用径向基核函数
                   trControl = ctrl, 
                   tuneGrid = svmGrid, 
                   metric = "Accuracy")  # 使用准确率作为评估指标

# 查看最佳模型参数
print(svm_model)

# 查看交叉验证结果
svm_model$results


#组学模型
modelTrainOmicsSVC <- svm(fall_down~.,data=trainClinicOmics,kernel = 'radial', type = "C-classification")

# 交叉验证
tuned_model <- tune.svm(fall_down~.,data=trainClinicOmics, kernel = "radial",type = "C-classification", cost = 10^(-1:2), gamma = c(0.1, 0.5, 1, 2))

# 查看最佳参数
summary(tuned_model)

# 预测
predictions <- predict(modelTrainOmicsSVC, testClinicOmics)

# 查看预测结果
xtab<-table(predictions, testClinicOmics$fall_down)

#在测试集上使用最佳参数配置进行预测
pred <- predict(modelTrainOmicsSVC, newdata = X_trainOmi, probability = TRUE, decision.values = TRUE)
xtab <- table(pred, Y_train)  
confusionMatrix(xtab,mode = "everything",positive="1")
probSVCTrainOmics <- attr(pred, "probabilities")#提取出模型预测的测试集的概率
rocTrainOmicsSVC <- roc(Y_train, probSVCTrainOmics[,2],levels=c("1","0"),ci=T,auc=T)
rocTrainOmicsSVC 
#在测试集上使用最佳参数配置进行预测
pred <- predict(modelTrainOmicsSVC, newdata = X_testOmi, probability = TRUE, decision.values = TRUE)
xtab <- table(pred, Y_test)  
confusionMatrix(xtab,mode = "everything",positive="1")
probSVCTestOmics <- attr(pred, "probabilities")#提取出模型预测的测试集的概率
rocTestOmicsSVC <- roc(Y_test, probSVCTestOmics[,2],levels=c("1","0"),ci=T,auc=T) 
rocTestOmicsSVC

