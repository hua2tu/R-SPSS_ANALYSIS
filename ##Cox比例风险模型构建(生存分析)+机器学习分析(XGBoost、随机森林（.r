##生存分析and机器学习分析

rm(list=ls())
library(openxlsx) 
library(ggplot2)
library(pROC)
library(rms)    #绘制校准曲线
library(Hmisc)   #缺失值填补
library("survival") #生存分析
library("survminer") #结果可视化
library(pec)     #获得生存概率
library(survivalROC)   #绘制cox回归分析的roc曲线图
library(dcurves)    #绘制生存分析的决策曲线（DCA）
library(caret)       #计算准确度、灵敏度等指标

dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/data.xlsx")
dataClinic[,c(2:10,12,13,15:19,21,27)] <- data.frame(lapply(dataClinic[,c(2:10,12,13,15:19,21,27)], as.factor))
dataClinic$PLT<-as.numeric(dataClinic$PLT)
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
print(paste('总样本数:',nrow(dataClinic),ncol(dataClinic)))
# 缺失数据的识别
sum(is.na(dataClinic))  # 输出缺失值个数
colSums(is.na(dataClinic))      #查看每列缺失值数量
str(dataClinic)

"""多个连续性变量线性插补
# 计算每个变量的平均值
selected_vars <- c("ALb","ALp","AST","ALT","TBIL","PLT","WBC")
means <- apply(dataClinic[selected_vars], 1, mean, na.rm = TRUE)
dataClinic[,c(11,31,32,33,34)] <- sapply(dataClinic[,c(11,31,32,33,34)], function(x) ifelse(is.na(x), means[match(names(x), names(means)), 1], x))"""


#分类变量缺失值填补
if(sum(is.na(dataClinic$原发癌数)) > 0) {
  # 提取非缺失值
  non_missing_values <- dataClinic$原发癌数[!is.na(dataClinic$原发癌数)]
  
  # 随机选择填补值
  dataClinic$原发癌数[is.na(dataClinic$原发癌数)] <- sample(non_missing_values, size = sum(is.na(dataClinic$原发癌数)), replace = TRUE)
}
#连续性变量缺失值填补    采用均数进行填补缺失
a<-round(mean(dataClinic$肿瘤大小,na.rm=T),1)
dataClinic$肿瘤大小<-impute(dataClinic$肿瘤大小, a) 
a<-round(mean(dataClinic$ALb,na.rm=T),1)
dataClinic$ALb<-impute(dataClinic$ALb, a) 
a<-round(mean(dataClinic$ALp,na.rm=T),1)
dataClinic$ALp<-impute(dataClinic$ALp, a) 
a<-round(mean(dataClinic$AST,na.rm=T),1)
dataClinic$AST<-impute(dataClinic$AST, a) 
a<-round(mean(dataClinic$ALT,na.rm=T),1)
dataClinic$ALT<-impute(dataClinic$ALT, a) 
a<-round(mean(dataClinic$TBIL,na.rm=T),2)
dataClinic$TBIL<-impute(dataClinic$TBIL, a) 
a<-round(mean(dataClinic$PLT,na.rm=T),0)
dataClinic$PLT<-impute(dataClinic$PLT, a) 
a<-round(mean(dataClinic$WBC,na.rm=T),2)
dataClinic$WBC<-impute(dataClinic$WBC, a) 
#输出新数据
write.xlsx(dataClinic,"C:/Users/txh/Desktop/05data/dataClinic.xlsx",rowNames = FALSE )

#读取新数据
dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/dataClinic.xlsx")
print(paste('总样本数:',nrow(dataClinic),ncol(dataClinic)))
# 缺失数据的识别
sum(is.na(dataClinic))  # 输出缺失值个数
colSums(is.na(dataClinic))      #查看每列缺失值数量
str(dataClinic)
dataClinic[,c(2:10,12,13,15:19,21,27)] <- data.frame(lapply(dataClinic[,c(2:10,12,13,15:19,21,27)], as.factor))
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic[,c(11,31:37)] <- data.frame(lapply(dataClinic[,c(11,31:37)], as.numeric))
write.xlsx(dataClinic,"C:/Users/txh/Desktop/05data/dataClinic.xlsx",rowNames = FALSE )


#使用library(survminer)包中的surv_cutpoint()函数
res.cut <- surv_cutpoint(dataClinic, time = "time", event = "event",variables = c("肿瘤大小", "ALb", "ALp","AST","ALT","TBIL","PLT","WBC"))
summary(res.cut)
res.cat <- surv_categorize(res.cut)
head(res.cat)
#分类
dataClinic$tumorSizeG<- cut(dataClinic$肿瘤大小, breaks = c(-Inf, 5, Inf), labels = c("≤5",">5"), right=T)
dataClinic$ALbG<- cut(dataClinic$ALb, breaks = c(-Inf, 34, Inf), labels = c("≤34",">34"), right=T)
dataClinic$tumorALpG<- cut(dataClinic$ALp, breaks = c(-Inf, 154.6, Inf), labels = c("≤154.6",">154.6"), right=T)
dataClinic$tumorASTG<- cut(dataClinic$AST, breaks = c(-Inf, 36.1, Inf), labels = c("≤36.1",">36.1"), right=T)
dataClinic$tumorALTG<- cut(dataClinic$ALT, breaks = c(-Inf, 22, Inf), labels = c("≤22",">22"), right=T)
dataClinic$tumorTBILG<- cut(dataClinic$TBIL, breaks = c(-Inf, 36.3, Inf), labels = c("≤36.3",">36.3"), right=T)
dataClinic$tumorPLTG<- cut(dataClinic$PLT, breaks = c(-Inf, 244.0, Inf), labels = c("≤244.0",">244.0"), right=T)
dataClinic$tumorWBCG<- cut(dataClinic$WBC, breaks = c(-Inf, 7.2, Inf), labels = c("≤7.2",">7.2"), right=T)
#生成连续性变量转变为分类变量的数据
write.xlsx(dataClinic,"C:/Users/txh/Desktop/05data/dataClinicNew.xlsx",rowNames = FALSE )
"""             cutpoint statistic
确诊初治年龄       50  1.383711
(上述分析中忘了计算，现已经补充)"""


dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/dataClinicNew.xlsx")
dataClinic[,c(2:10,12:19)] <- data.frame(lapply(dataClinic[,c(2:10,12:19)], as.factor))
dataClinic$status<-as.numeric(dataClinic$status)
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic$肿瘤直径分组<-factor(dataClinic$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic$年龄分组<-factor(dataClinic$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic$ALb分组<-factor(dataClinic$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic$ALp分组<-factor(dataClinic$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic$AST分组<-factor(dataClinic$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic$ALT分组<-factor(dataClinic$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic$TBIL分组<-factor(dataClinic$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic$PLT分组<-factor(dataClinic$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic$WBC分组<-factor(dataClinic$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))
#dataClinic$status<-factor(dataClinic$status,levels=c("0","1"),labels=c("0","1"))
str(dataClinic)


#多个单因素cox回归分析
# 总结做单因素cox回归分析的特征
covariates <- c("乙肝", "丙肝", "肝切除术","放疗","化疗","系统性抗肿瘤治疗","消融治疗","TACE","原发癌数","肝硬化","T","N","M","民族","性别","现住址","婚姻状况",
"肿瘤直径分组","年龄分组","ALb分组","ALp分组","AST分组","ALT分组","TBIL分组","PLT分组","WBC分组")
# 分别对每一个变量，构建生存分析的公式
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))
# 循环对每一个特征做cox回归分析
univ_models <- lapply( univ_formulas, function(x){coxph(x, data = dataClinic)})
# 提取HR，95%置信区间和p值
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         #获取p值
                         p.value<-signif(x$wald["pvalue"], digits=3)
                         #获取HR
                         HR <-signif(x$coef[2], digits=3);
                         #获取95%置信区间
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 3)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],3)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(p.value,HR)
                         names(res)<-c("p.value","HR (95% CI for HR)")
                         return(res)
                       })
univ_results

#单因素cox回归分析中有统计学意义的变量 ：婚姻状况+肝切除术+化疗+消融治疗+TACE+原发癌数+肿瘤直径分组+肝硬化+T+N+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组
# 查看模型结果
cox_model <- coxph(Surv(time, status) ~ 婚姻状况+肝切除术+化疗+消融治疗+TACE+原发癌数+肿瘤直径分组+肝硬化+T+N+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, data=dataClinic)
# 查看模型结果
summary(cox_model)

"""多因素cox回归分析中有统计学意义的变量 ：婚姻状况+肝切除术+肝硬化+T分期+M分期+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组"""


#依据单因素Cox回归分析的变量使用随机数表法按照3:1的比例分为训练集与验证集
dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/dataClinicNew.xlsx")
str(dataClinic)
#划分训练集与验证集
set.seed(888)
nn<-0.75
print(paste('总样本数:',nrow(dataClinic),ncol(dataClinic)))
sub<-sample(1:nrow(dataClinic),round(nrow(dataClinic)*nn))
train<-dataClinic[sub,]#取0.75的数据做训练集
test<-dataClinic[-sub,]#取0.25的数据做测试集
print(paste('训练集数:',nrow(train),ncol(train)))
print(paste('验证集数:',nrow(test),ncol(test)))
write.xlsx(train,"C:/Users/txh/Desktop/05data/train0.75.xlsx",rowNames = FALSE )
write.xlsx(test,"C:/Users/txh/Desktop/05data/test0.25.xlsx",rowNames = FALSE )

#训练集与验证集之间的变量比较
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/train0.75.xlsx")
dataClinic1$id<-1
dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/test0.25.xlsx")
dataClinic2$id<-2
dataClinic<-rbind(dataClinic1,dataClinic2)
str(dataClinic)
write.xlsx(dataClinic,"C:/Users/txh/Desktop/05data/trainAndtest.xlsx",rowNames = FALSE )

dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/trainAndtest.xlsx")
dataClinic[,c(2:10,12:19)] <- data.frame(lapply(dataClinic[,c(2:10,12:19)], as.factor))
dataClinic$status<-as.numeric(dataClinic$status)
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic$肿瘤直径分组<-factor(dataClinic$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic$年龄分组<-factor(dataClinic$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic$ALb分组<-factor(dataClinic$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic$ALp分组<-factor(dataClinic$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic$AST分组<-factor(dataClinic$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic$ALT分组<-factor(dataClinic$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic$TBIL分组<-factor(dataClinic$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic$PLT分组<-factor(dataClinic$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic$WBC分组<-factor(dataClinic$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))
dataClinic$id<-factor(dataClinic$id,levels=c("1","2"),labels=c("1","2"))
str(dataClinic)

#t检验   比较良两组之间的临床特征差异
library(nortest)
library(car)
t.test(time ~ id, data = dataClinic, var.equal = TRUE,alternative="two.sided")
aggregate(time ~ id, data = dataClinic,sd)    #计算标准差
#进行非参数检验
wilcox.test(time ~ id, data = dataClinic)
tapply(dataClinic$time,dataClinic$id,quantile)

#卡方检验    比较良两组之间的临床特征差异
table<-table(dataClinic$婚姻状况,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)
fisher.test(table)

table<-table(dataClinic$肝切除术,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$化疗,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$消融治疗,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$TACE,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$原发癌数,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$肿瘤直径分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$肝硬化,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$T,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$N,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$M,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$ALb分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$ALp分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$AST分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$ALT分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$TBIL分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$PLT分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

rm(list=ls())
#列线图--训练集
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/train0.75.xlsx")
dataClinic1[,c(2:10,12:19)] <- data.frame(lapply(dataClinic1[,c(2:10,12:19)], as.factor))
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic1$肿瘤直径分组<-factor(dataClinic1$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic1$年龄分组<-factor(dataClinic1$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic1$ALb分组<-factor(dataClinic1$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic1$ALp分组<-factor(dataClinic1$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic1$AST分组<-factor(dataClinic1$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic1$ALT分组<-factor(dataClinic1$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic1$TBIL分组<-factor(dataClinic1$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic1$PLT分组<-factor(dataClinic1$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic1$WBC分组<-factor(dataClinic1$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/test0.25.xlsx")
dataClinic2[,c(2:10,12:19)] <- data.frame(lapply(dataClinic2[,c(2:10,12:19)], as.factor))
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic2$肿瘤直径分组<-factor(dataClinic2$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic2$年龄分组<-factor(dataClinic2$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic2$ALb分组<-factor(dataClinic2$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic2$ALp分组<-factor(dataClinic2$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic2$AST分组<-factor(dataClinic2$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic2$ALT分组<-factor(dataClinic2$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic2$TBIL分组<-factor(dataClinic2$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic2$PLT分组<-factor(dataClinic2$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic2$WBC分组<-factor(dataClinic2$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))

dd<-datadist(dataClinic1)
options(datadist="dd")
cox_model <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic1)
surv <- Survival(cox_model)
surv1 <- function(x)surv(12, x)
surv2 <- function(x)surv(36, x)
surv3 <- function(x)surv(60, x)
nomolung = nomogram(cox_model,fun = list(surv1, surv2, surv3), lp=T,fun.at = c(0, seq(0, 1, by = 0.1), 1),
funlabel = c('1-year survial', '3-year survival', '5-year survival'))
plot(nomolung)


#绘制训练集cox回归的ROC曲线图
t<-c(36,60)
survprob<-predictSurvProb(cox_model,newdata=dataClinic1,times=t)
head(survprob)
dataClinic1$survprob3<-survprob[,1]
dataClinic1$survprob5<-survprob[,2]

#绘制训练集决策曲线图
dataClinic1$f_cph_36<-1-dataClinic1$survprob3
dataClinic1$f_cph_60<-1-dataClinic1$survprob5

dca1 <- dcurves::dca(Surv(time,status) ~ f_cph_36 + f_cph_60,
                     data = dataClinic1,
                     time = 36,
                     label = list(f_cph_36 = "f_cph_36",
                                  f_cph_60 = "f_cph_60"))  %>%
  dcurves::as_tibble()
 
ggplot(dca1,aes(x=threshold, y=net_benefit,color=variable))+
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.2) +
  coord_cartesian(ylim = c(0, 0.6)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Risk Threshold (Train Set)", y = "Net Benefit", color = "") +
  theme_bw()

#绘制生存分析的ROC曲线
Mayo3= survivalROC(Stime=dataClinic1$time,##生存时间
                     status=dataClinic1$status,## 终止事件    
                     marker = -dataClinic1$survprob3, ## marker value    
                     predict.time = 36,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo3)
Mayo5= survivalROC(Stime=dataClinic1$time,##生存时间
                     status=dataClinic1$status,## 终止事件    
                     marker = -dataClinic1$survprob5, ## marker value    
                     predict.time = 60,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo5)
## 绘制3年生存率计算的ROC曲线
plot(Mayo3$FP, Mayo3$TP,
     type="l",col="red",
     xlim=c(0,1), ylim=c(0,1),   
     xlab="False positive rate", 
     ylab="True positive rate",
     main="ROC curves of the Cox regression model (Train Set)")
# 添加对角线
abline(0,1,col="gray",lty=2)
# 添加5年生存率计算的ROC曲线
lines(Mayo5$FP, Mayo5$TP, 
      type="l",col="blue",
      xlim=c(0,1), ylim=c(0,1))
# 添加图例
legend("bottomright",legend = c(paste("AUC at 3-years =", round(Mayo3$AUC,3)),
                                paste("AUC at 5-years =", round(Mayo5$AUC,3))),
       col=c("red","blue"),
       lty= 1 ,lwd= 2)

#绘制验证集cox回归的ROC曲线图
t<-c(36,60)
survprob<-predictSurvProb(cox_model,newdata=dataClinic2,times=t)
head(survprob)
dataClinic2$survprob3<-survprob[,1]
dataClinic2$survprob5<-survprob[,2]

#绘制训练集决策曲线图
dataClinic2$f_cph_36<-1-dataClinic2$survprob3
dataClinic2$f_cph_60<-1-dataClinic2$survprob5

dca2 <- dcurves::dca(Surv(time,status) ~ f_cph_36 + f_cph_60,
                     data = dataClinic2,
                     time = 36,
                     label = list(f_cph_36 = "f_cph_36",
                                  f_cph_60 = "f_cph_60"))  %>%
  dcurves::as_tibble()
 
ggplot(dca2,aes(x=threshold, y=net_benefit,color=variable))+
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.2) +
  coord_cartesian(ylim = c(0, 0.6)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Risk Threshold (Test Set)", y = "Net Benefit", color = "") +
  theme_bw()

#绘制验证集生存分析的ROC曲线
Mayo3= survivalROC(Stime=dataClinic2$time,##生存时间
                     status=dataClinic2$status,## 终止事件    
                     marker = -dataClinic2$survprob3, ## marker value    
                     predict.time = 36,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo3)
Mayo5= survivalROC(Stime=dataClinic2$time,##生存时间
                     status=dataClinic2$status,## 终止事件    
                     marker = -dataClinic2$survprob5, ## marker value    
                     predict.time = 60,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo5)
## 绘制3年生存率计算的ROC曲线
plot(Mayo3$FP, Mayo3$TP,
     type="l",col="red",
     xlim=c(0,1), ylim=c(0,1),   
     xlab="False positive rate", 
     ylab="True positive rate",
     main="ROC curves of the Cox regression model (Test Set)")
# 添加对角线
abline(0,1,col="gray",lty=2)
# 添加5年生存率计算的ROC曲线
lines(Mayo5$FP, Mayo5$TP, 
      type="l",col="blue",
      xlim=c(0,1), ylim=c(0,1))
# 添加图例
legend("bottomright",legend = c(paste("AUC at 3-years =", round(Mayo3$AUC,3)),
                                paste("AUC at 5-years =", round(Mayo5$AUC,3))),
       col=c("red","blue"),
       lty= 1 ,lwd= 2)


dd1<-datadist(dataClinic1)
options(datadist="dd1")
#绘制训练集（3年）校准曲线
# 将月份转换为天数
dataClinic1$time<-dataClinic1$time/12*365
cox_model3 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic1,time.inc = 365*3)
cal_train3<-calibrate(cox_model3,u=365*3,cmethod='KM',m=100)
#绘制train-3年校准图
plot(cal_train3,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 3-year overall survival (Train Set)',#x轴名称
     ylab='Actual 3-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围
#绘制训练集（5年）校准曲线
cox_model5 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic1,time.inc = 365*5)
cal_train5<-calibrate(cox_model5,u=365*5,cmethod='KM',m=100)
#绘制train-5年校准图
plot(cal_train5,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 5-year overall survival (Train Set)',#x轴名称
     ylab='Actual 5-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围


dd2<-datadist(dataClinic2)
options(datadist="dd2")
#绘制验证集（3年）校准曲线
# 将月份转换为天数
dataClinic2$time<-dataClinic2$time/12*365
cox_model3 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic2,time.inc = 365*3)
cal_test3<-calibrate(cox_model3,u=365*3,cmethod='KM',m=40)
#绘制train-3年校准图
plot(cal_test3,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 3-year overall survival (Test Set)',#x轴名称
     ylab='Actual 3-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围
#绘制验证集（5年）校准曲线
cox_model5 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic2,time.inc = 365*5)
cal_test5<-calibrate(cox_model5,u=365*5,cmethod='KM',m=40)
#绘制train-5年校准图
plot(cal_test5,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 5-year overall survival (Test Set)',#x轴名称
     ylab='Actual 5-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围




#使用机器学习模型进行分析
rm(list=ls())
library(randomForest)    #进行随机森林模型
library(e1071)     #支持向量机
library(caret)
library(rmda)   #绘制决策曲线
#其他机器学习方法
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/train0.75.xlsx")
dataClinic1[,c(2:10,12:19)] <- data.frame(lapply(dataClinic1[,c(2:10,12:19)], as.factor))
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic1$肿瘤直径分组<-factor(dataClinic1$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic1$年龄分组<-factor(dataClinic1$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic1$ALb分组<-factor(dataClinic1$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic1$ALp分组<-factor(dataClinic1$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic1$AST分组<-factor(dataClinic1$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic1$ALT分组<-factor(dataClinic1$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic1$TBIL分组<-factor(dataClinic1$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic1$PLT分组<-factor(dataClinic1$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic1$WBC分组<-factor(dataClinic1$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/test0.25.xlsx")
dataClinic2[,c(2:10,12:19)] <- data.frame(lapply(dataClinic2[,c(2:10,12:19)], as.factor))
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic2$肿瘤直径分组<-factor(dataClinic2$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic2$年龄分组<-factor(dataClinic2$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic2$ALb分组<-factor(dataClinic2$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic2$ALp分组<-factor(dataClinic2$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic2$AST分组<-factor(dataClinic2$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic2$ALT分组<-factor(dataClinic2$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic2$TBIL分组<-factor(dataClinic2$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic2$PLT分组<-factor(dataClinic2$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic2$WBC分组<-factor(dataClinic2$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))
#RF分析
#临床模型
set.seed(888)
#组学模型
modelTrainRF <- randomForest(status ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组,data=dataClinic1,ntree = 5,importance=TRUE)
summary(modelTrainRF)
#查看模型在训练集的验证
pred<-predict(object =modelTrainRF,newdata=dataClinic1)   #预测分类
probRFTrainOmics <- ifelse(pred>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainRF <- roc(dataClinic1$status, pred,levels=c("1","0"),ci=T,auc=T) 
rocTrainRF
#查看模型在验证集的验证
pred<-predict(object =modelTrainRF,newdata=dataClinic2)   #预测分类
probRFTestOmics <- ifelse(pred>=0.5,1,0)      #预测值
xtab <- table(probRFTestOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestRF <- roc(dataClinic2$status, pred,levels=c("1","0"),ci=T,auc=T) 
rocTestRF
#绘制校准曲线
dataClinic2$pred<-pred
log_model1 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal1 <- calibrate(log_model1, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal1,main="Calibration plot (RF model)")
#绘制决策曲线
simple1<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple1,curve.names=c('RF Model'),cost.benefit.axis =FALSE,col= c('red'),confidence.intervals=FALSE,standardize = FALSE)


#SVM分析
#查看模型在训练集的验证
svm_model <- svm(status ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组,data=dataClinic1)
predicted <- predict(svm_model , dataClinic1)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainSVM <- roc(dataClinic1$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTrainSVM
#查看模型在验证集的验证
predicted <- predict(svm_model , dataClinic2)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestSVM <- roc(dataClinic2$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTestSVM
#绘制校准曲线
dataClinic2$pred<-predicted
log_model2 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal2 <- calibrate(log_model2, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal2,main="Calibration plot (SVM model)")
#绘制决策曲线
simple2<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple2,curve.names=c('SVM Model'),cost.benefit.axis =FALSE,col= c('orange'),confidence.intervals=FALSE,standardize = FALSE)


library(xgboost)
#xgboost分析
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/train0.75.xlsx")
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-as.numeric(factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic1$肝切除术<-as.numeric(factor(dataClinic1$肝切除术,levels=c("是","否"),labels=c("1","0")))
dataClinic1$T<-as.numeric(factor(dataClinic1$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic1$M<-as.numeric(factor(dataClinic1$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic1$ALb分组<-as.numeric(factor(dataClinic1$ALb分组,levels=c("≤34",">34"),labels=c("1","2")))
dataClinic1$ALp分组<-as.numeric(factor(dataClinic1$ALp分组,levels=c("≤154.6",">154.6"),labels=c("1","2")))
dataClinic1$AST分组<-as.numeric(factor(dataClinic1$AST分组,levels=c("≤36.1",">36.1"),labels=c("1","2")))
dataClinic1$ALT分组<-as.numeric(factor(dataClinic1$ALT分组,levels=c("≤22",">22"),labels=c("1","2")))
dataClinic1$TBIL分组<-as.numeric(factor(dataClinic1$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("1","2")))
dataClinic1$PLT分组<-as.numeric(factor(dataClinic1$PLT分组,levels=c("≤244.0",">244.0"),labels=c("1","2")))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/test0.25.xlsx")
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-as.numeric(factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic2$肝切除术<-as.numeric(factor(dataClinic2$肝切除术,levels=c("是","否"),labels=c("1","0")))
dataClinic2$T<-as.numeric(factor(dataClinic2$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic2$M<-as.numeric(factor(dataClinic2$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic2$ALb分组<-as.numeric(factor(dataClinic2$ALb分组,levels=c("≤34",">34"),labels=c("1","2")))
dataClinic2$ALp分组<-as.numeric(factor(dataClinic2$ALp分组,levels=c("≤154.6",">154.6"),labels=c("1","2")))
dataClinic2$AST分组<-as.numeric(factor(dataClinic2$AST分组,levels=c("≤36.1",">36.1"),labels=c("1","2")))
dataClinic2$ALT分组<-as.numeric(factor(dataClinic2$ALT分组,levels=c("≤22",">22"),labels=c("1","2")))
dataClinic2$TBIL分组<-as.numeric(factor(dataClinic2$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("1","2")))
dataClinic2$PLT分组<-as.numeric(factor(dataClinic2$PLT分组,levels=c("≤244.0",">244.0"),labels=c("1","2")))
dataTrain<-as.matrix(dataClinic1[,c(4,13,15,19,34:39)])
dataTest<-as.matrix(dataClinic2[,c(4,13,15,19,34:39)])

dtrain <- xgb.DMatrix(data = dataTrain, label = dataClinic1$status)
model <- xgboost(data = dtrain, max.depth = 2, eta = 1, nrounds = 2,nthread = 2,objective = "binary:logistic")
##查看模型在训练集的验证
predicted <- predict(model, dataTrain)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainXGB <- roc(dataClinic1$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTrainXGB
#查看模型在验证集的验证
predicted <- predict(model , dataTest)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestXGB <- roc(dataClinic2$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTestXGB
#绘制校准曲线
dataClinic2$pred<-predicted
log_model3 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal3 <- calibrate(log_model3, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal3,main="Calibration plot (XGB model)")
#绘制决策曲线
simple3<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple3,curve.names=c('XGB Model'),cost.benefit.axis =FALSE,col= c('green'),confidence.intervals=FALSE,standardize = FALSE)


dataTrain<-as.matrix(dataClinic1[,c(4,13,15,19,22,34:39)])
dataTest<-as.matrix(dataClinic2[,c(4,13,15,19,22,34:39)])
#多层感知机（MLP）分析
library(nnet)
model <- nnet(status ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组,data=dataTrain,size=5,maxit=200)
##查看模型在训练集的验证
predicted <- predict(model, dataTrain)
probRFTrainOmics <- as.numeric(ifelse(predicted>=0.5,1,0))      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainMLP <- roc(dataClinic1$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTrainMLP
#查看模型在验证集的验证
predicted <- predict(model , dataTest)
probRFTrainOmics <- as.numeric(ifelse(predicted>=0.5,1,0))      #预测值
xtab <- table(probRFTrainOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestMLP <- roc(dataClinic2$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTestMLP
#绘制校准曲线
dataClinic2$pred<-predicted
log_model4 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal4 <- calibrate(log_model4, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal4,main="Calibration plot (MLP model)")
#绘制决策曲线
simple4<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple4,curve.names=c('MLP Model'),cost.benefit.axis =FALSE,col= c('blue'),confidence.intervals=FALSE,standardize = FALSE)


#将多个图绘制在同一个图片上
par(mfrow=c(2,2))        #构建2*2画布
plot(cal1,main="Calibration plot (RF model)")
plot(cal2,main="Calibration plot (SVM model)")
plot(cal3,main="Calibration plot (XGB model)")
plot(cal4,main="Calibration plot (MLP model)")

plot_decision_curve(simple1,curve.names=c('RF Model'),cost.benefit.axis =FALSE,col= c('red'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (RF model)")
plot_decision_curve(simple2,curve.names=c('SVM Model'),cost.benefit.axis =FALSE,col= c('orange'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (SVM model)")
plot_decision_curve(simple3,curve.names=c('XGB Model'),cost.benefit.axis =FALSE,col= c('green'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (XGB model)")
plot_decision_curve(simple4,curve.names=c('MLP Model'),cost.benefit.axis =FALSE,col= c('blue'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (MLP model)")

dev.off()         #关闭画布




#roc曲线绘制
###组学模型
#绘制训练集中不同模型的ROC曲线图
plot(rocTrainRF, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.4, # 设置AUC值坐标为（x，y）
     main="Train ROC curve",  # 添加图形标题
     col="red",  
     lty=1,
     legacy.axes=TRUE)   
# 再添加1条ROC曲线
plot(rocTrainSVM, 
     print.auc=TRUE, 
     print.auc.x=0.5, print.auc.y=0.35, 
     add=TRUE,
     col="orange",   
     lty=1) 
plot(rocTrainXGB, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.3, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="green",    # 设置ROC曲线颜色
     lty=1) 
plot(rocTrainMLP, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.25, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="blue",    # 设置ROC曲线颜色
     lty=1) 
# 添加图例
legend("bottomright",  # 图例位置x，y
       bty = "n",   # 图例样式
       legend=c("RF model","SVM model","XGB model","MLP model"),  # 添加分组
       col=c("red","orange","green","blue"),  # 颜色跟前面一致
       lwd=1,
       lty=1)  # 线条粗细   输出700*580

#绘制验证集中不同模型的ROC曲线图
plot(rocTestRF, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.4, # 设置AUC值坐标为（x，y）
     main="Validation ROC curve",  # 添加图形标题
     col="red",  
     lty=1,
     legacy.axes=TRUE)   
# 再添加1条ROC曲线
plot(rocTestSVM, 
     print.auc=TRUE, 
     print.auc.x=0.5, print.auc.y=0.35, 
     add=TRUE,
     col="orange",   
     lty=1) 
plot(rocTestXGB, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.3, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="green",    # 设置ROC曲线颜色
     lty=1) 
plot(rocTestMLP, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.25, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="blue",    # 设置ROC曲线颜色
     lty=1) 
# 添加图例
legend("bottomright",  # 图例位置x，y
       bty = "n",   # 图例样式
       legend=c("RF model","SVM model","XGB model","MLP model"),  # 添加分组
       col=c("red","orange","green","blue"),  # 颜色跟前面一致
       lwd=1,
       lty=1)  # 线条粗细   输出700*580


#SVM分析数据处理 
X_trainAll <- dataClinic1[,c(4,13,15,19,34:39)]
Y_train<-factor(dataClinic1[,c(22)],levels=c("0","1"),labels=c("0","1"))

X_testAll <- dataClinic2[,c(4,13,15,19,34:39)]
Y_test<-factor(dataClinic2[,c(22)],levels=c("0","1"),labels=c("0","1"))

modelTrainClinicOmicsSVC <- svm(x = X_trainAll, y = Y_train,kernel = 'sigmoid', C = 1.0 ,probability = TRUE)
#在测试集上使用最佳参数配置进行预测
pred <- predict(modelTrainClinicOmicsSVC, newdata = X_trainAll, probability = TRUE, decision.values = TRUE)
xtab <- table(pred, Y_train)  
confusionMatrix(xtab,mode = "everything",positive="1")
probSVCTrainClinicOmics <- attr(pred, "probabilities")#提取出模型预测的测试集的概率
rocTrainClinicOmicsSVC <- roc(Y_train, probSVCTrainClinicOmics[,2],levels=c("1","0"),ci=T,auc=T)
rocTrainClinicOmicsSVC 
#在测试集上使用最佳参数配置进行预测
pred <- predict(modelTrainClinicOmicsSVC, newdata = X_testAll, probability = TRUE, decision.values = TRUE)
xtab <- table(pred, Y_test)  
confusionMatrix(xtab,mode = "everything",positive="1")
probSVCTestClinicOmics <- attr(pred, "probabilities")#提取出模型预测的测试集的概率
rocTestClinicOmicsSVC <- roc(Y_test, probSVCTestClinicOmics[,2],levels=c("1","0"),ci=T,auc=T) 
rocTestClinicOmicsSVC


#采用网格搜索方法确定各种机器学习参数
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/train0.75.xlsx")
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-as.numeric(factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic1$肝切除术<-as.numeric(factor(dataClinic1$肝切除术,levels=c("是","否"),labels=c("1","0")))
dataClinic1$T<-as.numeric(factor(dataClinic1$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic1$M<-as.numeric(factor(dataClinic1$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic1$ALb分组<-as.numeric(factor(dataClinic1$ALb分组,levels=c("≤34",">34"),labels=c("1","2")))
dataClinic1$ALp分组<-as.numeric(factor(dataClinic1$ALp分组,levels=c("≤154.6",">154.6"),labels=c("1","2")))
dataClinic1$AST分组<-as.numeric(factor(dataClinic1$AST分组,levels=c("≤36.1",">36.1"),labels=c("1","2")))
dataClinic1$ALT分组<-as.numeric(factor(dataClinic1$ALT分组,levels=c("≤22",">22"),labels=c("1","2")))
dataClinic1$TBIL分组<-as.numeric(factor(dataClinic1$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("1","2")))
dataClinic1$PLT分组<-as.numeric(factor(dataClinic1$PLT分组,levels=c("≤244.0",">244.0"),labels=c("1","2")))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/test0.25.xlsx")
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-as.numeric(factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic2$肝切除术<-as.numeric(factor(dataClinic2$肝切除术,levels=c("是","否"),labels=c("1","0")))
dataClinic2$T<-as.numeric(factor(dataClinic2$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic2$M<-as.numeric(factor(dataClinic2$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic2$ALb分组<-as.numeric(factor(dataClinic2$ALb分组,levels=c("≤34",">34"),labels=c("1","2")))
dataClinic2$ALp分组<-as.numeric(factor(dataClinic2$ALp分组,levels=c("≤154.6",">154.6"),labels=c("1","2")))
dataClinic2$AST分组<-as.numeric(factor(dataClinic2$AST分组,levels=c("≤36.1",">36.1"),labels=c("1","2")))
dataClinic2$ALT分组<-as.numeric(factor(dataClinic2$ALT分组,levels=c("≤22",">22"),labels=c("1","2")))
dataClinic2$TBIL分组<-as.numeric(factor(dataClinic2$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("1","2")))
dataClinic2$PLT分组<-as.numeric(factor(dataClinic2$PLT分组,levels=c("≤244.0",">244.0"),labels=c("1","2")))


#XGBoost   极限梯度提升
library(xgboost)
library(ROCR)

# 准备预测变量和结果变量
train_features<-as.matrix(dataClinic1[,c(4,13,15,19,34:39)])
train_labels<-ifelse(dataClinic1$status == 1, dataClinic1$time, -dataClinic1$time)
#train_labels<-dataClinic1[,c(24,26)]
dtrain <- xgb.DMatrix(data = train_features, label = train_labels)
dtrain

param <- list(objective = "survival:cox",
              booster = "gbtree",
              eval_metric = "cox-nloglik",
              eta = 0.03,
              max_depth = 5,
              subsample = 1,
              colsample_bytree = 1,
              gamma = 0.5)
set.seed(1)
xgb.fit <- xgb.train(params = param, data = dtrain, nrounds = 1000, watchlist = list(val2 = dtrain),early_stopping_rounds = 50)
riskScore <- predict(xgb.fit, newdata = train_features)  # newdata如果是训练集可以获取训练集的风险分数
hist(riskScore)
#计算一致性C-index
library(Hmisc)
rcorr.cens(as.numeric(riskScore), Surv(dataClinic1$time, dataClinic1$status))
#ROC曲线分析
dataClinic1$predicted <- predict(xgb.fit, train_features)
dataClinic1$status<-factor(dataClinic1$status,levels=c("0","1"),labels=c("0","1"))
pred <- prediction(dataClinic1$predicted,dataClinic1$status)
TrianXgboost <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- round(performance(pred, "auc")@y.values[[1]],digits = 3)
plot(TrianXgboost, lwd = 2, col = "blue", main = paste("ROC curve (", "AUC = ",auc,")"), legacy.axes=T)
abline(a = 0, b = 1, col = 2, lwd = 1, lty = 2)

#验证集中xgboost生存分析
test_features<-as.matrix(dataClinic2[,c(4,13,15,19,34:39)])
test_labels<-ifelse(dataClinic2$status == 1, dataClinic2$time, -dataClinic2$time)
dtest <- xgb.DMatrix(data = test_features, label = test_labels)
dtest
#ROC曲线分析
dataClinic2$predicted <- predict(xgb.fit, test_features)
dataClinic2$status<-factor(dataClinic2$status,levels=c("0","1"),labels=c("0","1"))
pred <- prediction(dataClinic2$predicted,dataClinic2$status)
TestXgboost <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- round(performance(pred, "auc")@y.values[[1]],digits = 3)
plot(TestXgboost, lwd = 2, col = "blue", main = paste("ROC curve (", "AUC = ",auc,")"), legacy.axes=T)
abline(a = 0, b = 1, col = 2, lwd = 1, lty = 2)

calibration_data <- data.frame(pred = predsurvMLP, obs = dataClinic2$status)
fit<-lrm(obs ~ pred, data = calibration_data,x=TRUE,y=TRUE)
logistic_calib_obj <- calibrate(fit, method = "boot")
plot(cal1, xlim=c(0,1.0), ylim=c(0,1.0),
     xlab = "Predicted Probability", 
     ylab = "Observed Probability",
     legend = FALSE,subtitles = FALSE)

calibration_data <- data.frame(pred = predsurvMLP, obs = dataClinic2$status)                    
dca(obs ~ pred,data=calibration_data)


rm(list=ls())
#其他机器学习方法
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/train0.75.xlsx")
dataClinic1[,c(2:10,12:19)] <- data.frame(lapply(dataClinic1[,c(2:10,12:19)], as.factor))
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic1$肿瘤直径分组<-factor(dataClinic1$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic1$年龄分组<-factor(dataClinic1$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic1$ALb分组<-factor(dataClinic1$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic1$ALp分组<-factor(dataClinic1$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic1$AST分组<-factor(dataClinic1$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic1$ALT分组<-factor(dataClinic1$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic1$TBIL分组<-factor(dataClinic1$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic1$PLT分组<-factor(dataClinic1$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic1$WBC分组<-factor(dataClinic1$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/test0.25.xlsx")
dataClinic2[,c(2:10,12:19)] <- data.frame(lapply(dataClinic2[,c(2:10,12:19)], as.factor))
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic2$肿瘤直径分组<-factor(dataClinic2$肿瘤直径分组,levels=c("≤5",">5"),labels=c("≤5",">5"))
dataClinic2$年龄分组<-factor(dataClinic2$年龄分组,levels=c("≤50",">50"),labels=c("≤50",">50"))
dataClinic2$ALb分组<-factor(dataClinic2$ALb分组,levels=c("≤34",">34"),labels=c("≤34",">34"))
dataClinic2$ALp分组<-factor(dataClinic2$ALp分组,levels=c("≤154.6",">154.6"),labels=c("≤154.6",">154.6"))
dataClinic2$AST分组<-factor(dataClinic2$AST分组,levels=c("≤36.1",">36.1"),labels=c("≤36.1",">36.1"))
dataClinic2$ALT分组<-factor(dataClinic2$ALT分组,levels=c("≤22",">22"),labels=c("≤22",">22"))
dataClinic2$TBIL分组<-factor(dataClinic2$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("≤36.3",">36.3"))
dataClinic2$PLT分组<-factor(dataClinic2$PLT分组,levels=c("≤244.0",">244.0"),labels=c("≤244.0",">244.0"))
dataClinic2$WBC分组<-factor(dataClinic2$WBC分组,levels=c("≤7.2",">7.2"),labels=c("≤7.2",">7.2"))


#随机森林（RF）
library(ggRandomForests)
library(randomForestSRC)
set.seed(888)
rfsrc_pbc <- rfsrc(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, 
             data = dataClinic1,ntree=10,nsplit = 10, na.action = "na.impute",tree.err = TRUE,importance = TRUE,calibrate=TRUE)
rfsrc_pbc

pred <- predict(rfsrc_pbc, newdata = dataClinic1)
head(pred)
dataClinic1$status<-as.numeric(dataClinic1$status)
pred$yvar$status<-as.numeric(pred$yvar$status)
rocRFTrain <- roc(dataClinic1$status, pred$yvar$status)
rocRFTrain
plot(rocRFTrain, 
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", 
     grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), 
     print.thres=TRUE)

#验证集随机森林生存分析
pred2 <- predict(rfsrc_pbc, newdata = dataClinic2)
head(pred2)
dataClinic2$status<-as.numeric(dataClinic2$status)
pred2$yvar$status<-as.numeric(pred2$yvar$status)
rocRFTest <- roc(dataClinic2$status, pred2$yvar$status)
rocRFTest
plot(rocRFTest, 
     print.auc=TRUE, 
     auc.polygon=TRUE, 
     max.auc.polygon=TRUE, 
     auc.polygon.col="skyblue", 
     grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), 
     print.thres=TRUE)
#预测测试集
testSurv <- Surv(dataClinic2$time, dataClinic2$status)
testPred <- predict(rfsrc_pbc, newdata=dataClinic2, type="risk")

#决策曲线
out<- gg_rfsrc(rfsrc_pbc)       #获取预测概率值
calibration_data <- data.frame(pred = out$value, obs = dataClinic1$status)                    
dca(obs ~ pred,data=calibration_data)
#校准曲线
fit<-lrm(obs ~ pred, data = calibration_data,x=TRUE,y=TRUE)
logistic_calib_obj <- calibrate(fit, method = "boot")
plot(logistic_calib_obj, xlim=c(0,1.0), ylim=c(0,1.0),
     xlab = "Predicted Probability", 
     ylab = "Observed Probability",
     legend = FALSE,subtitles = FALSE)


#绘制校准曲线
calibration(rfsrcdataClinic2$time, dataClinic2$status))~testPred, method="KM")

#支持向量机（SVM）

library(survivalsvm)
survsvm.pr1 <- survivalsvm(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组,data = dataClinic1,
                           type = "regression", gamma.mu = 0.25,
                           opt.meth = "quadprog", kernel = "add_kernel")
predsurvsvm<- predict(object = survsvm.pr1,newdata = dataClinic1)
# 提取预测的生存概率
survival_prob <- predsurvsvm$predicted
# 计算ROC曲线
rocSVMTrain <- roc(response = dataClinic1$status, predictor = survival_prob)
plot(rocSVMTrain, main = "ROC Curve for Survival Analysis", col = "blue")

#验证集中的测试
predsurvsvm2<- predict(object = survsvm.pr1,newdata = dataClinic2)
# 提取预测的生存概率
survival_prob2 <- predsurvsvm2$predicted
# 计算ROC曲线
rocSVMTest <- roc(response = dataClinic2$status, predictor = survival_prob2)
plot(rocSVMTest, main = "ROC Curve for Survival Analysis", col = "blue")

dataClinic2$pred<-as.numeric(survival_prob2)

fit<-lrm(dataClinic2$status ~ dataClinic2$pred,x=TRUE,y=TRUE)
SVM_calib_obj <- calibrate(fit, method = "boot")
plot(SVM_calib_obj, xlim=c(0,1.0), ylim=c(0,1.0),
     xlab = "Predicted Probability", 
     ylab = "Observed Probability",
     legend = FALSE,subtitles = FALSE)
dca(dataClinic2$status ~ dataClinic2$pred)


#采用网格搜索方法确定各种机器学习参数
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/train0.75.xlsx")
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-as.numeric(factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic1$肝切除术<-as.numeric(factor(dataClinic1$肝切除术,levels=c("是","否"),labels=c("1","2")))
dataClinic1$T<-as.numeric(factor(dataClinic1$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic1$M<-as.numeric(factor(dataClinic1$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic1$ALb分组<-as.numeric(factor(dataClinic1$ALb分组,levels=c("≤34",">34"),labels=c("1","2")))
dataClinic1$ALp分组<-as.numeric(factor(dataClinic1$ALp分组,levels=c("≤154.6",">154.6"),labels=c("1","2")))
dataClinic1$AST分组<-as.numeric(factor(dataClinic1$AST分组,levels=c("≤36.1",">36.1"),labels=c("1","2")))
dataClinic1$ALT分组<-as.numeric(factor(dataClinic1$ALT分组,levels=c("≤22",">22"),labels=c("1","2")))
dataClinic1$TBIL分组<-as.numeric(factor(dataClinic1$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("1","2")))
dataClinic1$PLT分组<-as.numeric(factor(dataClinic1$PLT分组,levels=c("≤244.0",">244.0"),labels=c("1","2")))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/test0.25.xlsx")
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-as.numeric(factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic2$肝切除术<-as.numeric(factor(dataClinic2$肝切除术,levels=c("是","否"),labels=c("1","2")))
dataClinic2$T<-as.numeric(factor(dataClinic2$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic2$M<-as.numeric(factor(dataClinic2$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic2$ALb分组<-as.numeric(factor(dataClinic2$ALb分组,levels=c("≤34",">34"),labels=c("1","2")))
dataClinic2$ALp分组<-as.numeric(factor(dataClinic2$ALp分组,levels=c("≤154.6",">154.6"),labels=c("1","2")))
dataClinic2$AST分组<-as.numeric(factor(dataClinic2$AST分组,levels=c("≤36.1",">36.1"),labels=c("1","2")))
dataClinic2$ALT分组<-as.numeric(factor(dataClinic2$ALT分组,levels=c("≤22",">22"),labels=c("1","2")))
dataClinic2$TBIL分组<-as.numeric(factor(dataClinic2$TBIL分组,levels=c("≤36.3",">36.3"),labels=c("1","2")))
dataClinic2$PLT分组<-as.numeric(factor(dataClinic2$PLT分组,levels=c("≤244.0",">244.0"),labels=c("1","2")))

#多层感知机（MLP）模型
library(neuralnet)
#训练多层感知机模型
set.seed(888)
formula<- as.formula(paste("status~婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组"))
mlp_fit<- neuralnet(formula, data = dataClinic1,linear.output = FALSE)
#计算预测值
predsurvMLP<- predict(object = mlp_fit,newdata = dataClinic1)
#绘制ROC曲线
rocMLPTrain<- roc(response = dataClinic1$status, predictor = predsurvMLP)
plot(rocMLPTrain, main = "ROC Curve for Survival Analysis", col = "blue")
pred<-as.factor(ifelse(predsurvMLP>=0.5,1,0))
dataClinic1$status<-as.factor(dataClinic1$status)
confusionMatrix(pred, dataClinic1$status,, positive = "1")

#在验证集中验证
predsurvMLP<- predict(object = mlp_fit,newdata = dataClinic2)
#绘制ROC曲线
rocMLPTest<- roc(response = dataClinic2$status, predictor = predsurvMLP)
plot(rocMLPTest, main = "ROC Curve for Survival Analysis", col = "blue")
pred<-as.factor(ifelse(predsurvMLP>=0.5,1,0))
dataClinic2$status<-as.factor(dataClinic2$status)
confusionMatrix(pred, dataClinic2$status, positive = "1")
predsurvMLP<-as.numeric(predsurvMLP)

calibration_data <- data.frame(pred = predsurvMLP, obs = dataClinic2$status)

#绘制校准曲线与决策曲线
fit<-lrm(obs ~ pred, data = calibration_data,x=TRUE,y=TRUE)
logistic_calib_obj <- calibrate(fit, method = "boot")
plot(cal1, xlim=c(0,1.0), ylim=c(0,1.0),
     xlab = "Predicted Probability", 
     ylab = "Observed Probability",
     legend = FALSE,subtitles = FALSE)

calibration_data <- data.frame(pred = predsurvMLP, obs = dataClinic2$status)                    
dca(obs ~ pred,data=calibration_data)

nn_predictions <- compute(nn_model, df[, names(df) != c("time", "status", "event")])$net.result












##删除甲胎蛋白缺失数据个案后的数据分析（个案减少215个，合计564个）
rm(list=ls())
library(openxlsx) 
library(ggplot2)
library(pROC)
library(rms)    #绘制校准曲线
library(Hmisc)   #缺失值填补
library("survival") #生存分析
library("survminer") #结果可视化
library(pec)     #获得生存概率
library(survivalROC)   #绘制cox回归分析的roc曲线图
library(dcurves)    #绘制生存分析的决策曲线（DCA）
library(caret)       #计算准确度、灵敏度等指标

dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/肝癌数据汇总6.1.xlsx")
df_new <- subset(dataClinic, !is.na(dataClinic$`甲胎蛋白（AFP）`))
View(df_new)
write.xlsx(df_new,"C:/Users/txh/Desktop/05data/NewData.xlsx",rowNames = FALSE )

dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/NewData.xlsx")
dataClinic[,c(2:10,12:18,22)] <- data.frame(lapply(dataClinic[,c(2:10,12:18,22)], as.factor))
library(raster)
dataClinic$婚姻状况<-trim(dataClinic$婚姻状况)
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic[,c(11,20,23:31)] <- data.frame(lapply(dataClinic[,c(11,20,23:31)], as.numeric))
print(paste('总样本数:',nrow(dataClinic),ncol(dataClinic)))
# 缺失数据的识别
sum(is.na(dataClinic))  # 输出缺失值个数
colSums(is.na(dataClinic))      #查看每列缺失值数量
str(dataClinic)

#分类变量缺失值填补
if(sum(is.na(dataClinic$原发癌数)) > 0) {
  # 提取非缺失值
  non_missing_values <- dataClinic$原发癌数[!is.na(dataClinic$原发癌数)]
  # 随机选择填补值
  dataClinic$原发癌数[is.na(dataClinic$原发癌数)] <- sample(non_missing_values, size = sum(is.na(dataClinic$原发癌数)), replace = TRUE)
}
#连续性变量缺失值填补    采用均数进行填补缺失
a<-round(mean(dataClinic$肿瘤直径,na.rm=T),1)
dataClinic$肿瘤直径<-impute(dataClinic$肿瘤直径, a) 
a<-round(mean(dataClinic$ALb,na.rm=T),1)
dataClinic$ALb<-impute(dataClinic$ALb, a) 
a<-round(mean(dataClinic$ALp,na.rm=T),1)
dataClinic$ALp<-impute(dataClinic$ALp, a) 
a<-round(mean(dataClinic$AST,na.rm=T),1)
dataClinic$AST<-impute(dataClinic$AST, a) 
a<-round(mean(dataClinic$ALT,na.rm=T),1)
dataClinic$ALT<-impute(dataClinic$ALT, a) 
a<-round(mean(dataClinic$TBIL,na.rm=T),2)
dataClinic$TBIL<-impute(dataClinic$TBIL, a) 
a<-round(mean(dataClinic$PLT,na.rm=T),0)
dataClinic$PLT<-impute(dataClinic$PLT, a) 
#输出新数据
write.xlsx(dataClinic,"C:/Users/txh/Desktop/05data/New/dataClinic.xlsx",rowNames = FALSE )

#读取新数据
dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/New/dataClinic.xlsx")
print(paste('总样本数:',nrow(dataClinic),ncol(dataClinic)))
# 缺失数据的识别
sum(is.na(dataClinic))  # 输出缺失值个数
colSums(is.na(dataClinic))      #查看每列缺失值数量
str(dataClinic)
dataClinic[,c(2:10,12:18,22)] <- data.frame(lapply(dataClinic[,c(2:10,12:18,22)], as.factor))
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic[,c(11,20,23:31)] <- data.frame(lapply(dataClinic[,c(11,20,23:31)], as.numeric))


#使用library(survminer)包中的surv_cutpoint()函数
res.cut <- surv_cutpoint(dataClinic, time = "time", event = "status",variables = 
c("确诊初治年龄","肿瘤直径", "ALb", "ALp","AFP","AST","ALT","TBIL","PLT","WBC"),minprop = 0.4)
summary(res.cut)
res.cat <- surv_categorize(res.cut)
head(res.cat)
#分类
dataClinic$肿瘤直径分组<- cut(dataClinic$肿瘤直径, breaks = c(-Inf, 5.0, Inf), labels = c("≤5.0",">5.0"), right=T)
dataClinic$年龄分组<- cut(dataClinic$确诊初治年龄, breaks = c(-Inf, 56.0, Inf), labels = c("≤56.0",">56.0"), right=T)
dataClinic$ALb分组<- cut(dataClinic$ALb, breaks = c(-Inf, 37.8, Inf), labels = c("≤37.8",">37.8"), right=T)
dataClinic$ALp分组<- cut(dataClinic$ALp, breaks = c(-Inf, 106.2, Inf), labels = c("≤106.2",">106.2"), right=T)
dataClinic$AFP分组<- cut(dataClinic$AFP, breaks = c(-Inf, 11.9, Inf), labels = c("≤11.9",">11.9"), right=T)
dataClinic$AST分组<- cut(dataClinic$AST, breaks = c(-Inf, 38.7, Inf), labels = c("≤38.7",">38.7"), right=T)
dataClinic$ALT分组<- cut(dataClinic$ALT, breaks = c(-Inf, 34.7, Inf), labels = c("≤34.7",">34.7"), right=T)
dataClinic$TBIL分组<- cut(dataClinic$TBIL, breaks = c(-Inf, 20.52, Inf), labels = c("≤20.5",">20.5"), right=T)
dataClinic$PLT分组<- cut(dataClinic$PLT, breaks = c(-Inf, 139.0, Inf), labels = c("≤139.0",">139.0"), right=T)
dataClinic$WBC分组<- cut(dataClinic$WBC, breaks = c(-Inf, 5.9, Inf), labels = c("≤5.9",">5.9"), right=T)
#生成连续性变量转变为分类变量的数据
write.xlsx(dataClinic,"C:/Users/txh/Desktop/05data/New/dataClinicNew.xlsx",rowNames = FALSE )
"""             cutpoint statistic
确诊初治年龄       50  1.383711
(上述分析中忘了计算，现已经补充)"""


dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/New/dataClinicNew.xlsx")
dataClinic[,c(2:10,12:18)] <- data.frame(lapply(dataClinic[,c(2:10,12:18)], as.factor))
dataClinic$status<-as.numeric(dataClinic$status)
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic$肿瘤直径分组<-factor(dataClinic$肿瘤直径分组,levels=c("≤5.0",">5.0"),labels=c("≤5.0",">5.0"))
dataClinic$年龄分组<-factor(dataClinic$年龄分组,levels=c("≤56.0",">56.0"),labels=c("≤56.0",">56.0"))
dataClinic$ALb分组<-factor(dataClinic$ALb分组,levels=c("≤37.8",">37.8"),labels=c("≤37.8",">37.8"))
dataClinic$ALp分组<-factor(dataClinic$ALp分组,levels=c("≤106.2",">106.2"),labels=c("≤106.2",">106.2"))
dataClinic$AFP分组<-factor(dataClinic$AFP分组,levels=c("≤11.9",">11.9"),labels=c("≤11.9",">11.9"))
dataClinic$AST分组<-factor(dataClinic$AST分组,levels=c("≤38.7",">38.7"),labels=c("≤38.7",">38.7"))
dataClinic$ALT分组<-factor(dataClinic$ALT分组,levels=c("≤34.7",">34.7"),labels=c("≤34.7",">34.7"))
dataClinic$TBIL分组<-factor(dataClinic$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("≤20.5",">20.5"))
dataClinic$PLT分组<-factor(dataClinic$PLT分组,levels=c("≤139.0",">139.0"),labels=c("≤139.0",">139.0"))
dataClinic$WBC分组<-factor(dataClinic$WBC分组,levels=c("≤5.9",">5.9"),labels=c("≤5.9",">5.9"))
str(dataClinic)


#多个单因素cox回归分析
# 总结做单因素cox回归分析的特征
covariates <- c("民族","性别","现住址","婚姻状况","乙肝", "丙肝", "肝切除术","放疗","化疗","系统性抗肿瘤治疗","消融治疗","TACE","原发癌数","肝硬化","T","N","M",
"肿瘤直径分组","年龄分组","ALb分组","ALp分组","AFP分组","AST分组","ALT分组","TBIL分组","PLT分组","WBC分组")
# 分别对每一个变量，构建生存分析的公式
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))
# 循环对每一个特征做cox回归分析
univ_models <- lapply( univ_formulas, function(x){coxph(x, data = dataClinic)})
# 提取HR，95%置信区间和p值
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         #获取p值
                         p.value<-signif(x$wald["pvalue"], digits=3)
                         #获取HR
                         HR <-signif(x$coef[2], digits=3);
                         #获取95%置信区间
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 3)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],3)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(p.value,HR)
                         names(res)<-c("p.value","HR (95% CI for HR)")
                         return(res)
                       })
univ_results

#单因素cox回归分析中有统计学意义的变量 ：单因素cox回归分析中有统计学意义的变量 ：
#肝切除术+化疗+消融治疗+TACE+原发癌数+肿瘤直径分组+T+N+M+ALb分组+ALp分组+AFP分组+AST分组+ALT分组+TBIL分组+WBC分组
cox_model <- coxph(Surv(time, status) ~ 肝切除术+化疗+消融治疗+TACE+原发癌数+肿瘤直径分组+T+N+M+ALb分组+ALp分组+AFP分组+AST分组+ALT分组+TBIL分组+WBC分组, data=dataClinic)
# 查看模型结果
summary(cox_model)

"""多因素cox回归分析中有统计学意义的变量 ：肝切除术+T分期+M分期+ALb分组+AFP分组+AST分组+PLT分组"""


#依据单因素Cox回归分析的变量使用随机数表法按照3:1的比例分为训练集与验证集
dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/New/dataClinicNew.xlsx")
str(dataClinic)
#划分训练集与验证集
set.seed(888)
nn<-0.75
print(paste('总样本数:',nrow(dataClinic),ncol(dataClinic)))
sub<-sample(1:nrow(dataClinic),round(nrow(dataClinic)*nn))
train<-dataClinic[sub,]#取0.75的数据做训练集
test<-dataClinic[-sub,]#取0.25的数据做测试集
print(paste('训练集数:',nrow(train),ncol(train)))
print(paste('验证集数:',nrow(test),ncol(test)))
write.xlsx(train,"C:/Users/txh/Desktop/05data/New/train0.75.xlsx",rowNames = FALSE )
write.xlsx(test,"C:/Users/txh/Desktop/05data/New/test0.25.xlsx",rowNames = FALSE )

#训练集与验证集之间的变量比较
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/New/train0.75.xlsx")
dataClinic1$id<-1
dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/New/test0.25.xlsx")
dataClinic2$id<-2
dataClinic<-rbind(dataClinic1,dataClinic2)
str(dataClinic)
write.xlsx(dataClinic,"C:/Users/txh/Desktop/05data/New/trainAndtest.xlsx",rowNames = FALSE )

dataClinic<-read.xlsx("C:/Users/txh/Desktop/05data/New/trainAndtest.xlsx")
dataClinic[,c(2:10,12:18)] <- data.frame(lapply(dataClinic[,c(2:10,12:18)], as.factor))
dataClinic$status<-as.numeric(dataClinic$status)
dataClinic$婚姻状况<-factor(dataClinic$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic$肿瘤直径分组<-factor(dataClinic$肿瘤直径分组,levels=c("≤5.0",">5.0"),labels=c("≤5.0",">5.0"))
dataClinic$年龄分组<-factor(dataClinic$年龄分组,levels=c("≤56.0",">56.0"),labels=c("≤56.0",">56.0"))
dataClinic$ALb分组<-factor(dataClinic$ALb分组,levels=c("≤37.8",">37.8"),labels=c("≤37.8",">37.8"))
dataClinic$ALp分组<-factor(dataClinic$ALp分组,levels=c("≤106.2",">106.2"),labels=c("≤106.2",">106.2"))
dataClinic$AFP分组<-factor(dataClinic$AFP分组,levels=c("≤11.9",">11.9"),labels=c("≤11.9",">11.9"))
dataClinic$AST分组<-factor(dataClinic$AST分组,levels=c("≤38.7",">38.7"),labels=c("≤38.7",">38.7"))
dataClinic$ALT分组<-factor(dataClinic$ALT分组,levels=c("≤34.7",">34.7"),labels=c("≤34.7",">34.7"))
dataClinic$TBIL分组<-factor(dataClinic$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("≤20.5",">20.5"))
dataClinic$PLT分组<-factor(dataClinic$PLT分组,levels=c("≤139.0",">139.0"),labels=c("≤139.0",">139.0"))
dataClinic$WBC分组<-factor(dataClinic$WBC分组,levels=c("≤5.9",">5.9"),labels=c("≤5.9",">5.9"))
dataClinic$id<-factor(dataClinic$id,levels=c("1","2"),labels=c("1","2"))
str(dataClinic)

#t检验   比较良两组之间的临床特征差异
library(nortest)
library(car)
t.test(time ~ id, data = dataClinic, var.equal = TRUE,alternative="two.sided")
aggregate(time ~ id, data = dataClinic,sd)    #计算标准差
#进行非参数检验
wilcox.test(time ~ id, data = dataClinic)
tapply(dataClinic$time,dataClinic$id,quantile)

#卡方检验    比较良两组之间的临床特征差异
table<-table(dataClinic$婚姻状况,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)
fisher.test(table)

table<-table(dataClinic$肝切除术,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$化疗,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$消融治疗,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$TACE,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$原发癌数,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$肿瘤直径分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$肝硬化,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$T,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)
fisher.test(table)

table<-table(dataClinic$N,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$M,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$ALb分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$ALp分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$AFP分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$AST分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$ALT分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$TBIL分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

table<-table(dataClinic$WBC分组,dataClinic$id)
table
prop.table(table,margin = 2)
chisq.test(table)$expected
chisq.test(table,correct=FALSE)

rm(list=ls())
#列线图--训练集
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/New/train0.75.xlsx")
dataClinic1[,c(2:10,12:18)] <- data.frame(lapply(dataClinic1[,c(2:10,12:18)], as.factor))
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic1$肿瘤直径分组<-factor(dataClinic1$肿瘤直径分组,levels=c("≤5.0",">5.0"),labels=c("≤5.0",">5.0"))
dataClinic1$年龄分组<-factor(dataClinic1$年龄分组,levels=c("≤56.0",">56.0"),labels=c("≤56.0",">56.0"))
dataClinic1$ALb分组<-factor(dataClinic1$ALb分组,levels=c("≤37.8",">37.8"),labels=c("≤37.8",">37.8"))
dataClinic1$ALp分组<-factor(dataClinic1$ALp分组,levels=c("≤106.2",">106.2"),labels=c("≤106.2",">106.2"))
dataClinic1$AFP分组<-factor(dataClinic1$AFP分组,levels=c("≤11.9",">11.9"),labels=c("≤11.9",">11.9"))
dataClinic1$AST分组<-factor(dataClinic1$AST分组,levels=c("≤38.7",">38.7"),labels=c("≤38.7",">38.7"))
dataClinic1$ALT分组<-factor(dataClinic1$ALT分组,levels=c("≤34.7",">34.7"),labels=c("≤34.7",">34.7"))
dataClinic1$TBIL分组<-factor(dataClinic1$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("≤20.5",">20.5"))
dataClinic1$PLT分组<-factor(dataClinic1$PLT分组,levels=c("≤139.0",">139.0"),labels=c("≤139.0",">139.0"))
dataClinic1$WBC分组<-factor(dataClinic1$WBC分组,levels=c("≤5.9",">5.9"),labels=c("≤5.9",">5.9"))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/New/test0.25.xlsx")
dataClinic2[,c(2:10,12:18)] <- data.frame(lapply(dataClinic2[,c(2:10,12:18)], as.factor))
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic2$肿瘤直径分组<-factor(dataClinic2$肿瘤直径分组,levels=c("≤5.0",">5.0"),labels=c("≤5.0",">5.0"))
dataClinic2$年龄分组<-factor(dataClinic2$年龄分组,levels=c("≤56.0",">56.0"),labels=c("≤56.0",">56.0"))
dataClinic2$ALb分组<-factor(dataClinic2$ALb分组,levels=c("≤37.8",">37.8"),labels=c("≤37.8",">37.8"))
dataClinic2$ALp分组<-factor(dataClinic2$ALp分组,levels=c("≤106.2",">106.2"),labels=c("≤106.2",">106.2"))
dataClinic2$AFP分组<-factor(dataClinic2$AFP分组,levels=c("≤11.9",">11.9"),labels=c("≤11.9",">11.9"))
dataClinic2$AST分组<-factor(dataClinic2$AST分组,levels=c("≤38.7",">38.7"),labels=c("≤38.7",">38.7"))
dataClinic2$ALT分组<-factor(dataClinic2$ALT分组,levels=c("≤34.7",">34.7"),labels=c("≤34.7",">34.7"))
dataClinic2$TBIL分组<-factor(dataClinic2$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("≤20.5",">20.5"))
dataClinic2$PLT分组<-factor(dataClinic2$PLT分组,levels=c("≤139.0",">139.0"),labels=c("≤139.0",">139.0"))
dataClinic2$WBC分组<-factor(dataClinic2$WBC分组,levels=c("≤5.9",">5.9"),labels=c("≤5.9",">5.9"))

dd<-datadist(dataClinic1)
options(datadist="dd")
cox_model <- cph(Surv(time, status) ~ 肝切除术+T+M+ALb分组+AFP分组+AST分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic1)
surv <- Survival(cox_model)
surv1 <- function(x)surv(12, x)
surv2 <- function(x)surv(36, x)
surv3 <- function(x)surv(60, x)
nomolung = nomogram(cox_model,fun = list(surv1, surv2, surv3), lp=T,fun.at = c(0, seq(0, 1, by = 0.1), 1),
funlabel = c('1-year survial', '3-year survival', '5-year survival'))
plot(nomolung)


#绘制训练集cox回归的ROC曲线图
t<-c(36,60)
survprob<-predictSurvProb(cox_model,newdata=dataClinic1,times=t)
head(survprob)
dataClinic1$survprob3<-survprob[,1]
dataClinic1$survprob5<-survprob[,2]

#绘制训练集决策曲线图
dataClinic1$f_cph_36<-1-dataClinic1$survprob3
dataClinic1$f_cph_60<-1-dataClinic1$survprob5

dca1 <- dcurves::dca(Surv(time,status) ~ f_cph_36 + f_cph_60,
                     data = dataClinic1,
                     time = 36,
                     label = list(f_cph_36 = "f_cph_36",
                                  f_cph_60 = "f_cph_60"))  %>%
  dcurves::as_tibble()
 
ggplot(dca1,aes(x=threshold, y=net_benefit,color=variable))+
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.2) +
  coord_cartesian(ylim = c(0, 0.6)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Risk Threshold (Train Set)", y = "Net Benefit", color = "") +
  theme_bw()

#绘制生存分析的ROC曲线
Mayo3= survivalROC(Stime=dataClinic1$time,##生存时间
                     status=dataClinic1$status,## 终止事件    
                     marker = -dataClinic1$survprob3, ## marker value    
                     predict.time = 36,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo3)
Mayo5= survivalROC(Stime=dataClinic1$time,##生存时间
                     status=dataClinic1$status,## 终止事件    
                     marker = -dataClinic1$survprob5, ## marker value    
                     predict.time = 60,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo5)
## 绘制3年生存率计算的ROC曲线
plot(Mayo3$FP, Mayo3$TP,
     type="l",col="red",
     xlim=c(0,1), ylim=c(0,1),   
     xlab="False positive rate", 
     ylab="True positive rate",
     main="ROC curves of the Cox regression model (Train Set)")
# 添加对角线
abline(0,1,col="gray",lty=2)
# 添加5年生存率计算的ROC曲线
lines(Mayo5$FP, Mayo5$TP, 
      type="l",col="blue",
      xlim=c(0,1), ylim=c(0,1))
# 添加图例
legend("bottomright",legend = c(paste("AUC at 3-years =", round(Mayo3$AUC,3)),
                                paste("AUC at 5-years =", round(Mayo5$AUC,3))),
       col=c("red","blue"),
       lty= 1 ,lwd= 2)

#绘制验证集cox回归的ROC曲线图
t<-c(36,60)
survprob<-predictSurvProb(cox_model,newdata=dataClinic2,times=t)
head(survprob)
dataClinic2$survprob3<-survprob[,1]
dataClinic2$survprob5<-survprob[,2]

#绘制训练集决策曲线图
dataClinic2$f_cph_36<-1-dataClinic2$survprob3
dataClinic2$f_cph_60<-1-dataClinic2$survprob5

dca2 <- dcurves::dca(Surv(time,status) ~ f_cph_36 + f_cph_60,
                     data = dataClinic2,
                     time = 36,
                     label = list(f_cph_36 = "f_cph_36",
                                  f_cph_60 = "f_cph_60"))  %>%
  dcurves::as_tibble()
 
ggplot(dca2,aes(x=threshold, y=net_benefit,color=variable))+
  stat_smooth(method = "loess", se = FALSE, formula = "y ~ x", span = 0.2) +
  coord_cartesian(ylim = c(0, 0.6)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  labs(x = "Risk Threshold (Test Set)", y = "Net Benefit", color = "") +
  theme_bw()

#绘制验证集生存分析的ROC曲线
Mayo3= survivalROC(Stime=dataClinic2$time,##生存时间
                     status=dataClinic2$status,## 终止事件    
                     marker = -dataClinic2$survprob3, ## marker value    
                     predict.time = 36,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo3)
Mayo5= survivalROC(Stime=dataClinic2$time,##生存时间
                     status=dataClinic2$status,## 终止事件    
                     marker = -dataClinic2$survprob5, ## marker value    
                     predict.time = 60,## 预测时间截点
                     method = 'KM')##span,NNE法的namda
str(Mayo5)
## 绘制3年生存率计算的ROC曲线
plot(Mayo3$FP, Mayo3$TP,
     type="l",col="red",
     xlim=c(0,1), ylim=c(0,1),   
     xlab="False positive rate", 
     ylab="True positive rate",
     main="ROC curves of the Cox regression model (Test Set)")
# 添加对角线
abline(0,1,col="gray",lty=2)
# 添加5年生存率计算的ROC曲线
lines(Mayo5$FP, Mayo5$TP, 
      type="l",col="blue",
      xlim=c(0,1), ylim=c(0,1))
# 添加图例
legend("bottomright",legend = c(paste("AUC at 3-years =", round(Mayo3$AUC,3)),
                                paste("AUC at 5-years =", round(Mayo5$AUC,3))),
       col=c("red","blue"),
       lty= 1 ,lwd= 2)


dd1<-datadist(dataClinic1)
options(datadist="dd1")
#绘制训练集（3年）校准曲线
# 将月份转换为天数
dataClinic1$time<-dataClinic1$time/12*365
cox_model3 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic1,time.inc = 365*3)
cal_train3<-calibrate(cox_model3,u=365*3,cmethod='KM',m=100)
#绘制train-3年校准图
plot(cal_train3,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 3-year overall survival (Train Set)',#x轴名称
     ylab='Actual 3-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围
#绘制训练集（5年）校准曲线
cox_model5 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic1,time.inc = 365*5)
cal_train5<-calibrate(cox_model5,u=365*5,cmethod='KM',m=100)
#绘制train-5年校准图
plot(cal_train5,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 5-year overall survival (Train Set)',#x轴名称
     ylab='Actual 5-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围


dd2<-datadist(dataClinic2)
options(datadist="dd2")
#绘制验证集（3年）校准曲线
# 将月份转换为天数
dataClinic2$time<-dataClinic2$time/12*365
cox_model3 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic2,time.inc = 365*3)
cal_test3<-calibrate(cox_model3,u=365*3,cmethod='KM',m=40)
#绘制train-3年校准图
plot(cal_test3,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 3-year overall survival (Test Set)',#x轴名称
     ylab='Actual 3-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围
#绘制验证集（5年）校准曲线
cox_model5 <- cph(Surv(time, status) ~ 婚姻状况+肝切除术+T+M+ALb分组+ALp分组+AST分组+ALT分组+TBIL分组+PLT分组, x = TRUE, y = TRUE, surv = TRUE,data=dataClinic2,time.inc = 365*5)
cal_test5<-calibrate(cox_model5,u=365*5,cmethod='KM',m=40)
#绘制train-5年校准图
plot(cal_test5,lwd=1,lty=2, #线条粗细与虚实
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)),#颜色设置
     xlab='Nomogram-Predicted Probability of 5-year overall survival (Test Set)',#x轴名称
     ylab='Actual 5-year overall survival probability',#y轴名称
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1))#x轴范围与y轴范围




#使用机器学习模型进行分析
rm(list=ls())
library(randomForest)    #进行随机森林模型
library(e1071)     #支持向量机
library(caret)
library(rmda)   #绘制决策曲线
#其他机器学习方法
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/New/train0.75.xlsx")
dataClinic1[,c(2:10,12:18)] <- data.frame(lapply(dataClinic1[,c(2:10,12:18)], as.factor))
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic1$肿瘤直径分组<-factor(dataClinic1$肿瘤直径分组,levels=c("≤5.0",">5.0"),labels=c("≤5.0",">5.0"))
dataClinic1$年龄分组<-factor(dataClinic1$年龄分组,levels=c("≤56.0",">56.0"),labels=c("≤56.0",">56.0"))
dataClinic1$ALb分组<-factor(dataClinic1$ALb分组,levels=c("≤37.8",">37.8"),labels=c("≤37.8",">37.8"))
dataClinic1$ALp分组<-factor(dataClinic1$ALp分组,levels=c("≤106.2",">106.2"),labels=c("≤106.2",">106.2"))
dataClinic1$AFP分组<-factor(dataClinic1$AFP分组,levels=c("≤11.9",">11.9"),labels=c("≤11.9",">11.9"))
dataClinic1$AST分组<-factor(dataClinic1$AST分组,levels=c("≤38.7",">38.7"),labels=c("≤38.7",">38.7"))
dataClinic1$ALT分组<-factor(dataClinic1$ALT分组,levels=c("≤34.7",">34.7"),labels=c("≤34.7",">34.7"))
dataClinic1$TBIL分组<-factor(dataClinic1$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("≤20.5",">20.5"))
dataClinic1$PLT分组<-factor(dataClinic1$PLT分组,levels=c("≤139.0",">139.0"),labels=c("≤139.0",">139.0"))
dataClinic1$WBC分组<-factor(dataClinic1$WBC分组,levels=c("≤5.9",">5.9"),labels=c("≤5.9",">5.9"))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/New/test0.25.xlsx")
dataClinic2[,c(2:10,12:18)] <- data.frame(lapply(dataClinic2[,c(2:10,12:18)], as.factor))
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("已婚","未婚","离婚","丧偶","其他"))
dataClinic2$肿瘤直径分组<-factor(dataClinic2$肿瘤直径分组,levels=c("≤5.0",">5.0"),labels=c("≤5.0",">5.0"))
dataClinic2$年龄分组<-factor(dataClinic2$年龄分组,levels=c("≤56.0",">56.0"),labels=c("≤56.0",">56.0"))
dataClinic2$ALb分组<-factor(dataClinic2$ALb分组,levels=c("≤37.8",">37.8"),labels=c("≤37.8",">37.8"))
dataClinic2$ALp分组<-factor(dataClinic2$ALp分组,levels=c("≤106.2",">106.2"),labels=c("≤106.2",">106.2"))
dataClinic2$AFP分组<-factor(dataClinic2$AFP分组,levels=c("≤11.9",">11.9"),labels=c("≤11.9",">11.9"))
dataClinic2$AST分组<-factor(dataClinic2$AST分组,levels=c("≤38.7",">38.7"),labels=c("≤38.7",">38.7"))
dataClinic2$ALT分组<-factor(dataClinic2$ALT分组,levels=c("≤34.7",">34.7"),labels=c("≤34.7",">34.7"))
dataClinic2$TBIL分组<-factor(dataClinic2$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("≤20.5",">20.5"))
dataClinic2$PLT分组<-factor(dataClinic2$PLT分组,levels=c("≤139.0",">139.0"),labels=c("≤139.0",">139.0"))
dataClinic2$WBC分组<-factor(dataClinic2$WBC分组,levels=c("≤5.9",">5.9"),labels=c("≤5.9",">5.9"))
#RF分析
#临床模型
set.seed(888)
#组学模型
modelTrainRF <- randomForest(status ~ 肝切除术+T+M+ALb分组+AFP分组+AST分组+PLT分组,data=dataClinic1,ntree = 5,importance=TRUE)
summary(modelTrainRF)
#查看模型在训练集的验证
pred<-predict(object =modelTrainRF,newdata=dataClinic1)   #预测分类
probRFTrainOmics <- ifelse(pred>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainRF <- roc(dataClinic1$status, pred,levels=c("1","0"),ci=T,auc=T) 
rocTrainRF
#查看模型在验证集的验证
pred<-predict(object =modelTrainRF,newdata=dataClinic2)   #预测分类
probRFTestOmics <- ifelse(pred>=0.5,1,0)      #预测值
xtab <- table(probRFTestOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestRF <- roc(dataClinic2$status, pred,levels=c("1","0"),ci=T,auc=T) 
rocTestRF
#绘制校准曲线
dataClinic2$pred<-pred
log_model1 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal1 <- calibrate(log_model1, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal1,main="Calibration plot (RF model)")
#绘制决策曲线
simple1<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple1,curve.names=c('RF Model'),cost.benefit.axis =FALSE,col= c('red'),confidence.intervals=FALSE,standardize = FALSE)


#SVM分析
#查看模型在训练集的验证
svm_model <- svm(status ~ 肝切除术+T+M+ALb分组+AFP分组+AST分组+PLT分组,data=dataClinic1)
predicted <- predict(svm_model , dataClinic1)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainSVM <- roc(dataClinic1$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTrainSVM
#查看模型在验证集的验证
predicted <- predict(svm_model , dataClinic2)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestSVM <- roc(dataClinic2$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTestSVM
#绘制校准曲线
dataClinic2$pred<-predicted
log_model2 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal2 <- calibrate(log_model2, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal2,main="Calibration plot (SVM model)")
#绘制决策曲线
simple2<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple2,curve.names=c('SVM Model'),cost.benefit.axis =FALSE,col= c('orange'),confidence.intervals=FALSE,standardize = FALSE)



library(xgboost)
#xgboost分析
dataClinic1<-read.xlsx("C:/Users/txh/Desktop/05data/New/train0.75.xlsx")
dataClinic1$status<-as.numeric(dataClinic1$status)
dataClinic1$婚姻状况<-as.numeric(factor(dataClinic1$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic1$肝切除术<-as.numeric(factor(dataClinic1$肝切除术,levels=c("是","否"),labels=c("1","0")))
dataClinic1$T<-as.numeric(factor(dataClinic1$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic1$M<-as.numeric(factor(dataClinic1$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic1$ALb分组<-as.numeric(factor(dataClinic1$ALb分组,levels=c("≤37.8",">37.8"),labels=c("1","2")))
dataClinic1$ALp分组<-as.numeric(factor(dataClinic1$ALp分组,levels=c("≤106.2",">106.2"),labels=c("1","2")))
dataClinic1$AFP分组<-as.numeric(factor(dataClinic1$AFP分组,levels=c("≤11.9",">11.9"),labels=c("1","2")))
dataClinic1$AST分组<-as.numeric(factor(dataClinic1$AST分组,levels=c("≤38.7",">38.7"),labels=c("1","2")))
dataClinic1$ALT分组<-as.numeric(factor(dataClinic1$ALT分组,levels=c("≤34.7",">34.7"),labels=c("1","2")))
dataClinic1$TBIL分组<-as.numeric(factor(dataClinic1$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("1","2")))
dataClinic1$PLT分组<-as.numeric(factor(dataClinic1$PLT分组,levels=c("≤139.0",">139.0"),labels=c("1","2")))
dataClinic1$WBC分组<-as.numeric(factor(dataClinic1$WBC分组,levels=c("≤5.9",">5.9"),labels=c("1","2")))

dataClinic2<-read.xlsx("C:/Users/txh/Desktop/05data/New/test0.25.xlsx")
dataClinic2$status<-as.numeric(dataClinic2$status)
dataClinic2$婚姻状况<-as.numeric(factor(dataClinic2$婚姻状况,levels=c("已婚","未婚","离婚","丧偶","其他"),labels=c("1","2","3","4","5")))
dataClinic2$肝切除术<-as.numeric(factor(dataClinic2$肝切除术,levels=c("是","否"),labels=c("1","0")))
dataClinic2$T<-as.numeric(factor(dataClinic2$T,levels=c("T1","T2","T3","T4","TX"),labels=c("1","2","3","4","5")))
dataClinic2$M<-as.numeric(factor(dataClinic2$M,levels=c("M0","M1"),labels=c("1","2")))
dataClinic2$ALb分组<-as.numeric(factor(dataClinic2$ALb分组,levels=c("≤37.8",">37.8"),labels=c("1","2")))
dataClinic2$ALp分组<-as.numeric(factor(dataClinic2$ALp分组,levels=c("≤106.2",">106.2"),labels=c("1","2")))
dataClinic2$AFP分组<-as.numeric(factor(dataClinic2$AFP分组,levels=c("≤11.9",">11.9"),labels=c("1","2")))
dataClinic2$AST分组<-as.numeric(factor(dataClinic2$AST分组,levels=c("≤38.7",">38.7"),labels=c("1","2")))
dataClinic2$ALT分组<-as.numeric(factor(dataClinic2$ALT分组,levels=c("≤34.7",">34.7"),labels=c("1","2")))
dataClinic2$TBIL分组<-as.numeric(factor(dataClinic2$TBIL分组,levels=c("≤20.5",">20.5"),labels=c("1","2")))
dataClinic2$PLT分组<-as.numeric(factor(dataClinic2$PLT分组,levels=c("≤139.0",">139.0"),labels=c("1","2")))
dataClinic2$WBC分组<-as.numeric(factor(dataClinic2$WBC分组,levels=c("≤5.9",">5.9"),labels=c("1","2")))
dataTrain<-as.matrix(dataClinic1[,c(4,13,15,34,36,39,41)])
dataTest<-as.matrix(dataClinic2[,c(4,13,15,34,36,39,41)])

dtrain <- xgb.DMatrix(data = dataTrain, label = dataClinic1$status)
model <- xgboost(data = dtrain, max.depth = 2, eta = 1, nrounds = 2,nthread = 2,objective = "binary:logistic")
##查看模型在训练集的验证
predicted <- predict(model, dataTrain)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainXGB <- roc(dataClinic1$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTrainXGB
#查看模型在验证集的验证
predicted <- predict(model , dataTest)
probRFTrainOmics <- ifelse(predicted>=0.5,1,0)      #预测值
xtab <- table(probRFTrainOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestXGB <- roc(dataClinic2$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTestXGB
#绘制校准曲线
dataClinic2$pred<-predicted
log_model3 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal3 <- calibrate(log_model3, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal3,main="Calibration plot (XGB model)")
#绘制决策曲线
simple3<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple3,curve.names=c('XGB Model'),cost.benefit.axis =FALSE,col= c('green'),confidence.intervals=FALSE,standardize = FALSE)


dataTrain<-as.matrix(dataClinic1[,c(4,13,15,22,34,36,39,41)])
dataTest<-as.matrix(dataClinic2[,c(4,13,15,22,34,36,39,41)])
#多层感知机（MLP）分析
library(nnet)
model <- nnet(status ~ 肝切除术+T+M+ALb分组+AFP分组+AST分组+PLT分组,data=dataTrain,size=5,maxit=200)
##查看模型在训练集的验证
predicted <- predict(model, dataTrain)
probRFTrainOmics <- as.numeric(ifelse(predicted>=0.5,1,0))      #预测值
xtab <- table(probRFTrainOmics, dataClinic1$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTrainMLP <- roc(dataClinic1$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTrainMLP
#查看模型在验证集的验证
predicted <- predict(model , dataTest)
probRFTrainOmics <- as.numeric(ifelse(predicted>=0.5,1,0))      #预测值
xtab <- table(probRFTrainOmics, dataClinic2$status)   #两个变量必须为分类变量
confusionMatrix(xtab,mode = "everything",positive="1")
rocTestMLP <- roc(dataClinic2$status, predicted,levels=c("1","0"),ci=T,auc=T) 
rocTestMLP
#绘制校准曲线
dataClinic2$pred<-predicted
log_model4 <- lrm(status ~ pred, data = dataClinic2,x=TRUE,y=TRUE)  
cal4 <- calibrate(log_model4, method = "boot", B = 100)  # 使用bootstrap进行校准  
plot(cal4,main="Calibration plot (MLP model)")
#绘制决策曲线
simple4<- decision_curve(status ~ pred, data = dataClinic2)  
plot_decision_curve(simple4,curve.names=c('MLP Model'),cost.benefit.axis =FALSE,col= c('blue'),confidence.intervals=FALSE,standardize = FALSE)


#将多个图绘制在同一个图片上
par(mfrow=c(2,2))        #构建2*2画布
plot(cal1,main="Calibration plot (RF model)")
plot(cal2,main="Calibration plot (SVM model)")
plot(cal3,main="Calibration plot (XGB model)")
plot(cal4,main="Calibration plot (MLP model)")

plot_decision_curve(simple1,curve.names=c('RF Model'),cost.benefit.axis =FALSE,col= c('red'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (RF model)")
plot_decision_curve(simple2,curve.names=c('SVM Model'),cost.benefit.axis =FALSE,col= c('orange'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (SVM model)")
plot_decision_curve(simple3,curve.names=c('XGB Model'),cost.benefit.axis =FALSE,col= c('green'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (XGB model)")
plot_decision_curve(simple4,curve.names=c('MLP Model'),cost.benefit.axis =FALSE,col= c('blue'),confidence.intervals=FALSE,standardize = FALSE,main="Decision curve (MLP model)")

dev.off()         #关闭画布




#roc曲线绘制
###组学模型
#绘制训练集中不同模型的ROC曲线图
plot(rocTrainRF, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.4, # 设置AUC值坐标为（x，y）
     main="Train ROC curve",  # 添加图形标题
     col="red",  
     lty=1,
     legacy.axes=TRUE,
     print.thres=T)   
# 再添加1条ROC曲线
plot(rocTrainSVM, 
     print.auc=TRUE, 
     print.auc.x=0.5, print.auc.y=0.35, 
     add=TRUE,
     col="orange",   
     lty=1) 
plot(rocTrainXGB, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.3, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="green",    # 设置ROC曲线颜色
     lty=1) 
plot(rocTrainMLP, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.25, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="blue",    # 设置ROC曲线颜色
     lty=1) 
# 添加图例
legend("bottomright",  # 图例位置x，y
       bty = "n",   # 图例样式
       legend=c("RF model","SVM model","XGB model","MLP model"),  # 添加分组
       col=c("red","orange","green","blue"),  # 颜色跟前面一致
       lwd=1,
       lty=1)  # 线条粗细   输出700*580

#绘制验证集中不同模型的ROC曲线图
plot(rocTestRF, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.4, # 设置AUC值坐标为（x，y）
     main="Validation ROC curve",  # 添加图形标题
     col="red",  
     lty=1,
     legacy.axes=TRUE)   
# 再添加1条ROC曲线
plot(rocTestSVM, 
     print.auc=TRUE, 
     print.auc.x=0.5, print.auc.y=0.35, 
     add=TRUE,
     col="orange",   
     lty=1) 
plot(rocTestXGB, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.3, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="green",    # 设置ROC曲线颜色
     lty=1) 
plot(rocTestMLP, 
     print.auc=TRUE, # 图像上输出AUC的值
     print.auc.x=0.5, print.auc.y=0.25, # 设置AUC值坐标为（x，y）
     add=TRUE,
     col="blue",    # 设置ROC曲线颜色
     lty=1) 
# 添加图例
legend("bottomright",  # 图例位置x，y
       bty = "n",   # 图例样式
       legend=c("RF model","SVM model","XGB model","MLP model"),  # 添加分组
       col=c("red","orange","green","blue"),  # 颜色跟前面一致
       lwd=1,
       lty=1)  # 线条粗细   输出700*580
