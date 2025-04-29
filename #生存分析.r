#生存分析



rm(list=ls())
library(glmnet)
library(rms)
library(openxlsx) 
library(Hmisc)
library(nortest)
library(car)
library(dplyr)

data<-read.xlsx("C:/Users/txh/Desktop/1.xlsx")
data<-as.data.frame(data)
print(paste('总样本数:',nrow(data),ncol(data)))
# 缺失数据的识别
sum(is.na(data))  # 输出缺失值个数
colSums(is.na(data))      #查看每列缺失值数量
str(data)
data$分组<- factor(data$分组,levels=c("0","1"),labels=c("缓解稳定组","进展组"))


# 定义需要分析的变量

variables2 <- c('年龄','BMI','腰围','收缩压','舒张压','糖尿病病程','空腹血糖','糖化血红蛋白','甘油三脂',
'总胆固醇','高密度脂蛋白','低密度脂蛋白','尿酸','尿素氮','血肌酐','纤维蛋白原')



tapply(data$年龄,data$分组,mean)
tapply(data$年龄,data$分组,sd)
#正态性检验  n>50
tapply(data$年龄,data$分组,lillie.test)    
 #方差齐性检验
leveneTest(data$年龄,data$分组,center=mean)  
#方差分析
summary(aov(data$年龄~data$分组))
#若方差不齐/非正态分布
kruskal.test(data$年龄~data$分组)


# 初始化结果列表
results2 <- list()

# 循环分析每个变量
for (var in variables2) {
  # 提取当前变量
  current_var <- data[[var]]
  
    result <- data %>%
  group_by(data$分组) %>%  # 按 group 分组
  summarise(
    median = median(current_var),  # 计算中位数
    Q1 = quantile(current_var, 0.25),  # 计算第 1 四分位数（Q1）
    Q3 = quantile(current_var, 0.75)   # 计算第 3 四分位数（Q3）
  )
  
  # 存储结果
  results2[[current_var]] <- result
}

# 查看结果
results2




quantiles1 <- quantile(data$年龄~data$分组, probs = c(0.25, 0.75))
quantiles2 <- quantile(group2, probs = c(0.25, 0.75))


# Mann-Whitney U 检验
test_result <- wilcox.test(group1, group2)

# 输出检验结果
print(test_result)