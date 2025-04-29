#切记：在导入数据进R软件后需要因子化及数值化

#打印临床特征名
#打印表中列名   names(trainClinic)
#打印表格类型与行列名    attributes(trainClinic)
#打印表格中列名及其类型、值    str(trainClinic)
#打印表格的维度（行列数）   dim(trainClinic)
#打印单个变量的类型    class(trainClicnic$MRSoutcome)
#查看数据的细分类（integer、double、single、float）     typeof(trainClicnic$年龄)
#查看数据大类(character、numeric、logical)    mode(trainClicnic$年龄)
#查看某一变量的列名：  which(names(mdata_train)=="id")



#单样本t检验
library(haven)
datasav<-read_sav("E:/R/R.practice/data01.sav")
datasav$体重<-as.numeric(datasav$体重)
datasav$性别<-as.factor(datasav$性别)
t.test(datasav$height,mu=160,data=datasav)


shapiro.test(data01$体重)    #n<50
ks.test(data01$体重,y="pnorm")    #n>50

#t检验
library(haven)
datasav<-read_sav("E:/R/R.practice/data01.sav")

#正态性检验
tapply(data$身高,data$性别,shapiro.test)      #n<50
library(nortest)
tapply(data$身高,data$性别,lillie.test)    #优化的ks.test   首选
with(datasav, ks.test(weight[group == "Man"],y="pnorm"))
with(datasav, ks.test(weight[group == "Women"],y="pnorm"))    #n>50(ks.test)

#方差齐性检验
#1.levene检验 (car package)    假定数据符合正态分布
library(car)
leveneTest(data01$身高,data01$性别,center=mean)
leveneTest(data01$身高,data01$性别,center=median)     #默认以中位值进行检测 
#2.Bartlett χ2检验  (stats)       假定数据不符合正态分布
bartlett.test(data01$身高~data01$性别)
#3.F检验(stats)
var.test(weight ~ group, data = datasav)

#离群值检验    （car包）
outlinerTest(fit)     

#独立样本t检验  
t.test(weight ~ group, data = my_data, var.equal = TRUE/FALSE,alternative="two.sides")
#配对样本t检验   单侧检验中  alternative="greater"  (前者大于后者)    alternative="less"（前者小于后者）
t.test(weight ~ group, data = my_data, var.equal = TRUE/FALSE,alternative="two.sides"，paired=TRUE)

#方差分析
#1.单因素设计方差分析

#查看各组的均数与标准差
tapply(data$成绩,data$教学法,mean)
tapply(data$成绩,data$教学法,sd)
tapply(ds$成绩~ds$教学法,var)
tapply(ds$成绩~ds$教学法,range)
#正态性检验
tapply(data$成绩,data$教学法,shapiro.test)
boxplot(data$成绩,data$教学法)
#方差齐性检验
library(car)
leveneTest(datasav$成绩,datasav$分组,center=mean)
#方差分析    方差齐
fit<-aov(datasav$成绩~datasav$分组)
summary(fit)
#方差分析    方差不齐（对应了spss中的Welch（韦尔奇））
fit<-oneway.test(datasav$学生成绩~datasav$分组, var.equal = F)  
summary(fit)
#各组间比较（多重比较）
#1.LSD
library(agricolae)
out <- LSD.test(fit,"datasavLSD$分组", p.adj="none")
summary(out)
out$group
#2.Bonferroni法（Bonferroni校正在LSD法上的应用。将LSD.test中p.adj设置为"bonferroni"）
library(agricolae)
out <- LSD.test(fit,"datasav$分组", p.adj="bonferroni")
summary(out)
out$group
#3.Dunnett检验  （用于多个试验组与一个对照组间的比较）
##如果要分别比较每个试验组与对照组,建议采用Dunnett法
library(multcomp)
out <- glht(fit,linfct = mcp(group = 'Dunnett'), alternative = 'two.side')
summary(out)
#4.SNK法（Student-Newman-Keuls）
library(agricolae)
out <- SNK.test(fit,"group")
out$group
##5.Turkey检验  （stats）（特点：所有各组的样本数相等；各组样本均数之间的全面比较）
tuk=TukeyHSD(fit)
tuk
plot(tuk)
#6.Duncan法(新复极差法)（SSR）
library(agricolae)
duncan.test(y, trt, …)
#7.Scheffe检验      ##如果例数不等,建议首选Scheffe法
library(agricolae)
scheffe.test(y, trt, …)

#随机区组设计方差分析   （无需分析交互作用，无法计算方差齐性检验）
fit<-aov(ds$体重~ds$区组+ds$干预组)
summary(fit)
#对干预组因素进行多重比较
#1.LSD
out <- LSD.test(fit,"ds$区组", p.adj="none")
out$groups
out <- LSD.test(fit,"ds$干预组", p.adj="none")
out$groups
#SNK
out <- SNK.test(fit,"ds$干预组")
out$group
out <- SNK.test(fit,"ds$区组")
out$group
#2.Duncan法(新复极差法)（SSR）
duncan.test(fit,"ds$干预组",alpha=0.05,console=T) 
duncan.test(fit,"ds$区组",alpha=0.05,console=T) 
#生成残差
res <- residuals(fit) 
#评估残差正态性 
shapiro.test(res) 

#析因设计方差分析  （往往用于研究因素或者水平不多的情况，当研究因素较多时，因采取正交设计或者均匀设计 ）
#如果有k≥2个试验因素（如A和B），每种因素又有m大于等于2个水平（如条件1，条件2），若k个试验因素的m个水平同时施加于受试对象，则这样的试验方案称为k*m的析因设计。
#主效应：指某一因素单独效应的平均值
#交互效用是指两因素或多因素间效应互不独立的情况下，当某一因素在各水平间变化时，另一（多）个因素各水平的效应也相应地发生变化
ds<-read_sav("E:/R/R.practice/data10.3.sav")
ds$A<-as.factor(ds$A)
ds$B<-as.factor(ds$B)
ds$RBC<-as.numeric(ds$RBC)
fit<-aov(ds$RBC~ds$A*ds$B)       #交互项A:B的P值大于0.05，则代表A，B无交互作用
summary(fit)
#若A:B没有交互项，需要重新拟合模型   主效应分析
fit<-aov(ds$RBC~ds$A+ds$B)
summary(fit)
#多重比较
library(agricolae)
out <- LSD.test(fit,"ds$A", p.adj="none")
summary(out)
out$group
#对A因素的多重比较
TukeyHSD(fit,"ds$A")
#对B因素的多重比较
TukeyHSD(fit,"ds$B")
#交互作用的轮廓图  (正向交互：A的效应随B的增大而增大；反向交互：A的效应随B的增大而减小；)
interaction.plot(ds$A,ds$B,ds$RBC,type=c("l","l"),col = c("red","blue","green"))

#若A:B没有交互项，需要重新拟合模型   单独效应分析     方法暂缺

#重复测量设计方差分析   (使用R很难做)
#正态与方差齐（各处理水平）与各时间点组成的协方差阵具有球形性特征
#1.单因素重复测量方差分析       没有组间干预措施的影响，只有主体内（时间）的影响
library(readxl)
ds<-read_xlsx("C:/Users/txh/Desktop/1.xlsx",sheet = 1)
boxplot(ds$value~ds$time,boxwex = 0.25,col=c('lightblue'),xlab='time(周)',ylab='weight',main='箱线图')
#正态性检验
tapply(ds$value,ds$time,shapiro.test)
library(car)
leveneTest(datasav$成绩,datasav$分组,center=mean)
#协方差阵球形检验  数据要求为矩阵，且变量为time1，time2...
library(haven)
ds<-read_sav("E:/R/R.practice/data10.4.sav")    #数据为平铺型
dsmat<-as.matrix(ds)      #将数据矩阵化
dsmatfit<-lm(dsmat~1)      ##进行多元线性回归 (多个因变量)
summary(dsmatfit)
estVar(dsmatfit)
mauchly.test(dsmatfit,X= ~1)       #单因素球形检验（莫奇来球形度检验）   需要数据为平铺的矩阵

df1<-melt(data=ds,id.vars="ID",variable.name = "time",value.name = "xuetang")    #数据倒置
fit <- aov(xuetangt~time+Error(ID/time),data=df1)
summary(fit)

#存在着许多问题待解决
#2.两因素重复测量方差分析       有组间干预措施的影响，也有主体内（时间）的影响
#多因素球形检验（莫奇来球形度检验）  其中 c(2, 2)中的2*2要与原矩阵中的列数相同
ds<-read_sav("E:/R/R.practice/data10.4.sav")    #数据为平铺型
dsmat<-as.matrix(ds)      #将数据矩阵化
dsmatfit<-lm(dsmat~1)
group=factor(rep(c("A","B"), c(2, 2)))
time=ordered(rep(1:2,2))
idata=data.frame(group,time)
mauchly.test(dfmatfit,M=~group+time,data=idata)
fit <- aov(weight~time*type+Error(subID/time),data=dt)
summary(fit)

 
#协方差分析     协变量为连续性变量
#条件：各组资料来自正态分布总体，且各组方差相等；
    #2.满足各组的总体回归系数β相等且不等于0，即平行性检验（也是自变量与协变量间无交互作用）
#交互作用的进一步分析是简单效应，主效应的进一步分析是事后比较
fit<-aov(df$治疗后血压~df$组别*df$治疗前血压)
summary(fit)
#看df$组别*df$治疗前血压de 交互性p值是否大于0.05（大于0.05则无交互）
#p大于0.05   则剔除交互性在进行分析
fit<-aov(df$治疗后血压~df$组别+df$治疗前血压)
summary(fit)
#p小于0.05   则存在交互性，应该进行两因素方差分析

#卡方检验    行标多用于不同方法；   列标多为结局变量   2*2，  RxC  当C为不相关指标
#1.对于直接出现频数的四格表
table<-matrix(c(43,13,48,3),nrow=2,ncol=2)
#2.对于出现变量而非频数的四格表
table<-(var1,var2...,data=)
prop.table(table)                  #生成总百分比
prop.table(table,margin = 1)       #生成行百分比
prop.table(table,margin = 2)       #生成列百分比
margin.table(table)               #生成总频数
margin.table(table,margin = 1)      #生成行总数
margin.table(table,margin = 2)      #生成列总数
addmargins(table)              #总求和结果添加到表中
addmargins(table,margins=1)    #行求和结果添加到表中
addmargins(table,margins=2)    #列求和结果添加到表中
ftable(table)                #创建一个紧凑的平铺列联表
#pearson  卡方检验
chisq.test(table,correct=FALSE)
chisq.test(table,correct=FALSE)$expected
#连续性矫正     N大于40，期望值出现小于5大于1时；   chisq.test 默认correct=TRUE
chisq.test(table)
#fisher test    当总数N小于40时，或期望值<1时
fisher.test(table)
chisq.test(table)$expected   #生成理论频数

#RxC列联表
table<-(var1,var2...,data=)
chisq.test(table,correct=FALSE)
chisq.test(table)$expected
#两两比较
chisq.test(table[1:2,], correct=F)
chisq.test(table[2:3,], correct=F)
chisq.test(table[c(1,3),], correct=F)

#RxC  当C是递进指标   需要使用Ridit分析（Ridit包） 或者非参数检验
ridit(table,margin=)       margin=n   n是递进指标所在位置（行/列）对于（1/2）

#线性趋势卡方     Cochran-Armitage检验
CAT <- matrix(c(13, 136, 17, 125, 16, 104, 32, 149, 9, 45), nrow = 5, byrow = T)
dimnames(CAT) <- list("Dose" = c("50", "100", "200", "300", "500"),
                       "Dizziness" = c("Yes", "No"))
CAT
library(DescTools)
CochranArmitageTest(CAT)

#配对卡方
table<-matrix(c(43,13,48,3),nrow=2,byrow=TRUE)
dimnames(table) <- list("ct" = c("阳性", "阴性"),
                       "病理" = c("阳性", "阴性"))
table
mcnemar.test(table)

#kappa  一致性检验   一般认为0.4<kappa<0.6时表明一般，0.6<kappa<0.8时，表明有较高，kappa>0.8表明有很好，kappa<0.4较差。
kappa2(table)
kappam.fleiss(table)     #irr package

#分层卡方
mantelhaen.test(x, y = NULL, z = NULL,alternative = c("two.sided", "less", "greater"),
correct = TRUE, exact = FALSE, conf.level = 0.95)

#卡方分割   暂无



#非参数检验
wilcox.test(y~A,data=)     #Mann-White U检验   两组独立样本    --符号秩检验
kruskal.test(y~A,data=)    #Kruskal-Wallis检验   多组独立样本
#多组间的相互比较
pairwise.wilcox.test(x, g, p.adjust.method = "holm"/"bonferroni",paired = FALSE, ...)

binom.test(sum(error$B1<error$Q1), length(error$B1))   #符号检验：

#单样本k-s检验    用于单样本正态/指数/泊松/二项等等分布检验
ks.test(x,y="pnorm"/"pexp"/"poisson"/"pgamma"/"binomial",alternative = "two.sided")

#单样本t/wilcox检验
shapiro.test(df$weight)
t.test(df$weight,mu = 55)
wilcox.test(df$weight,mu=55)

#两独立样本t/wilcox检验
t.test(df$weight~df$group,var.equal = T )
wilcox.test(df$weight~df$group,exact = F )

#两配对样本t/wilcox检验
t.test(df$weight~df$group,var.equal = T, paired=T )
wilcox.test(df$weight~df$group,exact = F,paired=T )
#Friedman       多组相关样本   其中数据需要是矩阵形式，使用as.matrix进行转换
#数据应转换为数值变量一个，id变量一个，分组变量一个   （横变竖）
mydata1<-melt(data=df,id.vars = "id",variable.name = "times",value.name = "xuetang")
friedman.test(xuetang~times|id,data=mydata1)
friedman.test(y~A|B,data=)     #B是区块（id）变量       A是分组变量

#多元方差分析
y <- cbind(weight,height,score,data=)
aggregate(y, by = list(group), FUN = mean,data=)
aggregate(y, by = list(group), FUN = sd,data=)
fit <- manova(y ~ group,data=)
summary.aov(fit)
#检验多元正态性
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y, center, cov)
coord <- qqplot(qchisq(ppoints(n), df = p), d, 
main = "QQ Plot Assessing Multivariate Normality", ylab = "Mahalanobis D2")
#等同于   coord <- qqplot(qchisq(ppoints(nrow(y)), df = ncol(y)), mahalanobis(y, colMeans(y), cov(y)))
abline(a = 0, b = 1) 
identify(coord$x, coord$y, labels = row.names(UScereal))
#方差—协方差矩阵同质性    使用Box’s M检验来评估  
#Box's M 检验验证方差-协方差矩阵同质性（p 值大于 0.05 即说明各组的协方差矩阵相同）
library(biotools)
boxM(y, group,data=)
#检测多元离群点
library(mvoutlier)
outliers <- aq.plot(y)
outliers


#高级找关系
#关联与相关
#pearson相关   适用于两组符合正态分布的数据
#spearman      适用于至少有一组数据不符合正态分布，或者至少有一个变量是等级变量
#Kendall's Tau相关系数   适用于两个有序分类变量
#正态性检验
shapiro.test(df$weight)
shapiro.test(df$height)
ks.test(df$weight,y="pnorm")
#散点图
scatter(df$weight,df$height)
#相关性分析
#相关系数0.8～1.0高度相关；0.6～0.8强相关；0.4～0.6中等程度相关；0.2～0.4弱相关；0.0～0.2极弱相关或无相关
cor.test(df$体重,df$身高,method="pearson"/"kendall"/"spearman",use="complete.obs")

#偏相关
#当要进行相关性分析的两个变量其取值受到其他变量影响时，可以利用偏相关分析对其他变量进行控制，
#在控制其他变量基础之上进行这两个变量之间相关性分析

#提取需分析的数据变量并删除缺失值
df_na<-df[,c(2,3,9)]
df_na<-na.omit(df_na)
#c(1,2,3)   前两变量是要计算的相关系数的变量数字标识，后面的均为控制变量的数字标识  cov：变量的协方差
R<-pcor(c(1,2,3),cov(dfna，method="pearson"/"kendall"/"spearman",use="everything"/"complete.obs"))
#偏相关分析   pcor.test(r,q,n)      r是上式中偏相关系数R，q是控制变量的数字标识，n是数据的样本量  
pcor.test(R,4,N) 

#典型相关    计算一组变量与另一组变量之间的相关性分析方法

#为了消除数量级的影响 将数据标准化处理 调用scale函数   可有可无
df<-read_sav("E:/R/R.practice/15.3.sav")
df<-scale(df)
#对数据做典型相关分析
ca<-cancor(df[,1:3],df[,4:7])              #df[,:],df[,:]分别对象X组变量，与Y组变量
#查看分析结果
ca
#计算数据在典型变量下的得分 U=AX  V=BY
U<-as.matrix(df[, 1:3])%*% ca$xcoef
V<-as.matrix(df[, 4:7])%*% ca$ycoef
#画出U1、V1和U3、V3为组表的数据散点图
plot(U[,1], V[,1], xlab="U1", ylab="V1")
plot(U[,3], V[,3], xlab="U3", ylab="V3"


#线性回归    线性（散点图）/独立性（德宾-沃森）/正态性（残差散点图）/方差齐性（残差散点图）   强影响点诊断（个案诊断）
#简单回归中，R2>0.5较好，>0.8就非常好；多重回归>0.3尚可，>0.5良好，>0.6非常好；
fit <- lm(weight ~ height, data=df)
summary(fit)
anova(fit)
#散点图与相关系数
scatter.smooth(df$身高,df$体重)
ggplot(df, aes(x = BLDG, y = SALES)) + geom_point() + geom_smooth(method = 'lm') 
ggplot(glass, aes(x = BLDG, y = SALES)) + geom_point()+ 
geom_abline(intercept = coef(glass.lm1)[1], slope = coef(glass.lm1)[2])
cor(df$身高,df$体重,use="complete.obs")
#残差符合正态分布 (normality)
qqnorm(fit$residuals)
qqline(fit$residuals)
shapiro.test(fit$residuals)
#残差满足同方差性 (homoscedasticity)
par(mfrow=c(1,2))
plot(fit$fitted.values, fit$residuals)
zres <- rstandard(fit)
plot(fit$fitted.values, zres) 
# 残差满足独立性 (independence)     Durbin-Watson检验(1.8-2.2时，数据相互独立)
library(car)
durbinWatsonTest(fit)   
#离群值检验
library(car)
outlierTest(fit)
#共线性诊断
library(car)
vif(fit)    #当VIF>10时，自变量x与其余变量之间有严重的多重共线性
#当存在共线性时，处理方法如下：
#（1）逐步法回归，但共线性太大时，逐步法也无法全部解决；（2）岭回归：较少用；（3）主成分回归




###尚未进行数据验证
#Logistic回归
#普通二分类 logistic 回归 用系统的 glm
library("haven")
df<-read_sav("E:/R/R.practice/data17.1.sav")
#先进行单因素分析，选取有意义的自变量在进行多因素分析，
#同时防止一些有意义自变量被单因素分析时剔除，单因素分析将检验水准设置为P<0.1
fit1<-glm(y~x1,family = binomial(link="logit"),data=df)
fit2<-glm(y~x2,family = binomial(link="logit"),data=df)     #....
#自变量中若出现无序分类变量，需要设置哑变量(虚拟变量)    x4必须为因子形式
dummyV1 <- model.matrix(~x4, data=df)    #x4是无序分类变量，且以数值1作为参照
df<-cbind(dummyV1, df)
#多因素分析
fit<-glm(y~x1+x2+x3+dummyV1,family = binomial(link="logit"),data=df)
summary(df)
#方差分析  来测试每一个系数的显着性
anova(model, test="Chisq")
step(fit)       #逐步回归
#回归系数与OR值
B<-fit$coefficients
OR<-exp(B)
#评估模型的拟合优度   伪R方
library(pscl)  
pR2(fit)
#预测值
pre<-predict(fit,type="response")
data=data.frame(prob=pre,obs=df$y)
data=data[order(data$prob),]
#绘制列线图
nom1<-nomogram(fit1,fun=function(x)1/(1+exp(-x)),lp=F,
fun.at = c(0.1,0.3,0.5,0.7,0.9),funlabel = "Risk")
plot(nom1)
#建立校准曲线图
cal1<-calibrate(fit1,method = "boot",B=1000)
plot(cal1,xlim=c(0,1.0),ylim=c(0,1.0),
     xlab = "Nomogram Predicted Survival", ylab = "Actual Survival")ROC
#ROC曲线（包括模型的最佳预测值）
library(pROC)
roc1=roc(df$y,pre,levels=c("",""))
roc1
plot(roc1,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),
max.auc.polygon=TRUE,auc.polygon.col="blue",print.thres=TRUE)

#有序分类因变量：用 MASS 包里的 polrb
library("haven")
df<-read_sav("E:/R/R.practice/data17.2.sav")
library(MASS)
#1.相关性分析
chart.Correlation(data,histogram= TRUE,pch=19)    #PerformanceAnalytics package
#2.第二步：进行有序分类Logistic回归模型拟合，检验模型是否收敛，
#同时得到有序分类logistic回归模型中截距和回归系数的最大似然估计值
#注意  因变量必须设定是分类变量；data是dataframe
data$y1<-factor(data$y1)
data<-data.frame(data)
fit01<-polr(y1~x1+x2+x3+x4+x5+x6,data=data)
summary(fit01)
#计算OR，CI，P值
OR<-round(exp(fit$coefficients),2)
CI <- round(exp(confint(fit)), 2)
P <- (pnorm(abs( coef(summary(fit))[,"t value"]),lower.tail = FALSE)*2)
out<- as.data.frame(cbind(OR, CI, P))
out
#3.第三步：对有序分类Logistic回归模型中的各个自变量回归系数进行显著性检验
drop1(fit01,test="Chi")  
#平行线检验
brant(fit)            #brant包      p值大于0.1即满足平行性检验
#4.第四步，检验有序Logistic回归模型进行检验，是否存在统计学意义
fit01a<-polr(factor(y1)~1,data=data)
fit01b<-polr(factor(y1)~ x1+x2+x3+x4+x5+x6,data=data)
anova(fit01a,fit01b)
#评估拟合模型的拟合优度   伪R方
library(pscl)  
pR2(fit)
#5.第五步，预测有序Logistic回归模型拟合得到的值，然后将预测值与观察值进行对比，检验模型的预测效果
pred.data<-predict(fit)
n<-table(data01$y1,pred.data);n
Percantage<-c(n[1,1]/sum(n[1,]),n[2,2]/sum(n[2,]),n[3,3]/sum(n[3,]))
rbind(n,Percantage)

#无序分类因变量：用 nnet 包里的 multinom     资料暂缺
library("haven")
df<-read_sav("E:/R/R.practice/data17.3.sav")
library(nnet)
result <- multinom(Y ~ X+age, data = tdata)
#计算P值    无法得出相应的结果
z <- summary(result)$coefficients/summary(result)$standard.errors
p <- (1-pnorm(abs(z),0,1))*2
exp(coef(test))     #OR值
#检测我们变量中某个变量的改变对预测结果的影响
head(pp <- fitted(test))
dses <- data.frame(ses = c("low", "middle", "high"),
  write = mean(ml$write))
predict(test, newdata = dses, "probs")

out <- data.frame(cbind(names(tdata)[q], t(summary(result)$coefficients[,2]), t(p[,2])))
timo <- rbind(timo,out)

#条件logistic回归，用 survival 包里的 clogit
library(survival)
## Conditional logistic regression
res.clogit3.int <- clogit(mi ~ smk + sbp + ecg + smk:sbp + smk:ecg + strata(match), midat)
res.clogit3 <- clogit(mi ~ smk + sbp + ecg + strata(match), midat)
summary(res.clogit3)
## No interaction (smaller) model is adequate
anova(res.clogit3.int, res.clogit3)
## 非条件性logistic回归, 忽略分层变量(not appropriate)
res.logit3b <- glm(mi ~ smk + sbp + ecg, midat, family = binomial(link = "logit"))
glm.ratio(res.logit3b)

resp <- levels(logan$occupation)
n <- nrow(logan)
indx <- rep(1:n, length(resp))
logan2 <- data.frame(logan[indx,],id = indx,tocc = factor(rep(resp, each=n)))
logan2$case <- (logan2$occupation == logan2$tocc)
logan2 <- logan2[order(logan2$id),]
## Show dataset for first three strata
logan2[logan2$id %in% c(1,2,3), ]
res.clogit <- clogit(case ~ tocc + tocc:education + strata(id), logan2)
summ.clogit <- summary(res.clogit)
summ.clogit
exp(coef(res.logit1)[c(1:5,843:846)]) #得到OR值
 
##非条件性logistic回归 (不推荐)，矫正id
res.logit1 <- glm(case ~ tocc + tocc:education + factor(id), logan2, family = binomial(link = "logit"))
summary(res.logit1)$coef[c(1:5,843:846),]
## 非条件性logistic回归 ，忽略分层（不适合）
res.logit1b <- glm(case ~ tocc + tocc:education, logan2, family = binomial(link = "logit"))
summary(res.logit1b)$coef

#生存分析   survival和survminer是最基本的两个包，survival负责分析，survimner负责可视化
library(survival)
library(survimner)
#------------
df<-read_sav("E:/R/R.practice/data18.2.sav")
df<-as.data.frame(df)
df$生存时间<-as.numeric(df$生存时间)
df$生存状态<-as.numeric(df$生存状态)
df$组别<-as.numeric(df$组别)
#总体的的生存曲线
km_fit <- survfit(Surv(df$生存时间, df$生存状态) ~ 1, data=df)   #df必须是数据库形式
ggsurvplot(km_fit)
#按不同分组的生存曲线
km_fit1 <- survfit(Surv(df$生存时间, df$生存状态) ~ df$组别, data=df)
ggsurvplot(km_fit1，data=df,conf.int = TRUE,surv.median.line = "hv",fun = "cumhaz")
#采用log-rank 检验生存率差异
survdiff(Surv(time,status) ~ sex)

#Cox 比例风险模型
library("haven")
df<-read_sav("E:/R/R.practice/18.3.sav")
df<as.data.frame(df)
df$肺癌类型<-factor(df$肺癌类型)
dummyV1 <- model.matrix(~肺癌类型,data=df)   
df<-cbind(dummyV1, df)
cox <- coxph(Surv(df$生存时间,df$生存状态) ~ dummyV1 + df$健康指数 + df$确诊时间 + df$年龄 + df$性别 , data = df)
summary(cox)
cox_fit <- survfit(cox,data=df)    #df必须是数据库形式
library(survimner)
ggsurvplot(cox_fit)


#随机森林模型   （暂未涉及）
# ranger model
r_fit <- ranger(Surv(time, status) ~ trt + celltype +  karno + diagtime + age + prior,
data = vet,mtry = 4,importance = "permutation",splitrule = "extratrees",verbose = TRUE)
# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)
# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], type = "l", ylim = c(0,1),
col = "red",xlab = "Days",ylab = "survival", main = "Patient Survival Curves")
cols <- colors()
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))
#为变量重要性排序
vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)   #ROC曲线值

#最后上述三种生存曲线图绘制在同一图中
# Set up for ggplot
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")
coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model"
rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")
plot_df <- rbind(km_df,cox_df,rf_df)
p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()



#聚类分析
#1.K-均值聚类（K-Means）（快速聚类）
# kmeans对iris进行聚类分析 
df<-read_xlsx("C:/Users/txh/Desktop/1.xlsx",sheet = 1)
df<-data.frame(df)
dfk<-kmeans(df[,2:4],3,iter.max = 10)        #kmeans中的第一变量中含有的自变量必须是num型
dfk
dfk$cluster
table(df$地区,dfk$cluster)    #聚类成员图
#方差分析


#系统聚类（层次聚类）
df<-read_sav("E:/R/R.practice/19.1.sav")
d=dist(scale(df)，method="")   #scale对数据做中心化或者标准化处理
hc1<-hclust(d)   #最长距离法
hc2<-hclust(d,"average")  #组间联接
hc3<-hclust(d,"centroid") #重心法
hc4<-hclust(d,"ward.D")  #ward法
opar<-par(mfrow=c(2,1), mar=c(5.2,4,1,0)) 
plclust(hc1,hang=-1)   #hang是表明谱系图中各类所在的位置;当hang取负值时，谱系图中的类从底部画起 生成谱系图
#rect.hclust  指定类数进行分类
re1<-rect.hclust(hc1, k=5, border="red")   #将分类结果分成5类 用红色矩形笔迹标记
plclust(hc2,hang=-1) 
re1<-rect.hclust(hc1, k=5, border="red")   #将分类结果分成5类   得出谱系图
re1                                        #得出各类中的个案
par(opr)   
opar<-par(mfrow(2,1),mar(5.2,4,0,0))
plclust(hc3,hang=-1)
re3<-rect.hclust(hc3,k=5,border="red")
plclust(hc4,hang=-1)
re4<-rect.hclust(hc4,k=5,border="red")

#两步聚类    资料暂无
daisy(x, metric = c("euclidean", "manhattan", "gower"), stand = FALSE, type = list(), weights = rep.int(1, p))

#2.K-Mediods 进行聚类分析
#k-mediods中包含pam（围绕中心点的划分）、clara、pamk三种算法，我们通过iris数据集来看看三者表现
install.packages("cluster")
library(cluster)
iris2.pam<-pam(iris2,3)
table(iris$Species,iris2.pam$clustering)
layout(matrix(c(1,2),1,2)) #每页显示两个图
plot(iris2.pam)
layout(matrix(1))

iris2.clara<-clara(iris2,3)
table(iris$Species,iris2.clara$clustering)
layout(matrix(c(1,2),1,2)) #每页显示两个图
plot(iris2.clara)
layout(matrix(1))

install.packages("fpc")
library(fpc)
iris2.pamk<-pamk(iris2)
table(iris2.pamk$pamobject$clustering,iris$Species)
layout(matrix(c(1,2),1,2)) #每页显示两个图
plot(iris2.pamk$pamobject)
layout(matrix(1))
#通过上述分类结果可以看到，pam和calra算法分类结果基本类似，但是pamk将三类分为了两类。

#基于密度的聚类
library(fpc)
iris2<-iris[-5]
ds<-dbscan(iris2,eps=0.42,MinPts = 5)
table(ds$cluster,iris$Species)
plot(ds,iris2)
#打印出iris第一列和第四列为坐标轴的聚类结果
plot(ds,iris2[,c(1,4)])
#另一个表示聚类结果的函数，plotcluster
plotcluster(iris2,ds$cluster)


#判别分析(Bayes判别，距离判别，Fisher判别)(少有包涉及了判别分析)
#fisher线性判别    得到判别函数
df<-read_sav("E:/R/R.practice/19.4.sav")
#把分组变量变为定性变量
group <- factor(df$group)
#随机抽取20个一般样本做训练样本
#train <- sample(1:31,20)
#显示训练样本中各类的比例
#table(group[train])
library(MASS)
Z<-lda(group~x1+x2+x3+x4+x5+x6+x7,data = df,prior=c(1,1,1)/3),dat na.action=)      #适用于公式fromula
Z           #判别函数系数--Coefficients of linear discriminants:
#lda(x, grouping, prior = proportions, tol = 1.0e-4,method, CV = FALSE, nu, ...)  #适用于数据框data.frame
#grouping则指明每个观测样本所属类别，prior可以设置各类别的先验概率 prior = c(1 , 1)/2：指的是先验概率相等：q1=q2=1/2，
#prior = c(1 , 1，1)/3：指的是q1=q2=q3=1/3，prior = c(13,7)/20)：指的是q1=13/20；q2=7/20   tol用于保证判别效果默认取0.0001，na.action用于选择对于缺失值的处理
zpre<-predict(Z)
newGroup<-zpre$class
cbind(df$Grop , zpre$x , newGroup)       ##显示预测前后分组结果
tab <- table(df$group , newGroup)
tab
sum(diag(prop.table(tab)))           #求判别率

#二次判别    当多总体之间的协方差矩阵不相同时，距离判别函数为非线性形式，一般为二次函数
#具体操作与结果与线性判别相似
library(MASS)
Z<-qda(group~x1+x2+x3+x4+x5+x6+x7,data = df,prior=c(1,1,1)/3),dat na.action=) 

#距离判别分析     #WeDiBaDis  package
TV_data <- read.table("clipboard" , header = T)
TV_data$G <- as.factor(TV_data$G) 
library(ggplot2)
#建立图层的底层
base_plot <- ggplot(data = TV_data , aes(color = G))
#显示质量评分和功能评分与销售状况的关系
base_plot + geom_point(aes(x = Q , y = C)) + geom_text(aes(x = Q , y = C , label = G) , vjust = -0.8)
#显示质量评分和销售价格与销售状况的关系
base_plot + geom_point(aes(x = Q , y = P)) + geom_text(aes(x = Q , y = P , label = G) , vjust = -0.8)
#显示功能评分和销售价格与销售状况的关系
base_plot + geom_point(aes(x = C , y = P)) + geom_text(aes(x = C , y = P , label = G) , vjust = -0.8)
library(WeDiBaDis)
#把TV_data转换成数据矩阵
TV_data_matrix <- as.matrix(TV_data)
#把矩阵转换成数值型
TV_data_matrix_1 <- matrix(as.integer(TV_data_matrix) , ncol = 4)
#进行马氏距离判别分析
TV_disc<-WDBdisc(data = TV_data_matrix_1 , datatype = "m" , 
classcol = 1 , distance = "Mahalanobis")
summary(TV_disc)
#构建新测试数据矩阵
new_test_data <- matrix(c(8.0 , 7.5 , 65) , nrow = 1)
#代入训练模型，预测新测试数据分类
summary(WDBdisc(data = TV_data_matrix_1 , datatype = "m" , 
classcol = 1 , new.ind = new_test_data , distance = "Mahalanobis"))

#BAYES判别分析
#假设先验概率相等，此时bayes判别函数等价于fisher线性判别函数。  如上述操作
library(MASS)
bayes1 <- lda(G ~ . , data = TV_data , prior = c(1 , 1)/2)
bayes1_predict <- predict(bayes1)
cbind(TV_data$G , bayes1_predict$x , bayes1_predict$class)

#假设先验概率不一样，取q1=13/20 , q2=7/20，然后建立bayes判别函数
library(MASS)
bayes2 <- lda(G~. , data = TV_data , prior = c(13,7)/20)
bayes2_predict <- predict(bayes2)
cbind(TV_data$G , bayes2_predict$x , bayes2_predict$class)


"""主成分分析
步骤:
    数据预处理(保证数据中没有缺失值)
    选择因子模型(判断是PCA还是EFA)
    判断要选择的主成分/因子数目
    选择主成分
    旋转主成分
    解释结果
    计算主成分或因子的得分"""

df<-read_sav("E:/R/R.practice/data20.1.sav")     #spss中没有主成分分析，是在因子分析中实施的
analysis_df<-df[,2:7]
#1.标准误、方差贡献率和累积贡献率（大于85%）
dfpr<- princomp(analysis_df, cor = TRUE)
summary(dfpr,loadings=TRUE)        #其中的loading中对于变量的参数即可组成函数
#主成分回归
pre<-predict(dfpr)
df$Z1<-pre[,1]
df$Z2<-pre[,2]
df$Z3<-pre[,3]
...
lm.pca<-lm(y~Z1+Z2+Z3...,df)
summary(lm.pca)

#2.使用碎石图/累积贡献率确定需要提取的主成分个数
#碎石图(有n个点在水平线1上,所以需要n个主成分)   
library(psych)
fa.parallel(analysis_df,fa='pc',n.iter = 100,
show.legend = F,main = 'Scree plot with parallel analysis')
dfcor<-cor(analysis_df)      #变量间的相关矩阵
#提取主成分
pc <- principal(analysis_df,nfactors = 3,rotate="none"/"varimax",scores=T)
pc
#得到成分矩阵（成分载荷）
pc$loading
#获取主成分得分
pc <- principal(analysis_df,nfactors = 3,scores = T)
head(pc$scores)
#获取主成分的得分系数
round(pc$weights,2)



#探索性因子分析（EFA）   #可以复刻spss中的主成分分析
#1.判断需要提取的因子数
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
fa.parallel(correlations,n.obs = 112,fa='both',n.iter=100,
main = 'Scree plots with parallel analysis')
#2.提取公共因子
fa <- fa(correlations,nfactors = 2,rotate = 'none',fm='pa')
fa
#3. 因子旋转(正交旋转)
fa.varimax <- fa(correlations,nfactors=2,rotate='varimax',fm='pa')
fa.varimax
fa.diagram(fa.promax,simple = F)
#斜交旋转提取因子
fa.promax <- fa(correlations,nfactors=2,rotate='Promax',fm='pa')
fa.promax
fa.diagram(fa.promax,simple = F)
#计算得分
fsm <- function(oblique) {
    if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
      warning("Object doesn't look like oblique EFA")
   } else {
      P <- unclass(oblique$loading)
      F <- P %*% oblique$Phi
      colnames(F) <- c("PA1", "PA2")
      return(F)
    }
 }
fsm(fa.promax)


#信度分析   克隆巴赫信度系数（cronbach's α） 用于评价问卷的内部一致性。

cronbach <- function(x){
  n <- ncol(x)  #计算x的列数(题目数)
  p <- sum(apply(x, 2, var)) / var(apply(x, 1, sum)) 
#计算公式最右边的一坨(sigma Si 2)/(St 2)
#apply(x,1/2,function)：对行/列进function的计算。
#2为按列处理，1为按行处理
  alpha = n/(n-1) * (1-p) 
#计算alpha系数
  cronbach <- cat("alpha=", alpha, "\n")   
#结果输出。cat是连接符
  return(cronbach) 
#返回alpha系数
}
x<-x<-read_sav("E:/R/R.practice/data21.1.sav") 
x<-as.data.frame(x[,2:30])
cronbach(x)

#效度分析    多为结构效度：测量题与测量变量之间的对应关系
#探索性因子分析        KMO&Bartlett检验（巴特球形检验）  （计算协方差矩阵/相关系数矩阵）
library(psych)
cb <- cortest.bartlett(cor(rc), n=length(rc[,1]))
kmo <- KMO(cor(rc))
output <- list(cb[['chisq']],cb[['p.value']],cb[['df']],kmo[["MSA"]])
output
# 查看数据类型
typeof(ability.cov)#list
#设置小数位数
options(digits=2)  
#将ability.cov数据集中的cov矩阵放入covariances中，并转化为相关系数矩阵。
covariances<-ability.cov$cov  
correlations<-cov2cor(covariances)  
correlations
#确定提取的公因子个数
#psych包中的fa.parallel()函数绘制碎石图
library(psych)  
cvs <- ability.cov$cov  
#将cv转化为相关矩阵  
crs <- cov2cor(cv) 
#碎石图
fa.parallel(correlations,
              n.obs=112, #观测数量(矩阵和dataframe不需要标明)
              fa="both", #principal components (fa="pc")  ； principal axis factor analysis (fa="fa")
              n.iter=100,  #自主抽样(bootstrap)100次
              main="Scree plots with parallel analysis")#标题
 #主轴迭代法提取公因子 
fa<-fa(correlations,nfactors=2,rotate="none",fm="pa")  
fa
#用最大方差法varimax旋转
>fa.varimax<-fa(correlations,nfactors=2,rotate="varimax",fm="pa")  
>fa.varimax 
#斜交旋转的promax
>fa.promax<-fa(correlations,nfactors=2,rotate="promax",fm="pa")  
>fa.promax
#变量与因子之间的相关性
fsm<-function(oblique){  
  if(class(oblique)[2]=="fa"&is.null(oblique$Phi)){  
    warning("Object doesn't look like oblique EFA")  
  }else{  
    P<-unclass(oblique$loading)  
    F<-P%*%oblique$Phi  
    colnames(F)<-c("PA1","PA2")  
    return (F)  
  }  
}  
fsm(fa.promax)  
#KMO值：如果此值高于0.8，则说明效度高；如果此值介于0.70.8之间，则说明效度较好；
#如果此值介于0.60.7，则说明效度可接受，如果此值小于0.6，说明效度不佳


#ROC曲线  AUC越大，说明该指标的诊断能力越强(AUC[0.5,0.7]:诊断价值较低； AUC[0.7,0.9]:诊断价值中等；AUC>0.9:诊断价值高； )
#诊断试验ROC
library(pROC)
library(ggplot2)
#建立曲线
x<-read_sav("E:/R/R.practice/data22.1.sav") 
rocobj1<-roc(x$组别,x$身高,levels=c(0,1),direction="<")
rocobj2<-roc(x$组别,x$体重,levels=c(0,1),direction="<")
rocobj3<-roc(x$组别,x$胸围,levels=c(0,1),direction="<")
rocobj4<-roc(x$组别,x$肺呼量,levels=c(0,1),direction="<")
#计算full AUC
auc(rocobj1)
auc(rocobj2)
auc(rocobj3)
auc(rocobj4)
#绘制曲线
plot(rocobj1)
#其他参数美化
plot(rocobj1,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),
max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)
#计算部分面积  AUC选择关注一定范围数据
plot(rocobj1,print.auc=TRUE,auc.polygon=TRUE,partial.auc=c(0.8,0.4),partial.auc.focus="sp",
grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE,reuse.auc=FALSE)
#比较两个曲线，pROC提供三种方法比较“delong”, “bootstrap”或“venkatraman”
roc.test(rocobj1,rocobj2,method = "bootstrap")
#ggroc(功能仍在测试中)绘制Multiple curves   多条曲线相比
g3<-ggroc(list(s100b=rocobj,wfns=rocobj2,ndka=rocobj3))
g3
#在原有的ROC曲线图中添加ROC曲线
library("pROC")##roc
x<-read_sav("E:/R/R.practice/data22.1.sav") 
rocobj1<-roc(x$组别,x$身高,levels=c(0,1),direction="<")
rocobj2<-roc(x$组别,x$体重,levels=c(0,1),direction="<")
polt(roc1,col="blue")
polt.roc(roc2,add=TRUE,col="red")
#单连续性变量指标进行诊断，直接使用pROC包中的roc函数
#多变量指标进行联合诊断时，先建立logistic回归模型，并计算出相应的预测值，对预测值进行roc曲线作图  如

#中介效应    如果自变量X通过某一变量M对因变量Y产生一定影响，则称M为X和Y的中介变量
library(mediation)
data(jobs)
b <- lm(job_seek ~ treat + econ_hard + sex + age, data=jobs)
c <- lm(depress2 ~ treat + job_seek + econ_hard + sex + age, data=jobs)
contcont <- mediate(b, c, sims=50, treat="treat", mediator="job_seek")
summary(contcont)
plot(contcont)
#结果中ACME即间接效应；   ADE即直接效应   total effect为总的效应
#Prop.mediated为中介变量解释X与Y间关联所占的百分比   上图三条线都与0线交叉，显示结果无统计学意义。

#调节效应
library(car)   
data(Ginzberg)  # 调用car包中的Ginzberg数据
library(pequod) 
model1<-lmres(adjdep~adjsimp*adjfatal, centered=c("adjsimp", "adjfatal"),data=Ginzberg) 
# 因变量为adjdep；自变量有：adjsimp，adjfatal，adjsimp与adjfatal的交互效应。
summary(model1, type="nested")                                 # 显示结果
S_slopes<-simpleSlope(model1,pred="adjsimp",mod1="adjfatal")  # 计算simple slope
summary(S_slopes)                                             # 显示simple slope的结果
PlotSlope(S_slopes,namemod=c("高","低"))                      # 画图


#倾向性评分   需要消除混杂因素Z的影响，常用方法有：1）分层匹配；2）多元回归模型；3）倾向性评分匹配。
#上述3种方法的优势：当混杂因素X较少或已明确时，可采用分层匹配。利用多元回归模型需不断调整混杂因素以评价X对Y的独立作用大小。
                    #而当混杂因素较多较复杂时，倾向性评分匹配则呈现出特有的优势。
library(MatchIt)
mydata <- read.csv ("C:/tumor/R-data.csv")
attach (mydata)
mydata [1:20,]
m.out = matchit (radio ~ sex + age + margin + lymph + differentiated + disease
                 + Tu + M + site + neck + targeted, method ="nearest", ratio =1)
summary (m.out)
plot (m.out, type = "jitter")
plot (m.out, type = "hist")

#一致性检验
#定性资料一致性评价
#1.kappa检验
require(irr)                    #library(irr)
data(diagnoses)
dat=diagnoses[,c(1,2)]
kappa2(dat[,c(1,2)],'unweighted')
2.McNemar检验
Performance <-
matrix(c(794, 86, 150, 570),nrow = 2,
      dimnames = list("1st Survey" = c("Approve","Disapprove"),
                       "2nd Survey" =c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
#计量资料一致性评价    pearson/组内相关系数/Bland-Altman法/最小二乘回归


#决策曲线
#logistic回归分析（多个模型）
library(rmda)
#因变量outcome必须为0，1的numeric类型
model_Clinic <- decision_curve(outcome ~ 年龄+`入院改良RANKIN量表评分(mRS)`+TOAST分型及血管情况,data=trainClinic,
                          family=binomial(link='logit'),
                          thresholds=seq(0,1,by=0.01),
                          confidence.intervals=0.95)

model_Omics <- decision_curve(outcome ~ RS,data=mdata_train,
                          family=binomial(link='logit'),
                          thresholds=seq(0,1,by=0.01),
                          confidence.intervals=0.95)


model_And <- decision_curve(outcome~年龄+`入院改良RANKIN量表评分(mRS)`+TOAST分型及血管情况+RS,data=Clinic_Omics,
                          family=binomial(link='logit'),
                          thresholds=seq(0,1,by=0.01),
                          confidence.intervals=0.95)

model_all <- list(model_Clinic,model_Omics,model_And)
plot_decision_curve(model_all,curve.names=c('Clinical model','Radiomics model','Clinical-Radiomics model'),
                    xlim=c(0,1.0),col=c('red','green','blue'),
                    confidence.intervals = F,
                    standardize = T,legend.position = "topright",
                    lty = c(1,2,3))

#校准曲线（单个模型）
fit1Cal<-lrm(outcome~年龄+`入院改良RANKIN量表评分(mRS)`+TOAST分型及血管情况+RS,data=Clinic_Omics,x = T, y = T)
cal1 <- calibrate(fit1Cal, method="boot", B=500)
plot(cal1, xlim=c(0,1.0), ylim=c(0,1.0),
     xlab = "Predicted Probability", 
     ylab = "Observed Probability",
     legend = FALSE,subtitles = FALSE)
#abline对角线
abline(0,1,col = "black",lty = 2,lwd = 2)
#再画一条模型预测的实际曲线
lines(cal1[,c("predy","calibrated.orig")], 
      type = "l",lwd = 2,col="red",pch =16)
#再画一条模型Bias-corrected是校准曲线
lines(cal1[,c("predy","calibrated.corrected")], 
      type = "l",lwd = 2,col="green",pch =16)
legend(0.55,0.35,
       c("Ideal","Apparent","Bias-corrected"),
       lty = c(2,1,1),
       lwd = c(2,1,1),
       col = c("black","red","green"),
       bty = "n") # "o"为加边框

#绘制校准曲线（单个模型）
library(runway)                 
cal_plot(measure_data,          
         outcome = "HeartDisease",          
         prediction = "pred",          
         n_bins = 10)      
# 绘制ROC曲线
roc_plot(measure_data,          
         outcome="HeartDisease",          
         prediction="pred")


#机器学习模型分析（多个模型）
library(tidymodels)
library(discrim)

#设置机器学习模型
Clinic_Omics_Train<-read.xlsx("C:/Users/txh/Desktop/nrrdata/Clinic_Omics_Train.xlsx")
Clinic_Omics_Train$症状持续时间<-factor(Clinic_Omics_Train$症状持续时间,levels=c("＜1月","1-3月","3-6月","＞6月"),labels=c("1","2","3","4")) 
Clinic_Omics_Train$乏力<-factor(Clinic_Omics_Train$乏力,levels=c("有","无"),labels=c("1","0"))
Clinic_Omics_Train$椎体受累数目<-factor(Clinic_Omics_Train$椎体受累数目,levels=c("单椎体","相邻两椎体","多椎体"),labels=c("1","2","3"))
Clinic_Omics_Train$矢状位骨质破坏2<-factor(Clinic_Omics_Train$矢状位骨质破坏2,levels=c("局限，骨质破坏＜1/2","弥漫，骨质破坏＞1/2"),labels=c("1","2"))
Clinic_Omics_Train$椎体楔变塌陷<-factor(Clinic_Omics_Train$椎体楔变塌陷,levels=c("有","无"),labels=c("1","0"))
Clinic_Omics_Train$轴向骨质破坏<-factor(Clinic_Omics_Train$轴向骨质破坏,levels=c("有","无"),labels=c("1","0"))

df <- Clinic_Omics_Train[,c(11,15,17,20,21,22)]
df$症状持续时间<-as.numeric(df$症状持续时间)
df$乏力<-as.numeric(df$乏力)
df$椎体受累数目<-as.numeric(df$椎体受累数目)
df$矢状位骨质破坏2<-as.numeric(df$矢状位骨质破坏2)
df$椎体楔变塌陷<-as.numeric(df$椎体楔变塌陷)
df$轴向骨质破坏<-as.numeric(df$轴向骨质破坏)
df$outcome<-Clinic_Omics_Train$outcome
df$outcome<-factor(df$outcome)

rec <- recipe(outcome~症状持续时间+乏力+椎体受累数目+矢状位骨质破坏2+椎体楔变塌陷+轴向骨质破坏,df)  # 设置recipe    
xgb_mod <- boost_tree() %>%           
  set_engine("xgboost") %>%           
  set_mode("classification") 
           
logistic_mod <-          
  logistic_reg() %>%          
  set_engine('glm')          
      
          
kknn_mod <-          
  nearest_neighbor() %>%          
  set_engine('kknn') %>%          
  set_mode('classification')          
          
rf_mod <-          
  rand_forest() %>%          
  set_engine('ranger') %>%          
  set_mode('classification')          
          
svm_mod <-          
  svm_rbf() %>%          
  set_engine('kernlab') %>%          
  set_mode('classification')

#模型拟合
wf <- workflow_set(preproc=list(rec),          
                   models=list(xgb=xgb_mod,       
                        log= logistic_mod,           
                        knn=kknn_mod,          
                        rf=rf_mod,          
                        svm=svm_mod))          
folds <- bootstraps(df,10)         #设置重采样 
ctr <- control_resamples(save_pred = TRUE)    # 控制条件，保存预测值
# 模型拟合 
wf_res <- wf %>% workflow_map("fit_resamples",    #重采样拟合       
                              resamples=folds,    # 重采样     
                              control=ctr)


#绘制DCA曲线
#单个模型
library(dcurves)          
collect_predictions(wf_res) %>%           
  group_by(wflow_id) %>%          
  group_map(~dca(data=.x,outcome~.pred_1))

#组合模型
dca_data <- collect_predictions(wf_res) %>%           
  group_by(model) %>%           
  select(outcome,.pred_1) %>%           
  tidyr::pivot_wider(          
    names_from = model,        
    values_from = .pred_1) %>%          
  unnest()
dca(data = dca_data,outcome~.) %>%          #,thresholds = seq(0, 0.35, by = 0.01)设置x轴范围
  as_tibble() %>%           
  ggplot(aes(x = threshold,           
             y = net_benefit,           
             color = label)) +          
  geom_line(lwd=1) +          
  coord_cartesian(ylim = c(-0.2, 0.3)) +          
  scale_x_continuous(          
    labels = scales::percent_format(accuracy = 1)) +          
  labs(x = "Threshold Probability",           
       y = "Net Benefit", color = "") +          
  theme_bw()+          
  theme(legend.position = c(0.85,0.8))


##决策曲线--方法二
#机器学习模型分析（单个模型）例子
# 构建XGBoost模型数据集
dt$HeartDisease <- if_else(dt$HeartDisease=="No",0,1)          
dtrain <- xgb.DMatrix(data = data.matrix(dt[,-11]),          
                      label=dt$HeartDisease)
                      #设置XGBoost模型参数
params <- list(objective="binary:logistic",          
               max_depth=3,          
               eta=0.2,          
               verbose=-1,          
               eval_metric="auc")
               # XGBoost模型拟合
xgb_fit <- xgb.train(dtrain,          
                     params=params,          
                     nrounds = 100)
# 模型预测
xgb_pred <- predict(xgb_fit,newdata=data.matrix(dt[,-11]))    #连续性数值Prob
# 数据整理
measure_data <- dt %>%           
  select(HeartDisease) %>%           
  bind_cols(pred=xgb_pred)

library(dcurves)  #实例
#训练集--临床
TrianClinic <- Clinic_Omics_Train %>%           
    select(outcome) %>%           
    bind_cols(RF_model=probRFTrainClinic,LR_model=probLRTrainClinic,SVC_model=probSVCTrainClinic,KNN_model=probKNNTrainClinic,XGB_model=probXGBTrainClinic)
dca(data=TrianClinic,outcome~RF_model+LR_model+SVC_model+KNN_model+XGB_model)
#测试集--临床
TestClinic <- Clinic_Omics_Test %>%           
    select(outcome) %>%           
    bind_cols(RF_model=probRFTestClinic,LR_model=probLRTestClinic,SVC_model=probSVCTestClinic,KNN_model=probKNNTestClinic,XGB_model=probXGBTestClinic)
dca(data=TestClinic,outcome~RF_model+LR_model+SVC_model+KNN_model+XGB_model)

#训练集--组学
TrianOmics <- Clinic_Omics_Train %>%           
    select(outcome) %>%           
    bind_cols(RF_model=probRFTrainOmics,LR_model=probLRTrainOmics,SVC_model=probSVCTrainOmics,KNN_model=probKNNTrainOmics,XGB_model=probXGBTrainOmics)
dca(data=TrianOmics,outcome~RF_model+LR_model+SVC_model+KNN_model+XGB_model)
#测试集--组学
TestOmics <- Clinic_Omics_Test %>%           
    select(outcome) %>%           
    bind_cols(RF_model=probRFTestOmics,LR_model=probLRTestOmics,SVC_model=probSVCTestOmics,KNN_model=probKNNTestOmics,XGB_model=probXGBTestOmics)
dca(data=TestOmics,outcome~RF_model+LR_model+SVC_model+KNN_model+XGB_model)

#训练集--联合
TrianClinicOmics <- Clinic_Omics_Train %>%           
    select(outcome) %>%           
    bind_cols(RF_model=probRFTrainClinicOmics,LR_model=probLRTrainClinicOmics,SVC_model=probSVCTrainClinicOmics,KNN_model=probKNNTrainClinicOmics,XGB_model=probXGBTrainClinicOmics)
dca(data=TrianClinicOmics,outcome~RF_model+LR_model+SVC_model+KNN_model+XGB_model)
#测试集--联合
TestClinicOmics <- Clinic_Omics_Test %>%           
    select(outcome) %>%           
    bind_cols(RF_model=probRFTestClinicOmics,LR_model=probLRTestClinicOmics,SVC_model=probSVCTestClinicOmics,KNN_model=probKNNTestClinicOmics,XGB_model=probXGBTestClinicOmics)
dca(data=TestClinicOmics,outcome~RF_model+LR_model+SVC_model+KNN_model+XGB_model)



#绘制ROC曲线
# 提取模型AUC值并排序
rank_results(wf_res,rank_metric = "roc_auc") %>% 
  filter(.metric=="roc_auc") %>% 
  select(model,mean)
#模型结果可视化
autoplot(wf_res) # Accuracy和AUC
#绘制多个模型的ROC曲线   基于ggplot2
collect_predictions(wf_res) %>%           
  group_by(model) %>%          
  roc_curve(outcome,.pred_0) %>%          
  ggplot(aes(x=1-specificity,y=sensitivity,color=model))+          
  geom_line(lwd=0.8)+          
  theme(legend.position = c(.98, .65),          
        legend.justification = c("right", "top"))

#计算C指数/HL检验--方法一    校准曲线是Hosmer-Lemeshow检验的可视化
#HL检验及C-index指数
library(ResourceSelection)
hl <- hoslem.test(fit$y, fitted(fit), g=10)
hl
#C-index及其95%CI
fitCal<-lrm(outcome~年龄+`入院改良RANKIN量表评分(mRS)`+TOAST分型及血管情况+RS,data=trainClicnicRS,x = T, y = T)
v<- validate(fitCal, method="boot", B=1000)
orig_Dxy = v[rownames(v)=="Dxy", colnames(v)=="index.orig"]
corrected_Dxy = v[rownames(v)=="Dxy", colnames(v)=="index.corrected"]
orig_C_index <- abs(orig_Dxy)/2+0.5
bias_corrected_C_index  <- abs(corrected_Dxy)/2+0.5
cbind("C指数"=orig_C_index,"校正C指数"=bias_corrected_C_index)

#计算模型中每个变量的C指数
c<-rcorrcens(formula=outcome~年龄+`入院改良RANKIN量表评分(mRS)`+TOAST分型及血管情况+RS,data=trainClicnicRS)
c
c[1,1]-1.96*c[1,4]/2
c[1,1]+1.96*c[1,4]/2

probClinicTest<-predict.glm(object =fitCal,newdata=trainClicnicRS,type = "response")
rocTestClinic<-roc(trainClicnicRS$outcome, probClinicTest,levels=c("1","0"),ci=T,auc=T)   #计算AUC 95%CI
c_index <- rocTestClinic@cindex
#c指数的95%CI    c±1.96*sd/2     c±1.96*se






