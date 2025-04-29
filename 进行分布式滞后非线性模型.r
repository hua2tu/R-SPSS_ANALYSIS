#使用DLNM包进行分布式滞后非线性模型（普遍应用于研究空气污染的健康效应）
    #主要特点（与分布式滞后线性模型的区别）：利用交叉基（cross—basis）对解释变量进行特征变换，进而以广义线性模型和广义相加模型等传统模型的思想为基础，阐述了分布滞后非线性模型的理论
    #整体的建模流程简单来说就两步：
        #1.利用交叉基进行特征转换，得到新的数据。
        #2.用广义线性模型对变换后的数据进行建模。
#for example
#第一部分
library(dlnm);library(ggplot2);library(mice);library(reshape2)；library(splines)
data<-chicagoNMMAPS
data<-as.data.frame(data)
#data deal
data[data$death>200,"death"]<-NA
data[data$cvd>100,'cvd']<NA
data[data$resp>200,'resp']<-NA
data$temp_dif[2:nrow(data)]<-diff(data$temp)
miceMod<-mice(data)      #进行mice插值（mice package）
data<-complete(miceMod)

#data visivalize
plot_varible <- c("death", "cvd","resp","temp","temp_dif","pm10","pm10")
plot_data <- melt(data,id=c('date'), measure.vars = plot_varible)
plot_data$variable <- factor(plot_data$variable, levels = levels(plot_data$variable), 
labels = c('死亡人数', '心血管疾病致死', '呼吸道疾病致死', '平均温度', '两天间温度差', 
expression(paste(PM[10], " (", mu, g, "/", m^3, ")", sep = "")), 
expression(paste(CO, " (", m, g, "/", m^3, ")", sep = ""))))

#绘制不同自变量因时间的变化图
ggplot(plot_data,aes(x = as.factor(date), y = value, fill=variable, color=variable)) +
  geom_point(alpha=0.5,shape=21) +
  scale_y_continuous(name = "Values of varibles")+
  scale_x_discrete(name = "Time", breaks=c('1988-01-01','1989-01-01','1990-01-01','1991-01-01','1992-01-01','1993-01-01','1994-01-01','1995-01-01','1996-01-01','1997-01-01','1998-01-01','1999-01-01','2000-01-01','2001-01-01')) +
  theme_bw() +
  theme(legend.position = "none")+
  facet_wrap(~variable,ncol=1,scales='free', labeller = label_parsed)+
  theme(axis.title = element_text(face="bold"),
        axis.title.x = element_text(colour="black",size=20),
        axis.title.y = element_text(colour="black",size=20),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black"),
        legend.position = "none",
        axis.text.y = element_text(colour="black",size=12),  
        strip.text = element_text(size=16),
        axis.text.x=element_text(color = "black", size=10, vjust=.8, hjust=0.8))
#温度与死亡散点图
ggplot(data,aes(x = temp, y = death, color='#99FFCC')) +
  geom_point(alpha=0.4)+
  theme_bw()+
  theme(legend.position = "none")+
  scale_y_continuous(name = "Death")+
  scale_x_continuous(name = "Temperature")
#温度与死亡改良后的气泡图（气泡的大小和颜色的深浅反映了所在区间内数据点的数量。
#将数据集中温度的取值范围划分为200个等间隔的小区间，并在每个区间内求均值，绘制了下面的气泡图。）
 scatter_plot <- function(input1, piece) {
  input = data[,paste(input1,"",sep="")]
  m = max(input, na.rm = TRUE)
  n = min(input, na.rm = TRUE)
  output=c()
  for (i in ceiling(piece*n/m):(piece-1)) {
    y_rate =  mean(data[m*(i)/piece < input & input < m*(i+1)/piece, 'death'] , na.rm = TRUE)
    number = length(data[m*(i)/piece < input & input < m*(i+1)/piece, 'death'])
    a <- cbind((i/piece)*m+m/(2*piece), y_rate, number)
    output = rbind(output,a)
    
  }
  output <- as.data.frame(output)
  colnames(output) = c(input1,'death','number')
  # print(output)
  ggplot(output, aes(x=output[,input1], y=death, size=number, color=number)) +
    scale_color_gradient2(low = "#deebf7", mid= '#6baed6',high = "#08306b") +
    geom_point(alpha=0.7) +
    theme(legend.position = "bottom")+
    xlab(input1) + 
    theme_bw()+
    scale_size_continuous(range = c(0.5, 5))
}
png(file = "温度均值散点图.png", width = 1500, height = 800, res = 300)
scatter_plot('temp',200)
dev.off()

#建立自变量交叉基函数（不同的函数方法）  
cb.temp = crossbasis(data$temp, lag=10, argvar=list(fun="bs", degree=2, knots= c(20)),arglag=list(fun="lin"))
# 自然样条 ns，knots与bs样条类似，df决定了自然样条的自由度
cb.temp = crossbasis(data$temp, lag=10, argvar=list(fun="ns", degree=2, knots= c(20)),arglag=list(fun="lin"))
# 多项式函数 poly，只需要选取最高的阶数 degree
cb.temp = crossbasis(data$temp, lag=10, argvar=list(fun="ns", degree=2, knots= c(20)),arglag=list(fun="lin"))
# 阈值函数 thr， 需要选取双侧或单侧阈值
cb.temp = crossbasis(data$temp, lag=10, argvar=list(fun="thr", thr.value=c(19,23), side= 'd'),arglag=list(fun="lin"))
# 地层函数 strata，选取不同的断点，断点间的解释变量取值是常熟
cb.temp = crossbasis(data$temp, lag=10, argvar=list(fun="strata", breaks=c(19,23)),arglag=list(fun="lin"))

#本次建模使用了特征维度使用了ns，滞后维度使用了poly。
cb.temp = crossbasis(data$temp, lag=30, argvar=list(fun="ns", knots= c(20)),arglag=list(fun="poly",degree=4))
# 拟合类 poission 模型
model1 = glm(death ~ cb.temp + ns(time,10*1)，family=quasipoisson(), data) 
summary(model1)
# 拟合模型计算 RR 以及 RR 的置信区间
pred1.temp = crosspred(cb.temp, model1, cen=round(median(data$temp)), bylag=0.2)

# 根据 QBIC、QAIC 选取最大滞后时长
qaicbic <- function(model) {
  phi <- summary(model)$dispersion
  logll <- sum(dpois(ceiling(model$y), lambda=exp(predict(model)), log=TRUE))
  cbind((-2*logll + 2*summary(model)$df[3]*phi),
        (-2*logll + log(length(resid(model)))*phi*summary(model)$df[3]))}

tqba <- data.frame()
for (ii in 7:100) {
  cb.temp = crossbasis(data$temp, lag=ii, argvar=list(fun="ns", knots= c(20)),
                       arglag=list(fun="poly",degree=4))
  model1 = glm(death ~ cb.temp + ns(time,10*1), family=quasipoisson(), data)
  summary(model1)
  q <- cbind(ii,qaicbic(model1))
  tqba <- rbind(tqba,q)
}
print(tqba)

#不同温度、滞后下的相对危险度 - 2D
plot(pred1.temp, "contour", xlab="MeanTemp", key.title=title("RR"),cex.axis=2,
     plot.axes={axis(1,cex.axis=2)
       axis(2,cex.axis=2)},
     key.axes = axis(4,cex.axis=2),
     plot.title=title(xlab="MeanTemp (°C)",ylab="Lag (days)",cex.main=2,cex.lab=1.5))

#不同温度、滞后下的相对危险度 - 3D
plot(pred1.temp,ticktype='detailed',border='#3366FF',xlab="MeanTemp (°C)",ylab="Lag (days)",zlab="RR",col='#99FFCC',shade = 0.1,cex.lab=1.3,cex.axis=1.3,lwd=1,theta = 20, phi = 25,ltheta = -35)

#累积效应plot
par(mfrow=c(1,3))
crall <- crossreduce(cb.temp,model1,cen=20,type="overall",lag=c(1,3))
plot(crall,xlab="Meantemp",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="Overall cumulative association 1-3 days",cex=0.89)

crall <- crossreduce(cb.temp,model1,cen=20,type="overall",lag=c(1,7))
plot(crall,xlab="Meantemp",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="Overall cumulative association 1-7 days",cex=0.89)

crall <- crossreduce(cb.temp,model1,cen=20,type="overall",lag=c(1,30))
plot(crall,xlab="Meantemp",ylab="RR",col=2,lwd=2,cex.lab=1.2,cex.axis=1.2,mar=c(1,2,0,1))
mtext(text="Overall cumulative association 1-30 days",cex=0.89)




#二（简约）
library(dlnm);library(ggplot2);library(mice);library(reshape2)
data<-chicagoNMMAPS
data<-as.data.frame(data)
#data deal
data[data$death>200,"death"]<-NA
data[data$cvd>100,'cvd']<NA
data[data$resp>200,'resp']<-NA
data$temp_dif[2:nrow(data)]<-diff(data$temp)
miceMod<-mice(data)      #进行mice插值（mice package）
data<-complete(miceMod)

#建立自变量交叉基函数
    ##1.暴露-反应维度拟合采用自然立方样条函数，节点位置设置在第10、75与90百分位数
    ##2.暴露-滞后维度拟合同样采用自然立方样条函数，滞后期选择21天，节点位置设置采用程序包提供的对数等间距的方法
    ##3.建立拟合暴露-滞后-反应的交叉基函数
#argvar<-list(fun='ns',konts=c(20)))
#arglag<-list(fun="lin")
basis.tm<-crossbasis(data$temp,lag=10,argvar=list(fun='ns',knots=c(20)),arglag=list(fun="lin"))
#或者(use splines package)
basis.tm<- crossbasis(data$temp, lag=30, argvar=list(fun="ns", knots= c(20)),
                     arglag=list(fun="poly",degree=4))

#构建DLNM分析模型
    ##1.应变量为逐日死亡数，变量名为death
    ##2.自变量为逐日平均温度变量名为tm
    ##3.混杂因素为时间趋势、相对湿度、空气污染浓度、星期几效应。变量名为time、rhum、O3、dow。
    ##4. 根据AIC准则选择自由度df，选择AIC值最小的df建立最终模型
model<-glm(death~basis.tm+ns(time,10*1)+ns(o3,3)+ns(rhum,3)+as.factor(dow),family=quasipoisson,data=data)
AIC<-2*sum(dpois(model$y,model$fitted.value,log=TRUE))+2*summary(model)$df[3]*summary(model)$dispersion
AIC

##对模型进行预测  以温度中位数为参考温度
pred.tm<-crosspred(basis.tm,model,by=0.5,cen=median(data$tm,na.rm=TRUE))
plot(pred.tm,"overall",col=2,cex.axis=1.5,xlab="温度（℃）",ylab="RR",main="NEW YORK",cex.max=1.5)
names(pred.tm)
round(cbind(pred.tm$allRRfit,pred.tm$allRRlow,pred.tm$allRRhigh),digits=3)
##确定风险最低温度（23摄氏度），作为参考温度重新进行模型预测
which.min(pred.tm$allRRfit)
pred.tm<-crossbasis(basis.tm,model,by=0.5,cen=23)
#展示气温-滞后-反应的整体关系(3D plot)
plot(pred.tm,'3d',col=2,cex.axis=1.2,xlab="温度",ylab="滞后日",main='NEW york',cex.main=1.5,cex.xlab1.5)
#气温-死亡关系曲线（滞后21天(lag=21)）：
plot(pred.tm,'overall',col=2,cex.axis=1.5,xlab="温度",ylab="RR",main='NEW york',cex.main=1.5)


attrdl <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,type="af",
  dir="back",tot=TRUE,range=NULL,sim=FALSE,nsim=5000) {
    type <- match.arg(type,c("an","af"))
    dir <- match.arg(dir,c("back","forw"))
    # 选择范围（强制为中心值，否则，意味着零风险）
    if(!is.null(range)) x[x<range[1]|x>range[2]] <- attr(basis,"argvar")$cen
   # 计算矩阵
   # - 如果 dir="back" 则滞后曝光
   # - 如果 dir="forw" 则持续曝光
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    xlag <- if(dir=="back") dlnm:::Lag2(x,seq(lag[1],lag[2])) else 
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(x)!=diff(lag)+1) stop("dimension of 'x' not compatible with 'basis'")
  }
  # 案例：在 dir="forw" 的未来案例中进行转换
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases)!=diff(lag)+1) stop("dimension of 'cases' not compatible")
    cases <- rowMeans(cases)
  } else {
    if(dir=="forw") cases <- rowMeans(dlnm:::Lag2(cases,-seq(lag[1],lag[2])))
  }
  #如果提供模型，则提取 COEF 和 VCOV
  if(!is.null(model)) {
    name <- deparse(substitute(basis))
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- dlnm:::getcoef(model,model.class,cond)
    vcov <- dlnm:::getvcov(model,model.class,cond)
  }
  red <- length(coef)!=ncol(basis)
# 计算交叉基础（或 ONEBASIS，如果减少）
  basisnew <- if(!red) 
    do.call(crossbasis,list(x=xlag,lag=lag,argvar=attr(basis,source("attrdl.R")

