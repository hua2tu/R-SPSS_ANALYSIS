#R中的数值转换
mydata<-data.frame(
       Name = c("苹果","谷歌","脸书","亚马逊","腾讯"),
       Conpany = c("Apple","Google","Facebook","Amozon","Tencent"),
       Sale2013 = c(5000,3500,2300,2100,3100),
       Sale2014 = c(5050,3800,2900,2500,3300),
       Sale2015 = c(5050,3800,2900,2500,3300),
       Sale2016 = c(5050,3800,2900,2500,3300)
       )

#melt函数是reshape2包中的数据宽转长的函数   横变竖
library(reshape2)
mydata1<-melt(
       mydata,                       #待转换的数据集名称
       id.vars=c("Conpany","Name"),  #要保留的主字段
       variable.name="Year",         #转换后的分类字段名称（维度）
       value.name="Sale"             #转换后的度量值名称
       )

#reshape2中的dcast函数可以完成数据长转宽的需求：  竖变横
dcast(
   data=data1,         #数据集名称
   Name+Conpany~Year   #x1+x2+……~class 
   #，表达式左侧列出要保留的主字段（即不会被扩宽的字段，
   #右侧则是要分割的分类变量，扩展之后的宽数据会增加若干列度量值，列数等于表达式右侧分类变量的类别个数
  ）

#或者使用spread函数
spread(
   data=data1,   #带转换长数据框名称
   key=Year,     #带扩宽的类别变量（编程新增列名称）  
   value=Sale)   #带扩宽的度量值 （编程新增列度量值）


#绘制具有R中的部分相关系数的散点图矩阵
pairs.cor <- function (x,y,smooth=TRUE, digits=2,  ...)
{
  panel.cor <- function(x, y, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r.obj = cor.test(x, y,use="pairwise",...)
    r = as.numeric(r.obj$estimate)
    p = r.obj$p.value
    mystars <- ifelse(p < .05, "* ", " ")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(txt, mystars, sep="")
    text(0.5, 0.5, txt)
  }
panel.hist <- function(x)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan")
  }
pairs(x,diag.panel=panel.hist,lower.panel=panel.cor,upper.panel=panel.smooth, ...)
} 

pairs.cor(iris[,1:4])

#典型相关系数的显著性检验
corcoef.test<-function(r, n, p, q, alpha=0.1){
  #r为相关系数 n为样本个数 且n>p+q
  m<-length(r);  Q<-rep(, m);  lambda <-
  for (k in m:){
    lambda<-lambda*(-r[k]^);   #检验统计量
    Q[k]<- -log(lambda)   #检验统计量取对数
  }
  s<-;  i<-m
  for (k in :m){
    Q[k]<- (n-k+-/*(p+q+)+s)*Q[k]  #统计量
    chi<--pchisq(Q[k], (p-k+)*(q-k+))
    if (chi>alpha){
      i<-k-; break
    }
    s<-s+/r[k]^
  }
  i  #显示输出结果 选用第几对典型变量
}

corcoef.test(r=ca$cor,n=,p=,q=)

#线性回归中的R2/adjust_R2/F/p的计算公式
R2<-function(y_test, y_true){

return (1 - (sum((y_test - y_true)^2)/sum((y_true - mean(y_true))^2)) )

}

adjust_R2<-function(y_test, y_true,n,k){

return (1-(n-1)*(1-R2(y_test, y_true))/(n-k) )

}

F<-function(y_test, y_true,n,k){

sse=sum((y_test - y_true)^2)

ssT=sum((y_true - mean(y_true))^2)

return(((n-k)/(k-1))*((ssT-sse)/sse))

}

p<-function(y_test, y_true,n,k){

return(1-pf(F(y_test, y_true,n,k), df1=k-1, df2=n-k))

}

"""倾向性评分的中MatchIt包中matchit函数中的method中 的选择：
匹配方法除了nearest以外，还可以采用
Exact Matching：病例组和对照组在每一变量上精确匹配，参数值完全相同。当协变量较多或者协变量取值范围较大时不宜采用。（method = "exact"）
Subclassification：将数据集分成子集，子集内协变量的分布相同。（method = "subclass"）
Optimal Matching：所有匹配病例之间的平均绝对距离最小，需要安装optmatch包。（method = "optimal"）
Genetic Matching：利用遗传学计算算法匹配，需安装Matching包。（method = "genetic"）
Coarsened Exact Matching：在确保其他协变量平衡下匹配某一协变量。（method = "cem"）"""

#主成分分析与因子分析的先决条件（KMO>0.5,p<0.05）   以下是KMO检验函数  
# KMO Kaiser-Meyer-Olkin Measure of Sampling Adequacy
kmo = function( data ){
  library(MASS)
  X <- cor(as.matrix(data))
  iX <- ginv(X)
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a)
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy

  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the
                                        # correlation matrix. That is the
                                        # negative of the partial correlations,
                                        # partialling out all other variables.

  kmo <- BB/(AA+BB)                     # overall KMO statistic

  # Reporting the conclusion
    if (kmo >= 0.00 && kmo < 0.50){
      test <- 'The KMO test yields a degree of common variance
unacceptable for FA.'
    } else if (kmo >= 0.50 && kmo < 0.60){
      test <- 'The KMO test yields a degree of common variance miserable.'
    } else if (kmo >= 0.60 && kmo < 0.70){
      test <- 'The KMO test yields a degree of common variance mediocre.'
    } else if (kmo >= 0.70 && kmo < 0.80){
      test <- 'The KMO test yields a degree of common variance middling.'
    } else if (kmo >= 0.80 && kmo < 0.90){
      test <- 'The KMO test yields a degree of common variance meritorious.'
    } else {
      test <- 'The KMO test yields a degree of common variance marvelous.'
    }

    ans <- list(  overall = kmo,
                  report = test,
                  individual = MSA,
                  AIS = AIS,
                  AIR = AIR )
    return(ans)

}    # end of kmo()


#克隆巴赫信度系数计算函数
reliability.function <- function(x)
{
    n <- length(x)   #变量个数
    m <- length(x[,1]) #样本个数

   #将每个样本的变量得分求和，保存向量y1中
    y1 <- matrix(nrow=m, ncol=1)
    for(i in 1:length(x[,1]))
    {
      y1[i,1]<- sum(x[i,1:n])
    }

   #将每个变量得分的方差保存在向量y2中
    y2 <- matrix(nrow=1, ncol=n)
    for(i in 1:n)
    {
      y2[1,i] <- var(x[1:m,i])
    }

    alpha <- n / (n-1) * (1 - sum(y2) / var(y1))

    alpha
}