#像Excel一样使用R进行数据分析(1)
#1.生成数据表
#导入数据表
  data <- data.frame(read.csv("C:/User/txh/Desktop/data.cvs"))   #cvs files
  library(readxl)
  data<-data.frame(read_xlsx("C:/Users/txh/Desktop/SDB.xlsx",sheet = 2))  #excel files
  data <- data.frame(id=c("001","002","003","004","005"),
                     city=c("beijing","shanghai","guangzhou","shenzhen","nanchang"),
                     age=c(23,44,54,32,34),
                     category=c("100-A","100-B","200-A","200-B","300-A")
                     price=c(1200,NA,2133,5433,NA))          #create manually 

#数据表检查
  dim(data)      #查看维度（行，列）
  fix(data)
  View(data)
  str(data)
  typeof(data$id)
  length(data$id)
  is.na(data$price)
  unique(data$id)
  duplicated(data$id)
  range(data$age)
  min(data$age)
  which.min(data$age)
  max(data$age)
  which.max(data$age)
  name(data)       #View column name
  head(data,n=10)  #view the first ten lines
  tail(data,n=10)  #view the last ten lines
  
#数据表的清洗
  data_na <- na.omit(data)       #delete na
  data[is.na(data)] <- 0         # 0 replace na
  data[is.na(data$price)] <- mean(data_na$price)    #mean replace na
  data$city <- toupper(data$city)
  data$city <- tolower(data$city)
  data$age <- as.integer(data$age)
  data$city <- as.character(data$city)
  data$category <- as.factor(data$category,levels=c(),labels=c())
  data$city <- gsub("shangha","SHi",data$city)     #data change  gsub(old,new,data)

#数据合并
  data1 <- data.frame(id=c("001","002","003","004","005"),
                      color=c("red","yellow","blue","orange","black"),
                      size=c(1,4,6,8,12))
  df <- cbind(data,data1)          #列合并，需要行数一样
  data2 <- data.frame(id=c("006","007"),
                     city=c("nanjing","hefei"),
                     age=c(469,65),
                     category=c("400-A","400-B")
                     price=c(1827,1534))          #create manually 
  df <- rbind(data,data2)          #行合并，需要列数一样
  id_city<-paste(df$id,df$city)    #将不同列合并成一列
  rate=round(data$price/sum(data$price),digits = 2)    #计算新列
  df=cbind(data,rate)
  
  data[order(data$id)]           #正序
  data[order(data$id,decreasing = TRUE)]       #倒序
  data[order(data$age,data$price,data$age,decreasing = TRUE)]
  data <- as.character(data$category)
  data_c <- strsplit(data$category,"-")
  color <- sapply(data_c,1)
  size <- sapply(data_c,2)
  category1 <- data.frame(color,size)
  df <- cbind(data,category1)
  
#数据表的合并
  df<-merge(df1,df2,by="id")         #合并方式inner
  df<-merge(df1,df2,by="id",all=TURE)      #合并方式outer
  df<-merge(df1,df2,by="id",all.x=TURE)   #合并方式left
  df<-merge(df1,df2,by="id",all.y=TURE)   #合并方式right
  
  rownames(data)=data$id         #设置索引列
  data$group <- ifelse(data$price>=2000,"high","low")
  
  data[c(2:5,7,9),c(1,4,8)]     #提取第2至5，7，9行，1，4，8列的数据
  data["002","city"]
  data[which.max(data$age),]

#数据筛选
  subset(data,data$city=="shanghai")
  subset(data,data$city!="shanghai")
  subset(data,data$city=="shanghai"&data$price>=2000)
  subset(data,data$city=="shanghai"|data$price>=2000)
  sum(subset(data,data$city=="shanghai"&data$price>=2000))
  length(subset(data,data$city=="shanghai"&data$price>=2000))
  mean(subset(data,data$city=="shanghai"&data$price>=2000))
#数据汇总
  table(data$category)
  table(data$city,data$categeory)
  tapply(data$price,data$city,sum)     #按照city对price求和汇总

#数据统计
 summary(data$price)
 sum(data$price)
 mean(data$price)
 var(data$price)
 sd(data$price)
 cov(data$weight,data$height)       #协方差
 cor(data$weight,data$height)       #相关系数

#数据输出
 write.csv(x=data,file="filename.csv")
 write.table(x=data,file="filename.csv",sep = ",")
 