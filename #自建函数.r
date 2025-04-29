#自建函数

#自建独立样本t检验（仅知道两样本的均数、标准差、样本量）

 t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
 {
     if( equal.variance==FALSE ) 
     {
        se <- sqrt( (s1^2/n1) + (s2^2/n2) )
         # welch-satterthwaite df
         df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
     } else
     {
         # pooled standard deviation, scaled by the sample sizes
         se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
         df <- n1+n2-2
     }      
     t <- (m1-m2-m0)/se 
     dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
     names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
     return(dat) 
}

#例子   自己函数独立样本t检验
set.seed(0)
x1 <- rnorm(100)
x2 <- rnorm(200) 
# you'll find this output agrees with that of t.test when you input x1,x2
tt2 <- t.test2(mean(x1), mean(x2), sd(x1), sd(x2), length(x1), length(x2))

#常规独立样本t检验
tt <- t.test(x1, x2)
