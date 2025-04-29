#R语言绘图

#绘制TIFF图（位图）
tiff(filename = "Rplot%03d.tif",
     width = 480, height = 480, units = "px", pointsize = 12,
     compression = c("none", "rle", "lzw", "jpeg", "zip", "lzw+p", "zip+p"),
     bg = "white", res = NA, family = "", restoreConsole = TRUE,
     type = c("windows", "cairo"), antialias)   # 开启目标图形设备
plot(mtcars$wt, mtcars$mpg)   # 绘图代码
ggplot2()   # 绘图代码
dev.off()  # 关闭目标图形设备

#生成PDF（矢量图）
pdf(file = if(onefile) "Rplots.pdf" else "Rplot%03d.pdf",
    width, height, onefile, family, title, fonts, version,
    paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
    useDingbats, useKerning, fillOddEven, compress)
plot(mtcars$wt, mtcars$mpg) # 绘图代码
dev.off() # 关闭图形设备

#绘制EPS图
setEPS()
postscript("mygraph.eps")
postscript(file = if(onefile) "Rplots.ps" else "Rplot%03d.ps",
           onefile, family, title, fonts, encoding, bg, fg,
           width, height, horizontal, pointsize,
           paper, pagecentre, print.it, command,
           colormodel, useKerning, fillOddEven)  # 开启目标图形设备
plot(rnorm(100), main="Hey Some Data")   # 自己的绘图函数
dev.off()   # 关闭目标图形设备

cairo_ps( 'plot2.eps',height = 6.85,width =9.05 )
forest(datameta,fs.hetstat=10,just = "center",digits.I2 = 1,digits.tau2 = 3,spacing=1.2)
dev.off()



