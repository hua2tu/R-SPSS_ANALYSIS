#ggplot2绘图代码

"""ggplot2是一个基于图形语法的声明式创建图形的系统。你提供数据，告诉ggplot2如何将变量映射到美学(aesthetic)，使用什么图形原语，然后它负责处理细节。
ggplot由几个基本元素构成(使用+连接):
数据ggplot()：作图用的原始数据
几何图形 geom_:表示数据的几何形状
美学 aes(): 几何或者统计对象的美学，比如位置，颜色，大小，形状等
刻度 scale_(): 数据与美学维度之间的映射，比如图形宽度的数据范围，
统计转换 stat_: 数据的统计，比如百分位，拟合曲线或者和
坐标系统 coord_: 数据的转换
面 facet_: 数据图表的排列
主题 theme(): 图形的整体视觉默认值，如背景、网格、轴、默认字体、大小和颜色

ggplot(data,aes(x=weight,y=height))+
geom_point()+
labs(x="",y="",title="",subtitle="",tag="Fig.1")+
xlim(c(0, 100))+
ylim(c(0, 50))+
theme()+
expand_limits(x = 0, y = 0)+
facet_grid(year ~ season)             

#aes()       x,y,colour,shape等参数，其中colour,shape的值可以为变量
geom_point()    包括colour,shape,size,fill,alpha(透明度参数),group等参数
theme_set(theme_bw())      可以修改掉灰色的默认ggplot2外观
theme()是修改特定主题元素(文本、标题、框、符号、背景等)的必要命令
  theme(axis.title.x = element_text(vjust = 0, size = 15),
        axis.title.y = element_text(vjust = 2, size = 15))    更改坐标轴标题的外观
  其中使用element_text()来改变所有或特定文本元素的属性，其参数有
    hjust:水平对齐；  vjust:垂直对齐  colour:颜色  size:大小  face:加粗或倾斜  margin=margin(t/r/b/l=10)：与边缘距离;angle:旋转文字
    theme(axis.text=element_text(),axis.text.x=element_text(),)    更改坐标轴文本(这里是数字)的外观
    theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 12)
    theme(plot.title = element_text(face = "bold.italic",margin = margin(10, 0, 10, 0),size = 14))   更改标题的外观
        bold.italic为加粗斜体，margin(10, 0, 10, 0)中的4个数据分别对应的上下左右
    theme(legend.title = element_text(color = "chocolate",size = 14, face = "bold")
    labs(x = NULL, y = "")          删除坐标轴标题
expand_limits(x = 0, y = 0)        使图表从坐标原点开始
scale_y_continuous(label = function(x) {return(paste(x, "Degrees Fahrenheit"))})   使用函数调整y轴文字标签
根据aes()中的colour，size可以自动生成不同分组的图及其图例
调整图例
    theme(legend.position = "none")    去除图例    
        guides(color = "none")         仅仅去除图例颜色
    theme(legend.title = element_blank())     去除图例标题
    theme(legend.position = "top/bottom/left/right")       改变图例位置（上下左右）
    theme(legend.position = c(0.15, 0.15),legend.background = element_rect(fill = "transparent"))    在面板内显示图例
改变图例的标签
    scale_color_discrete(name = "Seasons:", labels = c("Mar—May", "Jun—Aug", "Sep—Nov", "Dec—Feb")) +
    theme(legend.title = element_text(family = "Playfair", color = "chocolate", size = 14, face = 2))
    theme(panel.background = element_rect(fill = "#64D2AA", color = "#64D2AA", size = 2))    改变面板背景颜色
删除全部/部分网格线
    theme(panel.grid = element_blank())
    theme(panel.grid.minor = element_blank())


创建多面板图：facet_wrap本质上创建了一个基于单个变量的图形带，而facet_grid则生成了包含两个变量的网格。
    创建单个变量的多面板图  facet_wrap(~ year)     其中参数nrow, ncol可以设置面板图的布局
    创建多个变量的多面板图  facet_grid(year~season)
    创建面板的子图各个坐标轴都是一样的，若需要不同，应该在facet_wrap(scales="free")
    修改面板中子图标题
    theme(strip.text = element_text(face = "bold", color = "chartreuse4",hjust = 0, size = 20),
        strip.background = element_rect(fill = "chartreuse3", linetype = "dotted"))

    创建一个组合图
    p1 <- ggplot(chic, aes(x = date, y = temp,color = season)) +geom_point()+geom_rug()
    p2 <- ggplot(chic, aes(x = date, y = temp,color = season)) +geom_point()+geom_line()
    library(patchwork)
    p1 + p2 一行   p1/p2  两行    (g+p1)/p2    第一行是g与p1，第二行是p2

    ggplot2中的颜色
    aes内部的colour=变量是由变量编码的颜色，与变量有关；而那些外部的colour=某种颜色是与变量无关（外周）的属性。
    scale_color_manual(values = c("dodgerblue4",
                                   "darkolivegreen4",
                                   "darkorchid3",
                                   "goldenrod1"))     手动设置分类变量的分组颜色

    theme_bw(base_size = 30, base_family = "Playfair")     改变所有文本元素的大小与字体
在图上添加水平或垂直线
    geom_hline(yintercept = c(0, 73))              简单
    geom_vline(aes(xintercept = median(temp)), size = 1.5,
             color = "firebrick", linetype = "dashed")      竖中线
    geom_hline(aes(yintercept = median(dewpoint)), size = 1.5,
             color = "firebrick", linetype = "dashed")    横中线
      geom_linerange(aes(x = 50, ymin = 20, ymax = 55),
                 color = "steelblue", size = 2)          某一具体长度的线
在图中添加文本
    geom_text(aes(x = 25, y = 60,label = "This is a useful annotation"),stat = "unique",
            family = "Bangers",size = 7, color = "darkcyan")
    进行文本渲染（页面格式渲染）
    library(ggtext)
    lab_html <- "&#9733; This plot shows <b style='color:red;'>temperature</b> in <i>°F</i> versus <b style='color:blue;'>ozone level</b>in <i>ppm</i> &#9733;"
    geom_richtext(aes(x = 33, y = 3, label = lab_html),stat = "unique")

geom_rug()         Rug代表单个数量变量的数据，显示为沿轴线的标记


#ggplot2高级绘图
geom_bar()         条形图
geom_boxplot()   箱线图
geom_density()   密度图
geom_histogram()   直方图
geom_hline()   水平线
geom_jitter()   抖动点
geom_line()   线图
geom_point()   散点图
geom_rug()   地毯图
geom_smooth()   拟合曲线       method, formula、color、fi1l、linetype、size参数
geom_text()   文字注解
geom_violin()   小提琴图
geom_vline()   垂线        其他图中参数多为colorv,fill,alpha,linetype、size

其中参数解读
    color   对点、线和填充区域的边界进行着色
    fill   对填充区域着色，如条形和密度区域
    alpha   颜色的透明度，从0(完全透明)到1(不透明)。
    linetype   图案的线条(1-实线，2-虚线，3-点，4-点破折号，5-长破折号，6-双破折号)
    size   点的尺寸和线的宽度
    shape   点的形状(和pch一样，0-开放的方形，!-开放的圆形，2-开放的三角形，等等)
    position   绘制诸如条形图和点等对象的位置。对条形图来说，"dodge"将分组条形图并排，"stacked"堆叠分组条形图，"fi11"垂直地堆叠分组条形图并规范其高度相等。
            对于点来说，"jitter"减少点重叠
    binwidth   直方图的宽度
    notch   表示方块图（如箱式图）是否应为缺口(TRUE/FALSE)
    sides   地毬图的位置("b“底部，"l"左部，"t"顶部，"r"右部，"bl"左下部等等)
    width   箱线图的宽度


#绘制散点图及其拟合曲线
    ggplot(data=mtcars, aes(x=wt,y=mpg))+
    geom_point(pch=17, color="blue", size=2) +
    geom_smooth(method = "lm", color="red", linetype=2) +
    labs(title="Automobile Data", x="Weight", y="Miles Per Gallon")+
    theme(plot.title = element_text(hjust = 0.5)) 
#绘制散点面板图
    mtcars$am <- factor(mtcars$am, levels=c(0,1),labels=c("Automatic","Manual"))
    mtcars$vs <- factor(mtcars$vs, levels=c(0,1),labels=c("V-Engine","Straight Engine"))
    mtcars$cyl <- factor(mtcars$cyl)
    ggplot(data=mtcars, aes(x=hp, y=mpg, shape=cyl, color=cyl)) +
    geom_point(size=3) +
    facet_grid(am~vs) +
    labs(title="Automobiel Data by Engine Type",x="Horsepower", y="Miles Per Gallon")
#箱式图联合散点图
    ggplot(Salaries, aes(x=rank, y=salary)) +
    geom_boxplot(fill="cornflowerblue",color="black", notch=TRUE) +
    geom_point(position = "jitter", color="blue", alpha=.5) +
    geom_rug(side="1", color="black")
#箱式图联合小提琴图
    ggplot(singer, aes(x=voice.part, y=height)) +
    geom_violin(fill="lightblue") +
    geom_boxplot(fill="lightgreen", width=.2)

#刻面图  facet_wrap()用于单变量        facet_grid()用于多变量
facet_wrap(~var,ncol=n)   将每个var水平排列成n列的独立图       其中var、 rowvar和colvar是因子
facet_wrap(~var,nrow=n)   将每个vaz水平排列成n行的独立图
facet_grid(rowvar~colvar)   rowvar和colvar组合的独立图，其中rowvar表示行，co1var表示列
facet_grid(rowvar~.)   每个rowvar水平的独立图，配置成一个单列
facet_grid(.~colvar)   每个colvar水平的独立图，配置成一个单行

#坐标轴与刻度线外观函数
scale_x_continuous()和scale_y continuous()   breaks=指定刻度标记，labels=指定刻度标记标签，limits=控制要展示的值的范围
scale_x_discrete()和scale_y_discrete()   breaks=对因子的水平进行放置和排序，labels=指定这些水平的标签，limits=表示哪些水平应该展示
coord flip()   颠倒x轴和v轴
#例如
    ggplot(data=Salaries, aes(x=rank, y=salary, fill=sex)) +
    geom_boxplot() +
    scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                    labels=c("Assistant\nProfessor","Associate\nProfessor","Full\nProfessor")) +
    scale_y_continuous(breaks=c(50000,100000,150000,200000),labels=c("$50K", "$100K", "$150K", "$200K")) +
    labs(title="Faculty Salary by Rank and Sex", x="", y="")
#主题
    mytheme <- theme(plot.title=element_text(face="bold.italic",
                    size="14", color="brown"),
                    axis.title=element_text(face="bold.italic",
                    size=10, color="brown"),
                    axis.text=element_text(face="bold", size=9,
                    color="darkblue"),
                    panel.background=element_rect(fill="white",
                    color="darkblue"),
                    panel.grid.major.y=element_line(color="grey",
                    linetype=1),
                    panel.grid.minor.y=element_line(color="grey",
                    linetype=2),
                    panel.grid.minor.x=element_blank(),
                    legend.position="top")
#保存图片
    myplot <- ggplot(data=mtcars, aes(x=mpg)) + geom_histogram()
    ggsave(file="mygraph.png", plot=myplot, width=5, height=4) 



