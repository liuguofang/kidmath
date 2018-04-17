\name{plot_kid_eqt}
\alias{plot_kid_eqt}
\title{Output addition/minus or multiplication/division equations excise for primary students}
\usage{
plot_kid_eqt(equation, equation.layout = 1,no=50, col.n=3, page.no=NULL,main='小学生数学练习题',
	cex=1,width=NULL)}
\description{
The function could be used as producing excise on addition and minus equations for primary students. 
}
\arguments{
	\item{equation}{a array denoting addition/minus formulae.}
	\item{equation.layout}{the one or two blocks on 50 eauations.}
	\item{no}{the default is 50 equation.}
	\item{col.n}{Default is three columns.}
	\item{page.no}{add a page number at the bottom of pages.}
	\item{main}{a title of the taskment. }
	\item{cex}{change the font size of equations. }
	\item{width}{the width of several columns based on different lengths of equations. }
}
\author{ Guofang Liu \email{liugf@ibcas.ac.cn}}

\examples{
# 
# # 加减运算 between 10 and 100
# library(dplyr)
# add.minus <- expand.grid(x=10:100,y=10:100)
# add.eqt <- with(add.minus[between(apply(add.minus,1,sum),10,100),],paste0(x,'+',y,"="))
# minus.eqt <- with(add.minus[between(apply(add.minus,1,diff),10,100),],paste0(y,'-',x,"="))
# equation1 <- sample(c(add.eqt,minus.eqt))
# 
# 
# # 乘除运算 2到9
# x <- expand.grid(2:9,2:9)
# x <- unique(cbind(x,sum=x[,1]*x[,2]))
# equation2 <- sample(c(sprintf("%s×%s= ",x[,1],x[,2]),sprintf("%s÷%s= ",x[,3],x[,2])))
# 
# ## 乘除含大于小于
# x <- expand.grid(2:9,2:9)
# x <- unique(cbind(x,sum=x[,1]*x[,2]))
# library(plyr)
# x <- expand.grid(diff=seq(-5,5),sum=x$sum) %>% join(x)
# x <-subset(mutate(x,sum2=diff+sum),sum2>3&sum2%/%x[,3]<10) 
# 
# equation3 <- c(sprintf("%s×%s (  ) %s",x[,3],x[,4],x[,5]),
#                sprintf("%s÷%s (  ) %s",x[,5],x[,3],x[,4]))
# 
# # 除法含余数
# d <- expand.grid(10:89,2:9)
# d2 <- mutate(d,yu=Var1%%Var2,value=Var1%/%Var2)
# d2 <- subset(d2,yu!=0&value<10)
# equation4 <- sprintf("%s÷%s = (  )......(  )",d2$Var1,d2$Var2)
# 
# 
# plot_kid_eqt(string[1:50],col.n=3,main='小学二年级（下）数学练习题',cex=1.8,width=c(0.9,1,1.2))
# 
# # 生成pdf文件
# for(i in 1:20){	
#   cairo_pdf(sprintf("加减乘除(含余)大于小于50道(20页)(%s)(%s).pdf",i,Sys.Date()),
#             width=8.267717,height=11.69291,family='GB1')
#   
#   string <- c(sample(equation1,13),sample(equation2,13),
#               sample(equation3,12),sample(equation4,12))
#   
#   plot_kid_eqt(string[1:50],col.n=3,main='小学二年级（下）数学练习题',cex=1.8,width=c(0.9,1,1.2))
#   
#   dev.off()
# }	


}
