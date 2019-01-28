\name{plot_kid_eqt}
\alias{plot_kid_eqt}
\title{Output addition/minus or multiplication/division equations excise for primary students}
\usage{
plot_kid_eqt(equation, equation.layout = 1,no=50, col.n=3, page.no=NULL,main="A 50-math-formulas excise for kids",
	subtitle= c("Class","Name","Time","Date"),cex=1,width=NULL)}
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
	\item{subtitle} {rename subtitle according to this argument.}
	\item{cex}{change the font size of equations. }
	\item{width}{the width of several columns based on different lengths of equations. }
}
\author{ Guofang Liu \email{liugf@ibcas.ac.cn}}

\examples{

## # addition/minus between 10 and 100
## library(dplyr)
## add.minus <- expand.grid(x=10:100,y=10:100)
## add.eqt <- with(add.minus[between(apply(add.minus,1,sum),10,100),],paste0(x,'+',y,"="))
## minus.eqt <- with(add.minus[between(apply(add.minus,1,diff),10,100),],paste0(y,'-',x,"="))
## equation1 <- sample(c(add.eqt,minus.eqt))
## 
## multiplication and division operation for kids
## x <- expand.grid(2:9,2:9)
## x <- unique(cbind(x,sum=x[,1]*x[,2]))
## equation2 <- sample(c(sprintf("%s×%s= ",x[,1],x[,2]),sprintf("%s÷%s= ",x[,3],x[,2])))
## 
## #math: over and less operations for kids
## x <- expand.grid(2:9,2:9)
## x <- unique(cbind(x,sum=x[,1]*x[,2]))
## library(plyr)
## x <- expand.grid(diff=seq(-5,5),sum=x$sum) %>% join(x)
## x <-subset(mutate(x,sum2=diff+sum),sum2>3&sum2%/%x[,3]<10) 
## 	 	 	
## equation3 <- c(sprintf("%s×%s (  ) %s",x[,3],x[,4],x[,5]),
## 				sprintf("%s÷%s (  ) %s",x[,5],x[,3],x[,4]))
## 
## 
## d <- expand.grid(10:89,2:9)
## d2 <- mutate(d,yu=Var1%%Var2,value=Var1%/%Var2)
## d2 <- subset(d2,yu!=0&value<10)
## equation4 <- sprintf("%s÷%s = (  )......(  )",d2$Var1,d2$Var2)
## 
## 
## string <- c(sample(equation1,13),sample(equation2,13),
## 		sample(equation3,12),sample(equation4,12))
## plot_kid_eqt(string[1:50],col.n=3,cex=1.8,width=c(0.9,1,1.2))
## 
## 
## # produce a 100-pages pdf file.
## cairo_pdf("小学生加减乘除运算50道（100页）.pdf",
##    width=8.267717,height=11.69291,family='GB1',onefile=TRUE)
##   
## for(i in 1:100){ 
## 
## 	string <- c(sample(equation1,13),sample(equation2,13),
## 			sample(equation3,12),sample(equation4,12))
## 	
##  plot_kid_eqt(string[1:50],col.n=3,cex=1.8,width=c(0.9,1,1.2),main="小学生加减乘除运算50道",
##        subtitle=c("班级","姓名","时间","日期"))
## } 
##  dev.off()


}
