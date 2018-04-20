##' Output the positions of equation in a plot region
##' 
##' The function could be used as calcuating the positions of equations in a
##' plot region.
##' 
##' 
##' @param no the number of equations will be shown onto the plot region.
##' @param col.n the columns of equations will be shown, which the order is
##' from top to down and from left to right.
##' @author Guofang Liu \email{liugf@ibcas.ac.cn}
##' @examples
##' 
##' 
##' eqt.pos(no=50, column=4)
##' 
##' @export eqt.pos
eqt.pos <- function(no = 50,col.n = 3){
	row.no <- if(no%%col.n) {
				column.len <- rep(no%/%col.n+1,col.n-1) 
				c(column.len,no-sum(column.len))} else 
				rep(no%/%col.n,col.n)
	plyr::ldply(1:col.n,function(i) 
				data.frame(x=i,y=(max(row.no[1]):1)[1:row.no[i]])) 
}
