#' Method for correlation transformation
#'
#' cor.trsf function will do correlation transformation for given vector
#'
#' @param x is given vector
#' @return cor. trsfed vector
#' @examples
#' x=c(1,2,3)
#' cor.trsf(x)
#' @export
cor.trsf=function(x){
	xstar=sqrt(1/(length(x)-1)) * ((x-mean(x))/sqrt(var(x)))
}
