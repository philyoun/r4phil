#' Check for Brown-Forsythe test
#'
#' bf.test will give Brown-Forsythe t-statistic and its p-value.
#'
#' @param x is a set of vector.
#' @param y is another set of vector.
#' @return Brown-Forsythe t-statistic and its p-value.
#' @examples
#' x=rnorm(100); y=rnorm(50); # Two set of vectors are generated from same normal distribution with mean=0, sd=1
#' bf.test(x,y)
#' @export
bf.test=function(x,y){
	DNAME = c(deparse(substitute(x))," and ",deparse(substitute(y)))
	median1 = median(x); median2 = median(y)
	d1 = abs(x-median1); d2 = abs(y-median2)
	d1_bar = mean(d1); d2_bar = mean(d2)
	n1 = length(x); n2 = length(y); n = n1+n2
	s.squared = ( sum( ( d1 - d1_bar )^2 ) +sum( ( d2 - d2_bar )^2 ) ) / (n-2)
	t_stat = ( d1_bar-d2_bar ) / ( sqrt(s.squared)*sqrt((1/n1)+(1/n2)) )
	b=list(statistic = c(BF=t_stat), p.value=2*(1-pt(abs(t_stat),n-2)),
		 method="Brown-Forsythe test",data.name=DNAME)
	class(b) = "htest"
	return(b)
}
