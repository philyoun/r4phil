#' Check for Breusch-Pagan test
#'
#' bp.test will give Breusch-Pagan chi.squared-statistic and its p-value.
#'
#' @param df is a data.frame which has two columns of x and y
#' @param x is a independent variable.
#' @param y is a response variable.
#' @return Breusch-Pagan chi.squared-statistic(df=1) and its p-value.
#' @examples
#' library(ALSM); TolucaCompany
#' bp.test(TolucaCompany,x,y)
#'
#' @export
bp.test=function(df,x,y){
	DNAME = deparse(substitute(df))
	lm1=lm(y~x, data=df); lm2=lm((residuals(lm1))^2~x, data=df)
	SSR_star=anova(lm2)$Sum[1]; SSE=anova(lm1)$Sum[2]; n=length(y)
	chi_stat = (SSR_star/2) / (SSE/n)^2
	b=list(statistic = c(Chi_BP=chi_stat), p.value = 1-pchisq(chi_stat,1),
		method="Breusch-Pagan Test", data.name=DNAME)
	class(b) = "htest"
	return(b)
}
