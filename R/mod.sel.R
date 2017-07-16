#' Provide a data.frame of useful statistics for some models
#'
#' This data.frame can be found in Applied Linear Regression Models, Kutner, Chapter 9. Criteria for model selection
#'
#' @param x is a set of independent variables.
#' @param y is a response variable
#' @param z is for nbest in regsubsets. Try several numbers, from 1 to choose(ncol(x),1).
#' @return It provides p(number of parameters), SSE, R.squared, R.adj.squared, AIC, BIC and PRESS
#' @examples
#' library(ALSM)
#' mod.sel(SurgicalUnit[,1:4],SurgicalUnit[,10],1)
#' mod.sel(SurgicalUnit[,1:6],SurgicalUnit[,10],2)
#' mod.sel(BodyFat[,1:3],BodyFat[,4],3)
#' @export
mod.sel=function(x,y,z){
	asdf=leaps::regsubsets(x,y,nbest=z) # x should be matrix with indep. variables in column
	mod=summary(asdf)$which
	cp=summary(asdf)$cp; cp=round(cp,3)
	p=c(); sse=c(); rp=c(); rap=c(); aic=c(); bic=c(); pre=c()
	for(i in 1:nrow(summary(asdf)$which)){
		w=matrix(0,ncol(x),nrow(x))
		a=rep(0,ncol(x))
				for(j in 1:ncol(x)) {
				w[j,]=x[,j]
				if(summary(asdf)$which[i,j+1]==TRUE) a[j]=1
				}
		a*w
		qwer=lm(y~as.matrix(t(a*w)))
		n=nrow(qwer$model); pp=qwer$rank
		p[i]=qwer$rank; sse[i]=anova(qwer)$Sum[nrow(anova(qwer))]; rp[i]=summary(qwer)$r.squared;
		rap[i]=summary(qwer)$adj.r.squared; aic[i]=n*log(anova(qwer)$Sum[nrow(anova(qwer))]) - n*log(n) + 2*pp
		bic[i]=n*log(anova(qwer)$Sum[nrow(anova(qwer))]) - n*log(n) + log(n)*pp; pre[i]=MPV::PRESS(qwer)
	}
	sse=round(sse,3); rp=round(rp,3); rap=round(rap,3); aic=round(aic,3); bic=round(bic,3); pre=round(pre,3)
	data.frame(summary(asdf)$which[,-1],p=p,SSE=sse,Rp=rp,
		Rap=rap,Cp=round(summary(asdf)$cp,3),AIC=aic,BIC=bic,PRESS=pre)
}
