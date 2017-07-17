#' Model validation at a glance
#'
#' mod.valid function will provide internal&external statistics for model validation.
#'
#' @param a is a entire set of indep variables in model-building set.
#' @param b is a response variable in model-building set.
#' @param c is a entire set of indep variables in validation set.
#' @param d is a response variable in validation set
#' @param e is a number of column(s) you want to put in the model. ex) lm(y~x1+x2+x3+x8) is what you want, e=c(1,2,3,8)
#'
#' @return a matrix with coefficients, p, Mallow's Cp, R.squared, R.adj.squared, SSE, PRESS, MSE, MSPR, AIC and BIC
#' @examples
#' library(ALSM)
#' mod.valid(SurgicalUnit[,1:8],SurgicalUnit[,10],SurgicalUnitAdditional[,1:8],SurgicalUnitAdditional[,10],c(1,2,3,8))
#' @export

mod.valid=function(a,b,c,d,e){
  df=data.frame(a[e],b); dff=data.frame(a[1:max(e)],b)
  asdf=lm(b ~ ., data=df)				      # model what you want
  qwer=lm(b ~ ., data=dff)	        	# full model in order to get Cp
	mat=round(coef(summary(asdf))[,c(-3,-4)],4); mat2=matrix(0,5,2)
	mat=rbind(mat,mat2); mat			      # matrix for coefficients and others(model-building)
	n=nrow(anova(asdf)); m=nrow(anova(qwer))
	nn=length(b)					              # To get size of sample size
	p=asdf$rank						              # To get parameters p
	cp=anova(asdf)$Sum[n] / (anova(qwer)$Mean[m]) - (nn-2*p); cp=round(cp,4)
	mat[p+1,1]=p; mat[p+1,2]=cp			    # adding p and Cp
	rp=summary(asdf)$r.squared; rap=summary(asdf)$adj.r.squared; rp=round(rp,4); rap=round(rap,4)
	mat[p+2,1]=rp; mat[p+2,2]=rap		  	# adding  Rp2 and Rap2
	sse=anova(asdf)$Sum[n]; pre=MPV::PRESS(asdf); sse=round(sse,4); pre=round(pre,4)
	mat[p+3,1]=sse; mat[p+3,2]=pre		  # adding SSE and PRESS
	preds=data.frame(c[e]); predd=predict(asdf,newdata=preds)
	mspr=sum((d-predd)^2) / length(d); mse=anova(asdf)$Mean[n]; mspr=round(mspr,4); mse=round(mse,4)
	mat[p+4,1]=mse; mat[p+4,2]=mspr		  # adding MSE and MSPR
	aic=nn*log(anova(asdf)$Sum[n]) - nn*log(nn) + 2*p; aic=round(aic,4)
	bic=nn*log(anova(asdf)$Sum[n]) - nn*log(nn) + log(nn)*p; bic=round(bic,4)
	mat[p+5,1]=aic; mat[p+5,2]=bic		  # adding AIC and BIC
	rownames(mat)[p+1]="p&Cp"; rownames(mat)[p+2]="Rp.sq&Rap.sq"
	rownames(mat)[p+3]="SSE&PRESS"; rownames(mat)[p+4]="MSE&MSPR"; rownames(mat)[p+5]="AIC&BIC"

	df2=data.frame(c[e],d); dff2=data.frame(c[1:max(e)],d)
	asdf2=lm(d ~ ., data=df2)
	qwer2=lm(d ~ ., data=dff2)
	matt=round(coef(summary(asdf2))[,c(-3,-4)],4); matt2=matrix(0,5,2)
	matt=rbind(matt,matt2); matt		  	  # matrix for coefficients and others(validation)
	n2=nrow(anova(asdf2)); m2=nrow(anova(qwer2))
	nn2=length(d)					                 # To get size of sample size
	p2=asdf$rank					                 # To get parameters p
	cp2=anova(asdf2)$Sum[n2] / (anova(qwer2)$Mean[m2]) - (nn2-2*p2); cp2=round(cp2,4)
	matt[p2+1,1]=p2; matt[p2+1,2]=cp2	    	# adding p and Cp
	rp2=summary(asdf2)$r.squared; rap2=summary(asdf2)$adj.r.squared; rp2=round(rp2,4); rap2=round(rap2,4)
	matt[p2+2,1]=rp2; matt[p2+2,2]=rap2	  	# adding  Rp2 and Rap2
	sse2=anova(asdf2)$Sum[n]; pre2=MPV::PRESS(asdf2); sse2=round(sse2,4); pre2=round(pre2,4)
	matt[p2+3,1]=sse2; matt[p2+3,2]=pre2	  # adding SSE and PRESS
	mse2=anova(asdf2)$Mean[n]; mse2=round(mse2,4)
	matt[p2+4,1]=mse2; matt[p2+4,2]=NA		  # adding MSE and MSPR, in this case MSPR=0
	aic2=nn2*log(anova(asdf2)$Sum[n2]) - nn2*log(nn2) + 2*p2; aic2=round(aic2,4)
	bic2=nn2*log(anova(asdf2)$Sum[n2]) - nn2*log(nn2) + log(nn2)*p2; bic2=round(bic2,4)
	matt[p2+5,1]=aic2; matt[p2+5,2]=bic2	  # adding AIC and BIC
	mat=cbind(mat,matt); colnames(mat)=c("Estimate","Std.Error","Val.Estimate","Val.Std.Error")
	print(mat)
}
