# nik_class_notes
ISLR

```
#' forward selection
#' how to pick number of pc
#' cross validation mse
#' without cv: see how much variation is explained, then take 95%?...
#' generalized partial least squares
#' 
#' step 1 recenter, rescale
#' 
#' 
#' could you explain why you don't want intercept in lasso 
#' 
#' f(x0)+c(-2,2)*se
#' 95% 1.96
#' 2 correlation between variables
#' 
#' 
#' polynomial regression unstable, particularly in the tails
#' step function, pick jumps, discontinuous
#' 
#' smoothing spline
#' df_lambda=tr(S)
#' Note: in linear model tr(H)=tr((XtX)-1(XtX))=tr(I)=p
#' 
#' rpackage: mgcv for generalized additive model
#' better than package GAM
```


```
#' mgcv package for cross validation
#' How to pick knots: 100 knots equally spaced wrt percentiles ->
#' a lot of columns ->
#' forward selection
#' 
#' regression tree: single step ahead optimality
#' bagging, boosting, random forests
```

```
set.seed(0)
library(ISLR)
data("Wage")
maxcut<-20
cvlabel<-sample(as.numeric(cut(1:nrow(Wage),5)))
ncutcv<-rep(0,maxcut)
for(ncut in 2:maxcut){
  for(leave in 1:10){
    stepfit<-lm(wage~cut(age,ncut),data=Wage[cvlabel!=leave,])
    pred<-predict(stepfit,newdata=Wage[cvlabel==leave,c("wage","age")])
    ncutcv[ncut]<-ncutcv[ncut]+sum((pred-Wage$wage[cvlabel==leave])^2)
  }
}
ncut<-which.min(ncutcv)
deg
stepfit<-lm(wage~step(age,deg),data=Wage)
od<-order(Wage$age)
plot(wage~age,data=Wage,col="coral")
lines(Wage$age[od],stepfit$fitted.values[od])


#' Splines
#' 
#' ## B-spline
#' library(splines)
#' bs(age,knots=c(25,40,60))
#' bs(age,df=6)
#' 
#' ## smoothing spline
#' smooth.spline(age,wage,df=16)
#'
#' ## natural splines
#' ns(year ,4)
#' 
#' 

library(gam)
summary(gam(Outstate ~ Private + s(Room.Board, df = 2) + s(PhD, df = 2) + s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data=College))




selectedvars<-names(coef(regfit,which.min(regsummary$bic)))[-1]
paste(paste("s(",selectedvars,",df=2)",sep=""),collapse="+")

names(College)
names(regsummary)

```
