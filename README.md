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


```

set.seed(0)
library(ISLR)
data("Wage")
maxdeg<-20
polycv<-rep(0,maxdeg)
library(boot)
for(deg in 1:maxdeg){
  polyfit<-glm(wage~poly(age,deg),data=Wage,family=gaussian())
  polycv[deg]<-cv.glm(polyfit,data=Wage,K=10)$delta[1]
}
deg<-which.min(polycv)
deg
polyfit<-glm(wage~poly(age,deg),data=Wage,family=gaussian())
od<-order(Wage$age)
plot(wage~age,data=Wage,col="coral")
lines(Wage$age[od],polyfit$fitted.values[od])


set.seed(0)
library(ISLR)
data("Wage")
maxdeg<-20
cvlabel<-sample(as.numeric(cut(1:nrow(Wage),5)))
polycv<-rep(0,maxdeg)
for(deg in 1:maxdeg){
  for(leave in 1:10){
    polyfit<-lm(wage~poly(age,deg),data=Wage[cvlabel!=leave,])
    pred<-predict(polyfit,newdata=Wage[cvlabel==leave,])
    polycv[deg]<-polycv[deg]+sum((pred-Wage$wage[cvlabel==leave])^2)
  }
}
deg<-which.min(polycv)
deg
polyfit<-glm(wage~poly(age,deg),data=Wage,family=gaussian())
od<-order(Wage$age)
plot(wage~age,data=Wage,col="coral")
lines(Wage$age[od],polyfit$fitted.values[od])













set.seed(1)
library(ISLR)
data("Wage")
maxcut<-50
cvlabel<-sample(as.numeric(cut(1:nrow(Wage),5)))
ncutcv<-rep(0,maxcut)
for(ncut in 1:maxcut){
  for(leave in 1:10){
    cutpoints<-unique(quantile(Wage$age[cvlabel!=leave],seq(0,1,length.out=ncut+1)))
    cutpoints[1]<--Inf
    cutpoints[length(cutpoints)]<-Inf
    Wagetrain<-Wage[cvlabel!=leave,c("wage","age")]
    Wagetrain$age<-cut(Wagetrain$age,cutpoints)
    if(ncut>1){
      stepfit<-lm(wage~age,data=Wagetrain)
    }else{
      stepfit<-lm(wage~1,data=Wagetrain)
    }
    Wagetest<-Wage[cvlabel==leave,c("wage","age")]
    Wagetest$age<-cut(Wagetest$age,cutpoints)
    pred<-predict(stepfit,newdata=Wagetest)
    ncutcv[ncut]<-ncutcv[ncut]+sum((pred-Wage$wage[cvlabel==leave])^2)
  }
}
ncut<-which.min(ncutcv)
ncut
stepfit<-lm(wage~cut(age,ncut),data=Wage)
od<-order(Wage$age)
plot(wage~age,data=Wage,col="coral")
lines(Wage$age[od],stepfit$fitted.values[od])
```

```
#' 6 bootstrap data sets
#'     z*(1) z*(2) z*(3) z*(4) z*(5) z*(6)
#' i=1  *           *     *     *     *
#' i=2        *           *     *
#' i=3  *           *           *     *
#' .
#' .
#' .
#' i=n  *     *                 *     *
#' 
#' Suppose i=1,note (x1,y1) is not used in trees f*(2) f*(6), 
#' Hence bag these trees and use this bag to predict y1
#' (and compute test error across aggregating)
#' Test bagging/OOB bagging
#' 
#' Out-of-Bag: observations not in the training
#' 
#' for a given i, approximately B/3 predictions will not have ith observation
#' which we average 
#' 
#' this estimate is essentially the LOO cross-validation error for bagging if B is large
#' 
#' 
#' Boosting algorithm
#' 
#' Fit a tree fb with d splits
#' take a smaller multiple (lambda) of the tree we have grown 
#' f(x)<-f(x)+lambda*fb(x)
#' 
#' gbm: gradient boosted model(machine)
#' 
#' tuning parameters for boosting: 
#' B, number of trees
#' lambda, shrinkage parameter, typically 0.01 or 0.001
#' d, number of splits in each tree
#' 
#' 
#' For each tree we record the total amount that the RSS is decreased due to splits over a given predictor
#' importance
#' For categorical, add up the total amount that the Gini index is decreased
#' 
#' Model comparison: leave one predictor out, and compare RSS/gini. This method is more computationally intensive
#' 
```


```
#' download the package source 
#' put it in your working directory on HPG2
#' Create the directory /R in your working directory
#' install by: R CMD
#' 
#' 
#' multiple jobs directory R - contains any R packages
#' directory infiles
#' direcotry outfiles
#' 
#' sbatch command with array
#' my_job_%j.out
#' 
#' sbatch --array=1-100%50 Run_job.job
#' 
#' For example "--array=0-15%4" will limit the number of simultaneously running tasks from this job array to 4.
#' 
#' 1-100 one to one hundred
#' 50 each time
#' 
#' 
#' cd /ufrc/bliznyuk/yhan2014
#' 
#' ISLR_1.2.tar.gc infiles, outfiles Template.R
#' package: ISLR_1.2.tar.ge
#' 
#' sbatch --array 
#' 
#' 
#' 
#' caret package
#' 
#' library(caretNWS)
#' 
#' 
```

```
#!/bin/sh
#SBATCH --job-name=my_job
#SBATCH --output=outfiles/my_job_%j.out
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=xxxxxxxxxxxxx
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=1gb
#SBATCH --time=1:00:00
#SBATCH --qos=xxxxxxxxxxxx
pwd;hostname;date

cat /proc/cpuinfo | head
module load gcc/5.2.0 R/3.2.2
Rscript infiles/Code_${SLURM_ARRAY_TASK_ID}.R
date
```


If you have 001,002,003,004....
https://marylou.byu.edu/wiki/index.php?page=How+do+I+submit+a+large+number+of+very+similar+jobs%3F:
```
#!/bin/bash
# submit_array.sh

#SBATCH --ntasks=1
#SBATCH --time=00:30:00
#SBATCH --mem-per-cpu=1G
#SBATCH --array=1-700

# pad the task ID with leading zeros (to get 001, 002, etc.)
CASE_NUM=`printf %03d $SLURM_ARRAY_TASK_ID`

cd $SLURM_SUBMIT_DIR/case$CASE_NUM

./runcase.sh
```
