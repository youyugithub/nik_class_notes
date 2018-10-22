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
