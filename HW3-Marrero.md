Homework 3
================
Group 8
4/21/21

# Data

In this assignment, we examine the use of a *stepwise regression* and a
*LASSO* for model selection with *AIC and cross-validation*. We are
using data on home sales (used in a previous assignment). The data
includes a number of variables about house price/mortgage, number of
bedrooms/bathrooms, and more. See this
[Codebook](https://codowd.com/bigdata/hw/hw2/homes2004code.txt) for
interpretation of all variables in the dataset.

## Tidy Data

First, we will load and tidy the data. This involves converting all
variables with Y/N responses to binary dummy variables. Additionally, we
will create a *dummy variable indicating if the number of bedrooms and
bathrooms for a house are both at least two*, denoted as `twotwo`. We
will be attempting to predict this variable later in the assignment, but
it will be used as a predictor while we are still predicting
log(LPRICE).

``` r
# Load data
homes <- read_csv("https://codowd.com/bigdata/hw/hw2/homes2004.csv")

#Make a function that tells us if there are two levels -- Y and N
temp_fn <- function(x) is.character(x) & all(levels(as.factor(x)) %in% c("Y","N"))
# Make a function that takes those levels and makes a logical variable
temp_fn2 <- function(x) x == "Y"
# Assign levels to other variables for LASSO
na_relevel <- function(x) factor(x,levels=c(NA,levels(x)),exclude=NULL)

homes <- homes %>% 
  mutate(
    # For two-level variables, convert to logical
    across(where(temp_fn),temp_fn2),
    # Convert other character vars to factors
    across(where(is.character),as.factor),
    # Re-level for LASSO 
    across(where(is.factor),na_relevel),
    # Add dummy predictor for classification model
    twotwo = BATHS>=2 & BEDRMS >=2
  )
```

# Homework Questions

## Q1 – Stepwise Regression: AIC & BIC

In our first model (Model 1), we will use a forward step-wise linear
regression (with AIC-based steps) to predict `log(LPRICE)` in the data.

``` r
empty <- glm(log(LPRICE) ~ 1, data=homes) # model w/ no coeff
all <- glm(log(LPRICE) ~ ., data = homes) # model w/ all coeff

# stepwise regression using empty and full models
forward_aic <- step(empty, scope=formula(all), 
                    direction="forward",trace=0) # k=2, AIC

# get num coefficients and aic of resulting model
mod1_coeff <- length(forward_aic$coefficients)
mod1_aic <- forward_aic$aic
```

The AIC of the final regression is 3.0290064^{4}. This model has 41
coefficients, including the intercept.

Our second model (Model 2) will use the same forward stepwise linear
regression, but use BIC based steps.

``` r
# stepwise regression using empty and full models
forward_bic <- stats::step(empty, scope=formula(all), 
                    direction="forward",trace=0, k = log(nrow(homes))) # k=log(n), BIC

# get num coefficients and bic of resulting model
mod2_coeff <- length(forward_bic$coefficients)
mod2_aic <- forward_bic$aic
```

The AIC of the final regression is 3.0318007^{4}. This model has 32
coefficients, including the intercept.

### Model Differences

In each ‘step’ of building the model, the best new variable is added
until it no longer improves the fit criteria, which differ in models 1 &
2–AIC approximates the relative deviance from a true model, whereas the
BIC approximates the model truth. This results in the models having a
differing number of coefficients (BIC being smaller).

## Q2 – BASIC Cross-Validated LASSO

### Cross-Validated LASSO for predicting log(LPRICE)

Now, we will use a cross-validated LASSO to predict `log(LPRICE)` and
plot the deviance (in this case, MSE).

``` r
xmatrix <- sparse.model.matrix(log(LPRICE)~ .,data=homes)

cv_mod <- cv.glmnet(xmatrix, log(homes$LPRICE), type.measure="deviance")
plot(cv_mod) # this is probably the right graph? 
```

![](HW3-Marrero_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
if(ncol(xmatrix) != ncol(homes)){
  cat("There are",ncol(xmatrix),"possible subsets of the variables in our data")
}
```

    ## There are 50 possible subsets of the variables in our data

### Lamdas

From this model, we will now find the *λ* that minimizes OOS deviance
(hint: see `cv.glmnet` help page) and the deviance at that *λ* (no need
to report either). What is the number of non-zero coefficients with that
*λ*<sub>*m**i**n*</sub>?

``` r
n_min_coeff<- sum(coef(cv_mod, s=cv_mod$lambda.min) != 0)
```

There are 46 non-zero coefficients with the *λ* that minimize OOS
deviance.

``` r
n_1se_coeff<- sum(coef(cv_mod, s=cv_mod$lambda.1se) != 0)
```

There are 18 non-zero coefficients with the *λ* within 1 standard error
of the minimum OOS deviance.

We know that relative to the null distribution, the pseudo
*R*<sup>2</sup> is 1 − *D*/*D*<sub>0</sub>. What we haven’t discussed is
that we could use a similar method to compare any two models (with the
same likelihoods).

``` r
# ration larger lambda
ratio_dev <- 0.4456 / 0.4112 
```

The ratio of deviance of the larger model to deviance of the smaller
model is 1.0836576. INTERPRET in two sentences or less interpret that
ratio.

## Q3 – GIANT Models

Model 5: Add interactions (warning: may be slow, hint: you’ll need to
modify/recreate `xmatrix`) and re-estimate the same cross-validation
model as in Q2. How many possible subsets are there now (or at least,
what does R say)?

``` r
xmatrix_int<- sparse.model.matrix(log(LPRICE)~(.)^2,data=homes)
cv_mod2 <- cv.glmnet(xmatrix_int, log(homes$LPRICE), type.measure="deviance")

if(ncol(xmatrix_int) != ncol(homes)){
  cat("There are",ncol(xmatrix_int),"possible subsets of the variables in our data")
}
```

    ## There are 1134 possible subsets of the variables in our data

Show the CV error plot.

``` r
plot(cv_mod2)
```

![](HW3-Marrero_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> In one
sentence, how do these OOS deviances compare to the OOS deviances in Q2?
EXPLAIN

## Q4 – LPM, Deviances, Loss

Using a straightforward linear model to predict a binary is a model
structure known as a “linear probability model” (LPM). This is because
the *E*\[*Y*\] is still *P*(*Y* = 1), and the linear model is still
optimizing for *E*\[*Y*\], but is purely linear, so we have a linear
model of the probability.

Model 6: Cross-validate a linear LASSO (no interactions or anything) to
predict `twotwo`.

``` r
xmatrix_22 <- sparse.model.matrix(twotwo ~ .,data=homes)
mod6 <- cv.glmnet(xmatrix_22, homes$twotwo)
```

DON’T THINK I DID THESE RIGHT!

Model 7: Cross-validate a logistic LASSO to predict `twotwo` (hint:
`family="binomial"`).

``` r
xmatrix_22_log <- sparse.model.matrix(twotwo ~ .,data=homes, family="binomial")
mod7 <- cv.glmnet(xmatrix_22_log, homes$twotwo,type.measure="class")
```

Which model has the smaller deviance? In at most two sentences, why
can’t we use deviances to compare Model 6 and Model 7?

Rerun model 7 but set `type.measure="mse"` in `cv.glmnet`.

``` r
mod7 <- cv.glmnet(xmatrix_22_log, homes$twotwo,type.measure="mse")
```

In at most two sentences, Can we compare these models now? and if so,
which is better?

## Q5 – Changes to Predictions

A well known issue with LPMs is that they frequently predict
probabilities that are non-sensical (E.g. they might predict that
*P*(*Y* = 1) = 110, or *P*(*Y* = 1) =  − 10). Check your predictions
from Model 6 and you’ll see that some of them are like this.

One solution to this problem is to use a link function which is
constrained to \[0, 1\] – as the logistic regression does. A different
solution is to say “we know those probabilities are nonsense, but for
prediction purposes we can easily set them to reasonable values”.
Specifically, predictions that are greater than 1, we set to 1, and
predictions that are less than 0, we set to 0. I’ve written a simple
function below that does this.

``` r
lpm_prediction_editor = function(preds) {
  out = preds
  out[out>1] = 1
  out[out<0] = 0
  out
}
```

Would using this function for our predictions with Model 6 improve the
MSE? Could this change to our predictions change the value of *λ* we
should select? Given a *λ*, could this change to our predictions change
the variables we should choose? Could it change the coefficients on
variables we already chose?

## Q6 – purely R

Changing our predictions to “certainly FALSE” and “certainly TRUE” \[*ŷ*
either 0 or 1\] as the function above does seems extreme. Rewrite the
above function so that it takes an input argument `eps_err`, and adds
`eps_err` probability of being wrong to all the predictions which claim
to be certain.

``` r
lpm_prediction_editor = function(preds, eps_err) {
  out = preds
  out[out>= 1] = 1 - eps_err # make prediction less certain from 1
  out[out<= 0] = 0 + eps_err # make prediction less certain from 0
  out
}
```
