Homework 3
================
Group 8
4/28/21

# Setup

For the lecture I used the following code to generate data:

``` r
n = 50
set.seed(101)
x1 = runif(n)
x2 = runif(n)
prob  = ifelse(x1 < 0.5 & x1 > 0.25 & x2 > 0.25 & x2<0.75,0.8,0.3)
y  = as.factor(rbinom(n,1,prob))
levels(y) = c("1","2")
df = data.frame(y=y,x1=x1,x2=x2)
```

And then I used the following function to make KNN predictions:

``` r
knn_pred = function(point,x1,x2,y,k=5, thresh = 0.5) {
  dists = sqrt((x1-point[1])^2+(x2-point[2])^2) #Find all distances to current obs
  bound = sort(dists)[k]                #Find kth smallest distance
  indices = which(dists <= bound)       #Find which obs have dists 1:k
  outcomes = as.integer(y[indices]) #Find corresponding outcomes y
  prob = mean(outcomes)-1
  if (prob < thresh){
    out = 1
  } else {
    out = 2
  }
  out
  
}
```

This code builds a grid of points, and then makes predictions for each
of those points.

``` r
grid.fineness = 201
sequence = seq(0,1,length.out=grid.fineness)
grid = expand.grid(sequence,sequence)
colnames(grid) = c("x1","x2")
yhat = apply(grid,1,knn_pred,x1=x1,x2=x2,y=y,k=5)
yhat = as.factor(yhat)
```

With those predictions, we can build a dataframe, and plot.

``` r
df = as.data.frame(grid)
df$y = yhat
ggplot(df,aes(x=x1,y=x2,col=y))+geom_point(size=0.08)
```

![](Powell_HW4_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

If we drop the round and subtract 1 in our knn\_pred function, we can
get probabilities out.

``` r
knn_prob = function(point,x1,x2,y,k=5) {
  dists = sqrt((x1-point[1])^2+(x2-point[2])^2) #Find all distances to current obs
  bound = sort(dists)[k]                #Find kth smallest distance
  indices = which(dists <= bound)       #Find which obs have dists 1:k
  outcomes = as.integer(y[indices])     #Find corresponding outcomes y
  mean(outcomes)-1 #Taking advantage of 2 outcomes.
}
```

We can predict those probabilities at each point:

``` r
phat = apply(grid,1,knn_prob,x1=x1,x2=x2,y=y,k=5)
df$phat = phat
ggplot(df,aes(x=x1,y=x2,col=phat))+geom_point(size=0.08)
```

![](Powell_HW4_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

This is, in essence, the beginnings of a simulation study. We generated
data, and we can look at how our predictions perform. We can do this
with either the classifications or the underlying probabilities.

# Questions

We are going to extend this simulation study in a few ways.

## Q1 - Bigger Sample

Resetting the seed with:

``` r
set.seed(101)
```

Run the same data generation code, but with a sample size of 1000. Plot
the resulting probabilities when we use *K* = 1, *K* = 5, and *K* = 25.

``` r
n = 1000
set.seed(101)
x1 = runif(n)
x2 = runif(n)
prob  = ifelse(x1 < 0.5 & x1 > 0.25 & x2 > 0.25 & x2<0.75,0.8,0.3)
y  = as.factor(rbinom(n,1,prob))
levels(y) = c("1","2")
bigdf = data.frame(y=y,x1=x1,x2=x2)
grid.fineness = 201
sequence = seq(0,1,length.out=grid.fineness)
grid = expand.grid(sequence,sequence)
colnames(grid) = c("x1","x2")
# yhat = apply(grid,1,knn_pred,x1=x1,x2=x2,y=y,k=1)
# yhat = as.factor(yhat)
bigdf = as.data.frame(grid)

# Create probabilities for each K value
bigdf$phat1 = apply(grid,1,knn_prob,x1=x1,x2=x2,y=y,k=1)
bigdf$phat5 = apply(grid,1,knn_prob,x1=x1,x2=x2,y=y,k=5)
bigdf$phat25 = apply(grid,1,knn_prob,x1=x1,x2=x2,y=y,k=25)
```

``` r
ggplot(bigdf,aes(x=x1,y=x2,col=phat1))+geom_point(size=0.08)+
  labs(title = 'K=1')
```

![](Powell_HW4_files/figure-gfm/k1-1.png)<!-- -->

``` r
ggplot(bigdf,aes(x=x1,y=x2,col=phat5))+geom_point(size=0.08)+
  labs(title = 'K=5')
```

![](Powell_HW4_files/figure-gfm/k1-2.png)<!-- -->

``` r
ggplot(bigdf,aes(x=x1,y=x2,col=phat25))+geom_point(size=0.08)+
  labs(title = 'K=25')
```

![](Powell_HW4_files/figure-gfm/k1-3.png)<!-- -->

Plot the classification *predictions* when *K* = 10, using a probability
threshold of 0.2 for our predictions instead of the standard 0.5.

``` r
yhat10 = apply(grid,1,knn_pred,x1=x1,x2=x2,y=y,k=10, thresh = 0.2) %>% as.factor()
bigdf$yhat10 = yhat10
ggplot(df,aes(x=x1,y=x2,col=yhat10))+geom_point(size=0.08) +
  labs(title = 'K=10',subtitle = 'Probability threshold of 0.2')
```

![](Powell_HW4_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

## Q2 – Logit Comparison

Fit an interacted logit to this data. (i.e. model
*Y* ∼ *x*1 + *x*2 + *x*1 : *x*2 – using `glm`, not a LASSO). Find the
predicted probabilities for every point in our grid, and plot those
predicted probabilities.

``` r
gen_dat <- data.frame(y=y,x1=x1,x2=x2)
mymod <- glm(y ~ x1 + x2 + x1*x2, data = gen_dat, family = 'binomial')
preds <- predict.glm(mymod, grid, type = 'response')
ggplot(df, aes(x=x1,y=x2,col=preds))+ geom_point(size = 0.8)
```

![](Powell_HW4_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

## Q3 – ROC

Plot the (in-sample) ROC curves for both the logit model and the KNN
with *K* = 10. (hint: I have a function for doing this, given outcomes
and probabilities in the lecture)

Which of these models looks better?

## Q4 – Survey

Please complete the surveys posted last week (and in announcements on
canvas). They will help me ensure the last 5 weeks of class are as
useful to you as possible.

# Optional

1.  Plot the resulting predictions from a *k* = 30 KNN for Q4.
2.  Find OOS ROC curves in Q2 – build a holdout, then use predictions on
    it. 3. Do K-fold cross validation for the OOS ROC curves
3.  Add squared terms to the logit, remake predictions and plots
    (e.g. `y~x1+x1^2+x2+x2^2+x1:x2)`

## Optional Long Q

We have a new output with three categories.

``` r
# x1,x2,y from your sample with 1k observations need to exist to run this.
set.seed(101)
z = ifelse((x1>0.8 | x2 < 0.4),rbinom(length(y),1,0.8),(y==1)*2)
```

Modify `knn_pred` so that it predicts the most likely category out of 3
categories (hint: the functions `table`, `which.max`, `names`, and
`as.integer` are how I did this) (hint2: maybe start by building a
function that takes a vector and finds the most common element, then fit
it into the rest of this). Plot the grid-predictions with this new
classifier.
