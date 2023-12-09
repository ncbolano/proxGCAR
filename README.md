---
title: "proxGCAR"
---
Maximum Likelihood Estimation for a Gaussian Conditionally Autoregressive Model

Regional data often follows spatial trends, as correlations between areas tend to be stronger when closer in proximity. A classical approach to analyzing data is through construction of a proximity/(adjacency) matrix. The standard method is the neighboring proximity matrix, where we set wi,j = 1 if Ai and Aj share a common boundary, else let wi,j = 0. A variant on the idea, which will be used in the context of this project is to standardize W with its row sum.

The distribution of an Gaussian CAR in the context of our problem is 

``` math 
Y \sim \text{MVN}\left( \mu_{1n}, \left( \mathbf{I}_n - \rho \tilde{W} \right)^{-1} {\tau^2 \text{diag}\left( w^{-1}_1, +, w^{-1}_2, +, \ldots, w^{-1}_n, + \right)} \right)
```

This package conducts maximum likelihood estimation to estimate the three parameters (mu , rho , tau) of a Gaussian CAR model. Provided with an Nx1 vector of observations, and a valid proximity matrix corresponding to the regional structure of the data, it computes the Least Squared estimators. Then, these LS estimators are utilized as warm starting values to iterate through our negative likelihood function and find optimal MLE parameters. Finally, it analyzes the inverse hessian to relay the variance of each of our MLE's. proxGCAR returns a 3x3 matrix of the LS estimators , MLE's , and corresponding MLE variances. 



```r
plot(cars)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
