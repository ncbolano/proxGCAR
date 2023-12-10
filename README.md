---
"proxGCAR"
---
Maximum Likelihood Estimation for a Gaussian Conditionally Autoregressive Model

Regional data often follows spatial trends, as correlations between areas tend to be stronger when closer in proximity. A classical approach to analyzing data is through construction of a proximity/(adjacency) matrix. The standard method is the neighboring proximity matrix, where we set the proximity matrix's i,jth entry = 1 if region I and J share a boundary, and 0 otherwise. A variant on the idea, which will be used is to standardize W with its row sum.

The conditional distribution of our model is as follows :

``` math
Y_i | \{ Y_j \}_{j \neq i} \sim N(\rho \sum_{j=1}^{n} \tilde{w}_{i,j} Y_j, \tau^2_i)
```

The joint distribution of our model in the context of our problem is :

``` math 
Y \sim \text{MVN}\left( \mu_{1n}, \left( \mathbf{I}_n - \rho \tilde{W} \right)^{-1} {\tau^2 \text{diag}\left( w^{-1}_1, +, w^{-1}_2, +, \ldots, w^{-1}_n, + \right)} \right)
```
Thus , the parameters we seek to estimate for the models are as follows : 
Mu, the common mean term for the joint distribution.
Rho(p): a scalar value that represents the strength of conditional dependency on adjacent regions.
Tau: A scalar value representing the square root of the conditional model variance * the sum of the corresponding row of the proximity matrix. (Shown below)

``` math
\sqrt{\tau^2_i \cdot w_{i,+}}
```

This package conducts maximum likelihood estimation to estimate the three parameters (mu , rho , tau) of a Gaussian CAR model. Provided with an Nx1 vector of observations, and a valid proximity matrix corresponding to the regional structure of the data, it computes the Least Squared estimators. Then, these LS estimators are utilized as warm starting values to iterate through our negative likelihood function and find optimal MLE parameters. Finally, it analyzes the inverse hessian to relay the variance of each of our MLE's. proxGCAR returns a 3x3 matrix of the LS estimators , MLE's , and corresponding MLE variances.

Example Code: Running Maximum_Likelihood after generating data that truly follows a Gaussian CAR with a valid proximity matrix (Requires install of package mvtnorm for data generation)

```r
install.packages("mvtnorm")
```

```r
library(mvtnorm)

# Creating a proximity matrix that only has dependencies on stats i +,- 1 away
n = 100
proximity = matrix(0, nrow = n, ncol = n)
for (i in 1:n) {
  proximity[i, i] = 0
  
  if (i > 1) {
    proximity[i, i - 1] = 1
  }
  
  if (i < n) {
    proximity[i, i + 1] = 1
  }
}

# B Standardizing that matrix
Proximity_standardize = function(proximity) {
  # Obtains the rowSums of the matrix and saves them as a vector
  rowSums_vector = rowSums(proximity)
  
  # Used element wise division to efficiently compute this standardized matrix
  proximity = (proximity / rowSums_vector)
  return(proximity)
}

Proximity_std = Proximity_standardize(proximity)


# C Choosing mu , p and t (Can be changed)
mu = 50
p = .8
tau = 5


# D Finding D matrix
rowSums_vector = rowSums(proximity)
D = diag(1/rowSums_vector)


# E Finding (In - p * Prox_std)^-1
I = diag(nrow(Proximity_std))
term = I - (p * Proximity_std)
inv_term = solve(I - (p * Proximity_std))


# F finding Sigma
sigma = tau^2 * inv_term %*% D

# E Generating data
Y = rmvnorm(1, mean = rep(mu, nrow(sigma)), sigma)
Y = t(Y)

# Now that we have a valid proximity matrix and a Y vector, we can run our function

Maximum_Likelihood(Y,proximity)
```

For any further questions email Noah Bolanos (ncbolano@tamu.edu) .
