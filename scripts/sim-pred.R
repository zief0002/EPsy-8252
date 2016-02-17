###################################################
### Function to predict after accounting for sampling and prediction uncertainty
###################################################

yhat = function(x, data = d){
  n = nrow(data)
  k = 2
  rmse = 7.23961 * sqrt((n - k) / rchisq(n = 1, df = (n - k)))
  vc_sim = as.numeric(rmse ^ 2) * Vb 
  sampled_b = mvrnorm(n = 1, b, vc_sim)
  e = rnorm(n = 1, mean = 0, sd = rmse)
  sampled_b[1] + sampled_b[2] * x + e
}


       