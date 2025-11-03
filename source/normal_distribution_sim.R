#' normal_distribution_sim
#' Creates a data set of size n following a normal distribution with mu and sigma and runs a two sided t test on it
#' @param mu mean, default 0
#' @param sigma variance, default 5
#' @param n data set size, default 30
#' @param alpha significant level, default 0.05
#'
#' @returns mu_hat - mean of dataset
#' @returns p.value - resulting pvalue from t test 
#' @export
#'
#' @examples
normal_distribution_sim = function(mu = 0, sigma = 5, n = 30, alpha = 0.05){
  
  normal_dist_vec = rnorm(n = n, mean = mu, sd = sigma)
  
  t.test(normal_dist_vec, mu = 0, conf.level = 1-alpha) |> broom::tidy() |> 
    mutate(
      mu_hat = estimate
    ) |> 
    select(mu_hat, p.value)
  
}