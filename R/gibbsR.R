#' @title A Gibbs sampler using R
#' @description A Gibbs sampler using R
#' @param N the number of samples
#' @param thin the number of between-sample random numbers
#' @return a random sample of size \code{n}
#' @importFrom stats rbinom
#' @importFrom stats rbeta
#' @examples
#' \dontrun{
#' rnR <- gibbsR(100,10)
#' par(mfrow=c(2,1));
#' plot(rnR[,1],type='l')
#' plot(rnR[,2],type='l')
#' }
#' @export
gibbsR <- function(N, thin){
  burn <- thin #burn-in length
  X <- matrix(0, N, 2) #the chain, a bivariate sample
  mu1 <- 2
  mu2 <- 0.5
  a <- 1
  b <- 2
  n <- 10
  ###### generate the chain #####
  X[1, ] <- c(mu1, mu2) #initialize
  for (i in 2:N) {
    x2 <- X[i-1, 2]
    X[i, 1] <- rbinom(1, n, x2)
    x1 <- X[i, 1]
    X[i, 2] <- rbeta(1, x1+a, n-x1+b)
  }
  b <- burn + 1
  index <- b: N
  x <- X[index, ]
  return(x)
}
