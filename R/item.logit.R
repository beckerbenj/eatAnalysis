
####
#############################################################################
#' Simulates 1PL or 2PL responses
#'
#' Simulates responses following a 1PL or 2PL model given item parameters assuming normally distributed person parameters.
#'
#'@param z A vector of item difficulties.
#'@param slope A vector of item discrimination parameters. Default is 1 and equals a 1PL model.
#'@param thr A vector of item threshold parameters.
#'
#'@return A list with two matrices. First matrix includes the responses, seconds matrix includes the correct response probabilities.
#'
#'@author Ulf Kroehne
#'
#'@examples
#' z <- rnorm(30, 0, 1)
#' d <- item.logit(z = z, thr = 0)
#'
#'@export
item.logit <- function(z,slope=1,thr)  {
  n <- length(z)
  k <- length(thr)
  m.zts <- matrix(rep(z*slope,k),ncol=k)
  m.thr <- matrix(rep(thr*slope,n),ncol=k,byrow=T)
  cum.prob  <- 1.0 / (1.0 + exp(-(m.zts - m.thr)))
  u <- matrix(rep(stats::runif(n),k),ncol=k)
  x <- apply(u < cum.prob,1,sum)
  des <- cbind(-1*diag(k),rep(0,k))+cbind(rep(0,k),1*diag(k))
  prob <- cum.prob %*% des
  prob[,1] <- 1 + prob[,1]
  colnames(prob) <- c(paste("p",1:(k+1),sep=""))

  list(x=x,psc=prob)
}
