candy <- function(data, iteration, alpha, beta, mu0, kappa, mu_init, sigma2_init) {
  n <- length(data)
  ybar <- mean(data)
  s2 <- var(data)
  mu <- mu_init
  samplesMu <- array(NA, iteration)
  samplesSigma2 <- array(NA, iteration)
  set.seed(17)
  for (i in 1:iteration) {
    alphaShape <- alpha + 1
    betaRate <- beta + (kappa/2)*(mu-mu0)^2+(n/2)*(ybar-mu)^2
    sigma2 <- 1 / rgamma(1, shape = alphaShape, rate = betaRate)
    mean <- (kappa * mu0 + n * ybar) / (kappa + n)
    sd <- sqrt(sigma2 / (kappa + n))
    mu <- rnorm(1, mean, sd)
    samplesMu[i] <- mu
    samplesSigma2[i] <- sigma2
  }
  results <- matrix(c(samplesMu, samplesSigma2), nrow = iteration)
  return(results)
}

data <- c(46, 58, 40, 47, 47, 53, 43, 48, 50, 55, 49, 50, 52, 56, 49, 54, 51, 50, 52, 50)
start_time <- Sys.time()
chain1 <- candy(data, 10000, 38, 444, 51, 10, 100, 20)
chain2 <- candy(data, 10000, 38, 444, 51, 10, 1, 1)
chain3 <- candy(data, 10000, 38, 444, 51, 10, 150, 10)
end_time <- Sys.time()
elapsed <- end_time - start_time
elapsed
chain1 <- chain1[-c(1:1000), ]
chain2 <- chain2[-c(1:1000), ]
chain3 <- chain3[-c(1:1000), ]
(mean(chain1[,1]) + mean(chain2[,1]) + mean(chain3[,1])) / 3
(sd(chain1[,1]) + sd(chain2[,1]) + sd(chain3[,1])) / 3
(median(chain1[,1]) + median(chain2[,1]) + median(chain3[,1])) / 3
(mean(chain1[,2]) + mean(chain2[,2]) + mean(chain3[,2])) / 3
(sd(chain1[,2]) + sd(chain2[,2]) + sd(chain3[,2])) / 3
(median(chain1[,2]) + median(chain2[,2]) + median(chain3[,2])) / 3
