import numpy as np
from random import gauss
from scipy.stats import invgamma
import math
import timeit


def candy(data, iteration, alpha, beta, mu0, kappa, mu_init, sigma2_init):
    n = len(data)
    ybar = np.mean(data)
    s2 = np.var(data)
    mu = mu_init
    sigma2 = sigma2_init
    samples = []
    np.random.seed(17)
    for i in range(iteration):
        alphaShape = alpha + 1
        betaRate = beta + (kappa/2)*(mu-mu0)**2 + (n/2)*(ybar-mu)**2
        sigma2 = 1 / np.random.gamma(shape=alphaShape, scale=1/betaRate, size=1)
        mean = (kappa * mu0 + n * ybar) / (kappa + n)
        sd = math.sqrt(sigma2 / (kappa + n))
        mu = gauss(mu=mean, sigma=sd)
        samples.append([mu, sigma2])
    return samples


def main():
    data = [46, 58, 40, 47, 47, 53, 43, 48, 50, 55, 49, 50, 52, 56, 49, 54, 51, 50, 52, 50]
    start_time = timeit.default_timer()
    chain1 = candy(data, 10000, 38, 444, 51, 10, 100, 20)
    chain2 = candy(data, 10000, 38, 444, 51, 10, 1, 1)
    chain3 = candy(data, 10000, 38, 444, 51, 10, 150, 10)
    end_time = timeit.default_timer()
    elapsed = end_time - start_time
    del chain1[:1000]
    del chain2[:1000]
    del chain3[:1000]
    meanMu = (np.mean([item[0] for item in chain1]) + np.mean([item[0] for item in chain2]) +
              np.mean([item[0] for item in chain3])) / 3
    sdMu = (np.std([item[0] for item in chain1]) + np.std([item[0] for item in chain2]) +
            np.std([item[0] for item in chain3])) / 3
    medianMu = (np.median([item[0] for item in chain1]) + np.median([item[0] for item in chain2]) +
                np.median([item[0] for item in chain3])) / 3
    print(meanMu, sdMu, medianMu, elapsed)
    meanSigma2 = (np.mean([item[1] for item in chain1]) + np.mean([item[1] for item in chain2]) +
                  np.mean([item[1] for item in chain3])) / 3
    sdSigma2 = (np.std([item[1] for item in chain1]) + np.std([item[1] for item in chain2]) +
                np.std([item[1] for item in chain3])) / 3
    medianSigma2 = (np.median([item[1] for item in chain1]) + np.median([item[1] for item in chain2]) +
                    np.median([item[1] for item in chain3])) / 3
    print(meanSigma2, sdSigma2, medianSigma2, elapsed)


main()





