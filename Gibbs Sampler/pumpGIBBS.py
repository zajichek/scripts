import numpy
import scipy
from array import *
import math
import timeit
from scipy.stats import gamma

def get_alpha(a,b,t,l):

    def f(alpha,beta,theta,lamb):
        n = len(theta)
        val = (beta**(n*alpha)/scipy.special.gamma(alpha)**n)*math.exp(-1*lamb*alpha)*numpy.prod(numpy.power(t,alpha))
        return val

    def cur_value(a_old,beta,theta,lamb):
        a_new = gamma.rvs(a=a_old,scale=1)
        t1 = gamma.pdf(a_old,a=a_new,scale=1)/gamma.pdf(a_new,a=a_old,scale=1)
        t2 = f(a_new,beta,theta,lamb)/f(a_old,beta,theta,lamb)
        if t1*t2 < 1:
            p = t1*t2
        else:
            p = 1
        s = numpy.random.binomial(1,p,1)
        return s*a_new + (1-s)*a_old

    mh = numpy.zeros(1000)
    mh[0] = .1
    for i in range(1,1000):
        mh[i] = cur_value(mh[i-1],b,t,l)

    return numpy.random.choice(mh[1:1000],1)[0]


def pump(x, t, alpha_init, beta_init, iteration, l_1, l_2):
    n = len(x)
    alpha = numpy.zeros(iteration)
    cur_alpha = alpha_init
    beta = numpy.zeros(iteration)
    cur_beta = beta_init
    theta = numpy.zeros((n,iteration))
    cur_theta = numpy.zeros(n)
    for j in range(n):
        cur_theta[j] = gamma.rvs(a=x[j]+cur_alpha, scale = 1/(t[j]+cur_beta))
    for i in range(iteration):
        print(i)
        cur_beta = gamma.rvs(a=n*cur_alpha+1,scale=1/(l_2 + sum(cur_theta)))
        beta[i] = cur_beta
        cur_alpha = get_alpha(cur_alpha, cur_beta, cur_theta, l_1)
        alpha[i] = cur_alpha
        for j in range(n):
            cur_theta[j] = gamma.rvs(a=x[j]+cur_alpha, scale = 1/(t[j]+cur_beta))
            theta[j,i] = cur_theta[j]

    results = [alpha,beta,theta]
    return results

x = numpy.array([5,1,5,14,3,19,1,1,4,22])
t = numpy.array([94.3,15.7,62.9,126,5.24,31.4,1.05,1.05,2.1,10.5])
l_1 = 1
l_2 = 1
start_time = timeit.default_timer()
chain1 = pump(x, t, 10, 10, 3000, l_1, l_2)
chain2 = pump(x, t, 5, 20, 3000, l_1, l_2)
chain3 = pump(x, t, 30, 1, 3000, l_1, l_2)
end_time = timeit.default_timer()
elapsed = end_time - start_time
print(elapsed)

#burn-in

print((numpy.mean(chain1[0][1000:3000]) + numpy.mean(chain2[0][1000:3000]) + numpy.mean(chain3[0][1000:3000])) / 3)
print((numpy.mean(chain1[1][1000:3000]) + numpy.mean(chain2[1][1000:3000]) + numpy.mean(chain3[1][1000:3000])) / 3)
print((numpy.mean(chain1[2][0][1000:3000]) + numpy.mean(chain2[2][0][1000:3000]) + numpy.mean(chain3[2][0][1000:3000])) / 3)
print((numpy.mean(chain1[2][1][1000:3000]) + numpy.mean(chain2[2][1][1000:3000]) + numpy.mean(chain3[2][1][1000:3000])) / 3)
print((numpy.mean(chain1[2][2][1000:3000]) + numpy.mean(chain2[2][2][1000:3000]) + numpy.mean(chain3[2][2][1000:3000])) / 3)
print((numpy.mean(chain1[2][3][1000:3000]) + numpy.mean(chain2[2][3][1000:3000]) + numpy.mean(chain3[2][3][1000:3000])) / 3)
print((numpy.mean(chain1[2][4][1000:3000]) + numpy.mean(chain2[2][4][1000:3000]) + numpy.mean(chain3[2][4][1000:3000])) / 3)
print((numpy.mean(chain1[2][5][1000:3000]) + numpy.mean(chain2[2][5][1000:3000]) + numpy.mean(chain3[2][5][1000:3000])) / 3)
print((numpy.mean(chain1[2][6][1000:3000]) + numpy.mean(chain2[2][6][1000:3000]) + numpy.mean(chain3[2][6][1000:3000])) / 3)
print((numpy.mean(chain1[2][7][1000:3000]) + numpy.mean(chain2[2][7][1000:3000]) + numpy.mean(chain3[2][7][1000:3000])) / 3)
print((numpy.mean(chain1[2][8][1000:3000]) + numpy.mean(chain2[2][8][1000:3000]) + numpy.mean(chain3[2][8][1000:3000])) / 3)
print((numpy.mean(chain1[2][9][1000:3000]) + numpy.mean(chain2[2][9][1000:3000]) + numpy.mean(chain3[2][9][1000:3000])) / 3)

print((numpy.std(chain1[0][1000:3000]) + numpy.std(chain2[0][1000:3000]) + numpy.std(chain3[0][1000:3000])) / 3)
print((numpy.std(chain1[1][1000:3000]) + numpy.std(chain2[1][1000:3000]) + numpy.std(chain3[1][1000:3000])) / 3)
print((numpy.std(chain1[2][0][1000:3000]) + numpy.std(chain2[2][0][1000:3000]) + numpy.std(chain3[2][0][1000:3000])) / 3)
print((numpy.std(chain1[2][1][1000:3000]) + numpy.std(chain2[2][1][1000:3000]) + numpy.std(chain3[2][1][1000:3000])) / 3)
print((numpy.std(chain1[2][2][1000:3000]) + numpy.std(chain2[2][2][1000:3000]) + numpy.std(chain3[2][2][1000:3000])) / 3)
print((numpy.std(chain1[2][3][1000:3000]) + numpy.std(chain2[2][3][1000:3000]) + numpy.std(chain3[2][3][1000:3000])) / 3)
print((numpy.std(chain1[2][4][1000:3000]) + numpy.std(chain2[2][4][1000:3000]) + numpy.std(chain3[2][4][1000:3000])) / 3)
print((numpy.std(chain1[2][5][1000:3000]) + numpy.std(chain2[2][5][1000:3000]) + numpy.std(chain3[2][5][1000:3000])) / 3)
print((numpy.std(chain1[2][6][1000:3000]) + numpy.std(chain2[2][6][1000:3000]) + numpy.std(chain3[2][6][1000:3000])) / 3)
print((numpy.std(chain1[2][7][1000:3000]) + numpy.std(chain2[2][7][1000:3000]) + numpy.std(chain3[2][7][1000:3000])) / 3)
print((numpy.std(chain1[2][8][1000:3000]) + numpy.std(chain2[2][8][1000:3000]) + numpy.std(chain3[2][8][1000:3000])) / 3)
print((numpy.std(chain1[2][9][1000:3000]) + numpy.std(chain2[2][9][1000:3000]) + numpy.std(chain3[2][9][1000:3000])) / 3)

print((numpy.median(chain1[0][1000:3000]) + numpy.median(chain2[0][1000:3000]) + numpy.median(chain3[0][1000:3000])) / 3)
print((numpy.median(chain1[1][1000:3000]) + numpy.median(chain2[1][1000:3000]) + numpy.median(chain3[1][1000:3000])) / 3)
print((numpy.median(chain1[2][0][1000:3000]) + numpy.median(chain2[2][0][1000:3000]) + numpy.median(chain3[2][0][1000:3000])) / 3)
print((numpy.median(chain1[2][1][1000:3000]) + numpy.median(chain2[2][1][1000:3000]) + numpy.median(chain3[2][1][1000:3000])) / 3)
print((numpy.median(chain1[2][2][1000:3000]) + numpy.median(chain2[2][2][1000:3000]) + numpy.median(chain3[2][2][1000:3000])) / 3)
print((numpy.median(chain1[2][3][1000:3000]) + numpy.median(chain2[2][3][1000:3000]) + numpy.median(chain3[2][3][1000:3000])) / 3)
print((numpy.median(chain1[2][4][1000:3000]) + numpy.median(chain2[2][4][1000:3000]) + numpy.median(chain3[2][4][1000:3000])) / 3)
print((numpy.median(chain1[2][5][1000:3000]) + numpy.median(chain2[2][5][1000:3000]) + numpy.median(chain3[2][5][1000:3000])) / 3)
print((numpy.median(chain1[2][6][1000:3000]) + numpy.median(chain2[2][6][1000:3000]) + numpy.median(chain3[2][6][1000:3000])) / 3)
print((numpy.median(chain1[2][7][1000:3000]) + numpy.median(chain2[2][7][1000:3000]) + numpy.median(chain3[2][7][1000:3000])) / 3)
print((numpy.median(chain1[2][8][1000:3000]) + numpy.median(chain2[2][8][1000:3000]) + numpy.median(chain3[2][8][1000:3000])) / 3)
print((numpy.median(chain1[2][9][1000:3000]) + numpy.median(chain2[2][9][1000:3000]) + numpy.median(chain3[2][9][1000:3000])) / 3)