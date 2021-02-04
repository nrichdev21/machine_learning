import numpy as np
from scipy.stats import norm
from statsmodels.stats.weightstats import ztest

N = 100
mu = 0.2
sigma = 1
x = np.random.randn(N) * sigma + mu

# 2 Sided test
print(ztest(x))

mu_hat = x.mean()
mu_zero = 0
sigma_hat = x.std(ddof=1)
z = (mu_hat - mu_zero) / (sigma_hat / np.sqrt(N))
p_right = 1 -norm.cdf(np.abs(z))
p_left = norm.cdf(-np.abs(z))
p = p_left + p_right

print (z, p)

# 1 Sided Test
print(ztest(x, alternative='larger'))

mu_hat = x.mean()
mu_zero = 0
sigma_hat = x.std(ddof=1)
z = (mu_hat - mu_zero) / (sigma_hat / np.sqrt(N))
p = 1 - norm.cdf(z)

print(z,p)

# Use true mean of population for testing - 2 Sided
mu_zero = mu
print(ztest(x, value=mu_zero))  # Fail to reject null hypothesis that H0: mu = 0.2

mu_hat = x.mean()
mu_zero = mu
sigma_hat = x.std(ddof=1)
z = (mu_hat - mu_zero) / (sigma_hat / np.sqrt(N))
p_right = 1 -norm.cdf(np.abs(z))
p_left = norm.cdf(-np.abs(z))
p = p_left + p_right
print(z,p)

# 2 Sample Test
N0 = 100
mu0 = 0.2
sigma0 = 1
x0 = np.random.randn(N) * sigma + mu0

N1 = 100
mu1 = 0.5
sigma1 = 1
x1 = np.random.randn(N) * sigma + mu0

print(ztest(x0, x1))

# 2 Sample Test with known variances and independant samples
mu_zero = 0
mu0_hat = x0.mean()
mu1_hat = x1.mean()
dmu_hat = mu1_hat-mu0_hat
s2_hat0 = x0.var(ddof=1)
s2_hat1 = x1.var(ddof=1)
s_hat = np.sqrt(s2_hat0/N0 + s2_hat1/N1)
z = (dmu_hat - mu_zero) / s_hat
p_right = 1 -norm.cdf(np.abs(z))
p_left = norm.cdf(-np.abs(z))
p = p_left + p_right
print(z, p)   # 2 sided test so sign mismatch not an issue because normal distribution is symetrical

# Show that we will reject the null hypothesis when it is true (false alarm/type error error) 5 % of the time
num_test = 10000
results = np.zeros(num_test)
for i in range(num_test):
    x1 = np.random.randn(100)
    x2 = np.random.randn(100)
    z, p = ztest(x1, x2)
    results[i] = (p < 0.05)
print(results.mean())