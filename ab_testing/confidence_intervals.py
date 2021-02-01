import numpy as np
from scipy.stats import norm, t
import seaborn as sns
import matplotlib.pyplot as plt


N = 1000
mu = 5
sigma = 2

X = np.random.randn(N)*sigma + mu
print(X)

#sns.distplot(X, bins=100)
#plt.show()

# Z Confidence Interval
mu_hat = np.mean(X)
sigma_hat = np.std(X, ddof=1)

z_left = norm.ppf(0.025)
z_right = norm.ppf(0.975)

print(f"Z Left: {z_left}")
print(f"Z Right: {z_right}")

lower = mu_hat + z_left * sigma_hat / np.sqrt(N)
upper = mu_hat + z_right * sigma_hat / np.sqrt(N)
print(lower, mu_hat, upper)

# T-Confidence Interval   - Requires ddof parameter
mu_hat = np.mean(X)
sigma_hat = np.std(X, ddof=1)

t_left = t.ppf(0.025, df = N-1)
t_right = t.ppf(0.975, df= N-1)

print(t_left, t_right)

lower = mu_hat + t_left * sigma_hat / np.sqrt(N-1)
upper = mu_hat + t_right * sigma_hat / np.sqrt(N-1)

print(lower, mu_hat, upper)


def experiment():
    X = np.random.randn(N)*sigma + mu
    mu_hat = np.mean(X)
    sigma_hat = np.std(X, ddof=1)
    t_left = t.ppf(0.025, df=N - 1)
    t_right = t.ppf(0.975, df=N - 1)
    lower = mu_hat + t_left * sigma_hat / np.sqrt(N - 1)
    upper = mu_hat + t_right * sigma_hat / np.sqrt(N - 1)
    return mu > lower and mu < upper

def multi_experiment(M):
    results = [experiment() for _ in range(M)]
    return np.mean(results)

print(multi_experiment(10000))

