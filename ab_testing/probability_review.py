import numpy as np
from scipy.stats import norm
import seaborn as sns
import matplotlib.pyplot as plt

sns.set_style('whitegrid')

# SAT Score Distributions
mu = 1051
sd = 221
s = 1000
bins = np.sqrt(1000).astype(int)

# Generate Samples
X = norm.rvs(loc=mu, size=s, scale=sd)

sns.distplot(X, bins=bins, )

# Maximum Likelihood mean
print(X.mean())

# Maximum Likelihood variance
print(X.var())
print(((X - X.mean())**2).mean())

# Maximum Likelihood Standard Deviation
print(np.sqrt(X.var()))
print(np.sqrt(((X - X.mean())**2).mean()))

# Unbiased Estimates
print(X.var(ddof=1))
print(np.sqrt(X.var(ddof=1)))

# What score is in the 95th percentile?
x = norm.ppf(0.95, loc=mu, scale=sd)
print(x)

test_score = 1350

# What percentile is a 1400 score?
x = norm.cdf(test_score, loc=mu, scale=sd)
print(x)

x = norm.ppf(x, loc=mu, scale=sd)
print(x)

plt.show()