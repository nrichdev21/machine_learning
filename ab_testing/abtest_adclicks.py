import pandas as pd
import numpy as np
from scipy.stats import norm
import seaborn as sns
import matplotlib.pyplot as plt
from statsmodels.stats.weightstats import ztest

df = pd.read_csv('advertisement_clicks.csv')

print(df.head())
print(df.info())

x1 = df[df['advertisement_id'] == 'A']['action']
x2 = df[df['advertisement_id'] == 'B']['action']

print(x1.mean(), x1.std())
print(x2.mean(), x2.std())

print(ztest(x1,x2))

N1 = len(x1)
N2 = len(x2)
mu_zero = 0
mu1_hat = x1.mean()
mu2_hat = x2.mean()
dmu_hat = mu2_hat-mu1_hat
s2_hat1 = x1.var(ddof=1)
s2_hat2 = x2.var(ddof=1)
s_hat = np.sqrt(s2_hat1/N1 + s2_hat2/N2)
z = (dmu_hat - mu_zero) / s_hat
p_right = 1 -norm.cdf(np.abs(z))
p_left = norm.cdf(-np.abs(z))
p = p_left + p_right
print(z, p)   # 2 sided test so sign mismatch not an issue because normal distribution is symetrical

# Reject the null hypothesis at the 0.05 significance value.