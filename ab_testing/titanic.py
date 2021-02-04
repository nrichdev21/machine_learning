import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from statsmodels.stats.weightstats import ztest

df = pd.read_csv('titanic_train.csv')

print(df.head())
print(df.info())

print(df[df['Survived'] == 1].head())

x1 = df[df['Survived'] == 1]['Fare'].dropna().values
x2 = df[df['Survived'] == 0]['Fare'].dropna().values

print(x1.mean(), x1.std())
print(x2.mean(), x2.std())

print(ztest(x1,x2))

sns.displot(x1, kde=True)
sns.displot(x2, kde=True)
plt.show()