import matplotlib.pyplot as plt
import numpy as np

N_TRIALS = 1000
EPSILON = 0.1
BANDIT_PROB = [0.3, 0.4, 0.6]

class BanditArm:
    def __init__(self,p):
        self.m = p
        self.m_estimate = 0
        self.N = 0

    def pull(self):
        # Draw a 1 with probability p
        return np.random.random() < self.m   # Returns random float between [0.0 and 1.0]

    def update(self, x):
        self.N += 1
        self.m_estimate = (1 - 1.0/self.N) * self.m_estimate + 1.0/self.N * x

def experiment(m1, m2, m3, eps, N):
    bandits = [BanditArm(m1), BanditArm(m2), BanditArm(m3)]

    means = np.array([m1, m2, m3])
    true_best = np.argmax(means)
    count_suboptimal = 0

    data = np.empty(N)

    for i in range(N):
        p = np.random.rand()
        if p < eps:
            num_trials_explored += 1
            j = np.random.choice(len(bandits))  # Generates a random sample from a given 1-D array
        else:
            num_trials_exploited += 1
            j = np.argmax([b.m_estimate for b in bandits])

        x = bandits[j].pull()
        bandits[j].update(x)
        if j != true_best:
            count_suboptimal += 1


        # Update the rewards log
        data[i] = x

        # Update the distribution for the bandit whose arm we just pulled
        bandits[j].update(x)

  # print mean estimates for each bandit
    for b in bandits:
        print("mean estimate:", b.p_estimate)

  # print total reward
    print("total reward earned:", rewards.sum())
    print("overall win rate:", rewards.sum() / N_TRIALS)
    print("num_times_explored:", num_trials_explored)
    print("num_times_exploited:", num_trials_exploited)
    print("num times selected optimal bandit:", num_optimal)

  # plot the results
    cumulative_rewards = np.cumsum(rewards)   # Return the cumulative sum of the elements along a given axis.
    win_rates = cumulative_rewards / (np.arange(N_TRIALS) + 1) # Return evenly spaced values within a given interval.
    plt.plot(win_rates)
    plt.plot(np.ones(N_TRIALS)*np.max(BANDIT_PROB))
    plt.show()

if __name__ == "__main__":
    experiment()

