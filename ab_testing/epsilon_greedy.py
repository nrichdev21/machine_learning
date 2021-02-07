import matplotlib.pyplot as plt
import numpy as np

N_TRIALS = 1000
EPSILON = 0.1
BANDIT_PROB = [0.3, 0.4, 0.6]

class BanditArm:
    def __init__(self,p):
        self.p = p
        self.p_estimate = 0
        self.N = 0

    def pull(self):
        # Draw a 1 with probability p
        return np.random.random() < self.p   # Returns random float between [0.0 and 1.0]

    def update(self, x):
        self.N += 1
        self.p_estimate = ((self.N - 1)*self.p_estimate + x) / self.N

def experiment():
    bandits = [BanditArm(p) for p in BANDIT_PROB]  # Creates 3 new bandit objects with p = probability

    rewards = np.zeros(N_TRIALS)
    num_trials_explored = 0
    num_trials_exploited = 0
    num_optimal = 0
    optimal_j = np.argmax(b.p for b in bandits)
    print("Optimal J: ", optimal_j)

    for i in range(N_TRIALS):
        if np.random.random() < EPSILON:
            num_trials_explored += 1
            j = np.random.randint(len(bandits))
        else:
            num_trials_exploited += 1
            j = np.argmax([b.p_estimate for b in bandits])

        if j == optimal_j:
            num_optimal += 1

        # Pull arm of bandit with the largest sample
        x = bandits[j].pull()

        # Update the rewards log
        rewards[i] = x

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

