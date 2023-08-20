import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy.spatial import distance
#### Mahalanobis Distance (MD) #######
# Load the file
df_500 = pd.read_csv('/home/fe/ayinde/Desktop/pythonProject3/ATR_FTIR_500.csv')
df_400 = pd.read_csv('/home/fe/ayinde/Desktop/pythonProject3/ATR_FTIR_400_New.csv')

# To Identify the MD for H1Bl, the close polymer is PEC, HDPE, PET and ABS
# Extracting the spectral data for the known samples
known_samples = {
    #'ABS': df_500["O3EC"],
    'PEC': df_500["O5.1"],
    'HDPE': df_500["SP3.1"],
    'PP':  df_500["SP1.1TT"],
    'PVC': df_500["O8"],
    'PET': df_500["SP4"]
}

# Extracting the spectral data for the unknown samples
unknown_samples = {
    'H1Bl': df_400["H1Bl Hair"]
}

# Calculate the mean and covariance matrix for known samples
known_samples_matrix = np.array(list(known_samples.values())).T
mean_vector = np.mean(known_samples_matrix, axis=1)
cov_matrix = np.cov(known_samples_matrix)
inv_cov_matrix = np.linalg.inv(cov_matrix)

# Find the closest known sample for each unknown sample
closest_known_samples = {}
for name, unknown_sample in unknown_samples.items():
    min_distance = float('inf')
    min_sample = None
    for known_sample_name, known_sample in known_samples.items():
        dist = distance.mahalanobis(unknown_sample, known_sample, inv_cov_matrix)
        if dist < min_distance:
            min_distance = dist
            min_sample = known_sample_name
    closest_known_samples[name] = min_sample

# Plot known samples
for name, sample in known_samples.items():
    plt.scatter(sample[0], sample[1], label=name)

# Plot unknown samples and connect them to the closest known sample
for name, sample in unknown_samples.items():
    plt.scatter(sample[0], sample[1], marker='*', s=200, edgecolors='k', label=name)
    plt.plot([sample[0], known_samples[closest_known_samples[name]][0]], [sample[1], known_samples[closest_known_samples[name]][1]], linestyle='--', color='gray')

plt.xlabel('Variable 1')
plt.ylabel('Variable 2')
plt.title('Mahalanobis Distance Plot For H1Bl')
plt.legend()
plt.show()