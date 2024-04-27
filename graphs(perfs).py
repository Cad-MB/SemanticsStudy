import pandas as pd
import matplotlib.pyplot as plt
import os

# lecture CSVs
csv_files_typing = ['APS0/output_typing_perf.csv', 'APS1/output_typing_perf.csv', 'APS1a/output_typing_perf.csv', 'APS2/output_typing_perf.csv']
csv_files_evaluation = ['APS0/output_evaluation_perf.csv', 'APS1/output_evaluation_perf.csv', 'APS1a/output_evaluation_perf.csv', 'APS2/output_evaluation_perf.csv']

sums_typing_elapsed_time = []
sums_typing_memory_usage = []
sums_evaluation_elapsed_time = []
sums_evaluation_memory_usage = []

# version APS
aps_names = [file.split('/')[0] for file in csv_files_typing]

for file in csv_files_typing:
    file_path = os.path.join(os.getcwd(), file)
    df = pd.read_csv(file_path)
    column_sum_typing_elapsed_time = df[' elapsed time (ms)'].sum() 
    column_sum_typing_memory_usage = df[' memory usage (KB)'].sum() 
    sums_typing_elapsed_time.append(column_sum_typing_elapsed_time)
    sums_typing_memory_usage.append(column_sum_typing_memory_usage)

for file in csv_files_evaluation:
    file_path = os.path.join(os.getcwd(), file)
    df = pd.read_csv(file_path)
    column_sum_evaluation_elapsed_time = df[' elapsed time (ms)'].sum() 
    column_sum_evaluation_memory_usage = df[' memory usage (KB)'].sum() 
    sums_evaluation_elapsed_time.append(column_sum_evaluation_elapsed_time)
    sums_evaluation_memory_usage.append(column_sum_evaluation_memory_usage)

# Couleurs
typing_color = '#4CAF50'  # Green
evaluation_color = '#FF5722'  # Orange

# Traçage du temps écoulé pour la saisie et l'évaluation des CSV
plt.figure(figsize=(10, 5))
plt.bar(range(len(aps_names)), sums_typing_elapsed_time, width=0.4, align='center', label='Typing - Elapsed Time (ms)', color=typing_color)
plt.bar([i + 0.4 for i in range(len(aps_names))], sums_evaluation_elapsed_time, width=0.4, align='center', label='Evaluation - Elapsed Time (ms)', color=evaluation_color)
plt.xlabel('Versions APS')
plt.ylabel('Performance en temps')
plt.title('Performance en temps pour Typage et Evaluation (ms)')
plt.xticks(range(len(aps_names)), aps_names, rotation=45, ha='right')
plt.legend()
plt.tight_layout()

# Traçage l'utilisation de la mémoire pour la saisie et l'évaluation des CSV
plt.figure(figsize=(10, 5))
plt.bar(range(len(aps_names)), sums_typing_memory_usage, width=0.4, align='center', label='Typing - Memory Usage (KB)', color=typing_color)
plt.bar([i + 0.4 for i in range(len(aps_names))], sums_evaluation_memory_usage, width=0.4, align='center', label='Evaluation - Memory Usage (KB)', color=evaluation_color)
plt.xlabel('Versions APS')
plt.ylabel('Performance en mémoire')
plt.title('Performance en mémoire pour Typage et Evaluation (KB)')
plt.xticks(range(len(aps_names)), aps_names, rotation=45, ha='right')
plt.legend()
plt.tight_layout()

# Display both figures
plt.show()