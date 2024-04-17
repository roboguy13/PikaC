import numpy as np
import matplotlib.pyplot as plt

import numpy as np
import numpy as np
import matplotlib.pyplot as plt

import sys

options = ['--no-show']

def makePlot(file_name, test_case_names, running_times, tool_names):
    fig, ax = plt.subplots()
    width = 0.15
    x = np.arange(len(test_case_names))

    for i in range(len(tool_names)):
        ax.bar(x + i * width, running_times[i], width, label=tool_names[i])

# Adding labels and title
    ax.set_xlabel('Test cases')
    ax.set_ylabel('Execution time (ms)')
# ax.set_title('Performance Comparison for Different Test Cases')
    ax.set_xticks(x + width)
    ax.set_xticklabels(test_case_names)
    ax.legend()

# Displaying the plot
    plt.savefig(file_name + 'eps', format='eps')

    if '--no-show' not in sys.argv:
        plt.show()
#
# test_cases = ['tree_map_plus1', 'multilist_map_sum']
# running_times = np.array([[363.2, 285], [54.837, 52.481], [54.450, 171.8], [45.902, 27.981]])
#
# test_cases = ['append', 'sll_map_plus1', 'maximum', 'filterLt20000', 'sum']
# running_times = np.array([[26.060, 29.65, 8.101, 9.722, 4.412], [8.398, 11.56, 6.364, 5.467, 6.049], [0.754, 2.083, 1.914, 0.562, 1.659], [5.300, 6.189, 1.712, 2.913, 1.708]])
#
# tool_names = ['GHC w/o -O2', 'GCC w/o -O3', 'GHC w/ -O2', 'GCC w/ -O3']
# # Creating a bar plot
