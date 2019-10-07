#·
# Utility to plot x-y data from CSV file.
#
# Assumes that x-values are in first column
# Assumes curve titles are in first row
# Plots a curve for each column in addtion to·
# the x-data
#
# Usage:
#    python plotxy.py <CSV input file name> <PNG output file name>
#
import sys
import matplotlib
matplotlib.rcParams['font.size'] = 8
matplotlib.use("Agg")
from matplotlib import pyplot as plt
import numpy as np
# Read CSV data into numpy array
data = np.genfromtxt(sys.argv[1], delimiter=",")
# x-values  are in the first column
xval = data[:,0]
# Get the number of rowa
nrow = xval.shape[0]
# Get the number of curves to plot        ncurve = data.shape[1] - 1
fig = plt.figure(1)
ax = plt.subplot(1, 1, 1)
# Loop over curves adding them to the plot
for i in range(ncurve):
   title = "Column " + str(i)
   ax.plot(xval[0:], data[0:,i+1], marker="+", label=title)
ax.legend(loc='best', fancybox=True, framealpha=0.5)
fig.savefig(sys.argv[2])
