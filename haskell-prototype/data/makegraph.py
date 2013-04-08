#!/usr/bin/env python
import numpy as np
import matplotlib.pyplot as pyplot
from matplotlib.mlab import csv2rec

def importdata(filename):
    """Import CSV data into a list"""
    results = {}
    data = csv2rec(filename)
    
    # print data

    StrategiesData = []
    LVarPureData = []

    # Disregard everything but work, cores, and med running time
    for row in data:
        if row[0] == "BFS_Strategies":
            StrategiesData.append([row[1], # work
                                   row[2], # cores
                                   row[4]]) # med running time
        elif row[0] == "BFS_LVar":
            LVarPureData.append([row[1], # work
                                 row[2], # cores
                                 row[4]]) # med running time
        else:
            pass

    return [StrategiesData, LVarPureData]

def filterdata(data, numcores):
    StrategiesData = data[0]
    LVarPureData = data[1]

    StrategiesTimes = []
    LVarPureTimes = []
    for row in StrategiesData:
        if (row[1] == numcores):
            StrategiesTimes.append(row[2])
    for row in LVarPureData:
        if (row[1] == numcores):
            LVarPureTimes.append(row[2])

    return [StrategiesTimes, LVarPureTimes]

def plotdata(times, numcores):

    StrategiesTimes = times[0]
    LVarPureTimes = times[1]

    N = 6 # Number of different amounts of work (1, 2, 4, 8, 16, 32)

    ind = np.arange(N)  # the x locations for the groups
    width = 0.45        # the width of the bars

    pyplot.subplot(("22%d" % numcores))
    rects1 = pyplot.bar(ind, StrategiesTimes, width,
                        color='b')

    rects2 = pyplot.bar(ind+width, LVarPureTimes, width,
                        color='g')

    autolabel(rects1)
    autolabel(rects2)

    pyplot.xlabel('Work done by analyze (us)')
    pyplot.ylabel('Running time (s)')

    pyplot.title('Strategies vs. LVarPure, %d core%s'
                 % (numcores, "" if numcores == 1 else "s"))
    pyplot.xticks(ind+width, ('1', '2', '4', '8', '16', '32'))

    pyplot.legend((rects1[0], rects2[0]), ('Strategies', 'LVarPure'),
                   loc="upper left")

def autolabel(rects):
    # Put some text labels at the top of columns, truncated to two
    # decimal places (TODO: actually round?)

    for rect in rects:
        height = rect.get_height()
        pyplot.text(rect.get_x()+rect.get_width()/2., 1.05*height, '%.2f'%height,
                 ha='center', va='bottom')

if __name__ == '__main__':

    filename = '%s.csv' % "BFS_benchmark_data"
    data = importdata(filename)
    
    pyplot.figure(1)
    plotdata(filterdata(data,1), 1)
    plotdata(filterdata(data,2), 2)
    plotdata(filterdata(data,3), 3)
    plotdata(filterdata(data,4), 4)

    pyplot.savefig('%s.png' % "BFS_benchmark_data")
    pyplot.show()
