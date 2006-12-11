# experiment.py

from linearsearch import *
from bubblesort import *
from randdata import *

def average(lst):
    return sum(lst) / float(len(lst))

def linear_search_experiment():
    # search 100 times for item known to be in the list
    result = []
    for i in range(100):
        lst = random_range(10000)
        index, num_comps = linear_search_num_comps(lst, 5)
        result.append(num_comps)
    print 'Average number of comparisons: ' + str(average(result))

def bubblesort_experiment():
    result = []
    for i in range(11):
        lst = random_range(10 * (i + 1))
        num_comps = bubblesort_num_comps(lst)
        result.append([len(lst), num_comps])
    print result
    



        
