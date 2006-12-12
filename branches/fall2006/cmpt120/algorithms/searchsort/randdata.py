from random import *

def random_range(n):
    lst = range(n)
    shuffle(lst)
    return lst

def shuffle1(lst):
    """ Neat idea, but too slow.
    """
    rnd_lst = [(random.random(), item) for item in lst]
    rnd_lst.sort()
    for i in xrange(len(lst)):
	lst[i] = rnd_lst[i][1]
